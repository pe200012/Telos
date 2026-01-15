{-# LANGUAGE RecursiveDo #-}

module Telos.TUI.FRP
  ( FRPEvent(..)
  , FRPOutput(..)
  , ScrollCmd(..)
  , EditorCmd(..)
  , buildFRPNetwork
  ) where

import qualified Graphics.Vty               as Vty

import           Reactive.Banana
import           Reactive.Banana.Frameworks

import           Relude

import           Telos.TUI.Chat             ( ChatMessage(..), FocusPanel(..)
                                            , MessageSender(..), Mode(..) )

-- | Raw VTY events coming from Brick
data FRPEvent
  = FRPVtyEvent Vty.Event
  | FRPTick  -- For future use (animations, polling)
  deriving ( Eq, Show )

-- | Scroll commands for viewport
data ScrollCmd
  = ScrollUp
  | ScrollDown
  | ScrollPageUp
  | ScrollPageDown
  | ScrollToEnd
  deriving ( Eq, Show )

-- | Editor commands
data EditorCmd
  = EditorInsertNewline
  | EditorClear
  | EditorForward Vty.Event
  | EditorMoveCursorEnd
  deriving ( Eq, Show )

-- | Output from FRP network - commands to execute
data FRPOutput = FRPOutput
  { outMode        :: Mode
  , outFocus       :: FocusPanel
  , outHistory     :: [ChatMessage]
  , outEditorCmd   :: Maybe EditorCmd
  , outScrollCmd   :: Maybe ScrollCmd
  , outShouldHalt  :: Bool
  }
  deriving ( Eq, Show )

-- | Build FRP event network
-- Returns: (EventNetwork, Handler for output)
buildFRPNetwork
  :: AddHandler FRPEvent
  -> Handler FRPOutput
  -> IO EventNetwork
buildFRPNetwork inputHandler outputHandler = compile $ mdo
  -- Input event stream
  eInput <- fromAddHandler inputHandler

  -- Extract VTY events
  let eVty :: Event Vty.Event
      eVty = filterJust $ fmap getVtyEvent eInput

      getVtyEvent (FRPVtyEvent e) = Just e
      getVtyEvent _               = Nothing

  -- Classify key events
  let eKeyPress :: Event (Vty.Key, [Vty.Modifier])
      eKeyPress = filterJust $ fmap extractKey eVty

      extractKey (Vty.EvKey k mods) = Just (k, mods)
      extractKey _                  = Nothing

  -- Specific key events
  let eQuit       = () <$ filterE (\(k, m) -> k == Vty.KChar 'd' && m == [Vty.MCtrl]) eKeyPress
      eEnter      = () <$ filterE (\(k, m) -> k == Vty.KEnter && null m) eKeyPress
      eShiftEnter = () <$ filterE (\(k, m) -> k == Vty.KEnter && m == [Vty.MShift]) eKeyPress
      eEsc        = () <$ filterE (\(k, m) -> k == Vty.KEsc && null m) eKeyPress
      eArrowUp    = () <$ filterE (\(k, m) -> k == Vty.KUp && null m) eKeyPress
      eArrowDown  = () <$ filterE (\(k, m) -> k == Vty.KDown && null m) eKeyPress
      ePageUp     = () <$ filterE (\(k, m) -> k == Vty.KPageUp && null m) eKeyPress
      ePageDown   = () <$ filterE (\(k, m) -> k == Vty.KPageDown && null m) eKeyPress

      -- Other keys (for editor forwarding)
      isSpecialKey (k, _) = k `elem`
        [ Vty.KEnter, Vty.KEsc, Vty.KUp, Vty.KDown, Vty.KPageUp, Vty.KPageDown ]
      isQuitKey (k, m) = k == Vty.KChar 'd' && m == [Vty.MCtrl]
      eOtherKey = filterE (\km -> not (isSpecialKey km) && not (isQuitKey km)) eKeyPress

  -- ══════════════════════════════════════════════════════════════════
  -- MODE BEHAVIOR
  -- ══════════════════════════════════════════════════════════════════

  -- Mode transitions:
  -- NormalMode + Enter -> InsertMode
  -- InsertMode + Esc -> NormalMode
  let eModeChange :: Event (Mode -> Mode)
      eModeChange = unions
        [ (\mode -> if mode == NormalMode then InsertMode else mode) <$ eEnter
        , const NormalMode <$ eEsc
        ]

  bMode <- accumB NormalMode eModeChange

  -- ══════════════════════════════════════════════════════════════════
  -- FOCUS BEHAVIOR
  -- ══════════════════════════════════════════════════════════════════

  -- Focus transitions (only in NormalMode):
  -- NormalMode + Up -> HistoryPanel
  -- NormalMode + Down -> InputPanel
  let eFocusChange :: Event (FocusPanel -> FocusPanel)
      eFocusChange = unions
        [ (\_ -> HistoryPanel) <$ whenE ((== NormalMode) <$> bMode) eArrowUp
        , (\_ -> InputPanel)   <$ whenE ((== NormalMode) <$> bMode) eArrowDown
        ]

  bFocus <- accumB InputPanel eFocusChange

  -- ══════════════════════════════════════════════════════════════════
  -- HISTORY BEHAVIOR
  -- ══════════════════════════════════════════════════════════════════

  -- Submit happens when: InsertMode + InputPanel + Enter
  let bCanSubmit = (&&) <$> ((== InsertMode) <$> bMode)
                        <*> ((== InputPanel) <$> bFocus)
      eSubmit = whenE bCanSubmit eEnter

  -- We need to track editor content for submission
  -- This is tricky because Brick's Editor is stateful
  -- We'll use an accumulator for the text lines
  -- EditorClear resets, other keys we track via external state

  -- For now, we'll signal submission and let Brick handle the actual text extraction
  -- The FRP network will add placeholder messages that get replaced by actual content

  -- History accumulator: on submit, we add a placeholder that NewUI will fill
  let eAddMessages :: Event ([ChatMessage] -> [ChatMessage])
      eAddMessages = (\history ->
        let userMsg = ChatMessage "<<PENDING>>" UserMessage
            echoMsg = ChatMessage "<<PENDING_ECHO>>" AIMessage
        in echoMsg : userMsg : history) <$ eSubmit

  bHistory <- accumB [] eAddMessages

  -- ══════════════════════════════════════════════════════════════════
  -- OUTPUT COMMANDS
  -- ══════════════════════════════════════════════════════════════════

  -- Helper to merge value events (prefer left on simultaneous)
  let mergeEvents :: [Event a] -> Event a
      mergeEvents = foldr (unionWith const) never

  -- Scroll commands
  let eScrollCmd :: Event ScrollCmd
      eScrollCmd = mergeEvents
        -- InsertMode + HistoryPanel: arrows scroll
        [ ScrollUp   <$ whenE (liftA2 (&&) ((== InsertMode) <$> bMode)
                                           ((== HistoryPanel) <$> bFocus)) eArrowUp
        , ScrollDown <$ whenE (liftA2 (&&) ((== InsertMode) <$> bMode)
                                           ((== HistoryPanel) <$> bFocus)) eArrowDown
        -- PageUp/PageDown in InsertMode + HistoryPanel
        , ScrollPageUp   <$ whenE (liftA2 (&&) ((== InsertMode) <$> bMode)
                                               ((== HistoryPanel) <$> bFocus)) ePageUp
        , ScrollPageDown <$ whenE (liftA2 (&&) ((== InsertMode) <$> bMode)
                                               ((== HistoryPanel) <$> bFocus)) ePageDown
        -- Scroll to end after submit
        , ScrollToEnd <$ eSubmit
        ]

  -- Editor commands
  let eEditorCmd :: Event EditorCmd
      eEditorCmd = mergeEvents
        -- Shift+Enter in InsertMode + InputPanel: insert newline
        [ EditorInsertNewline <$ whenE bCanSubmit eShiftEnter
        -- Submit clears editor
        , EditorClear <$ eSubmit
        -- Move cursor to end when entering insert mode on input panel
        , EditorMoveCursorEnd <$ whenE ((== InputPanel) <$> bFocus)
                                       (whenE ((== NormalMode) <$> bMode) eEnter)
        -- Forward other keys to editor in InsertMode + InputPanel
        , EditorForward . (\(k, m) -> Vty.EvKey k m) <$>
            whenE bCanSubmit eOtherKey
        -- Also forward arrow keys to editor in InsertMode + InputPanel
        , EditorForward (Vty.EvKey Vty.KUp []) <$
            whenE (liftA2 (&&) ((== InsertMode) <$> bMode)
                               ((== InputPanel) <$> bFocus)) eArrowUp
        , EditorForward (Vty.EvKey Vty.KDown []) <$
            whenE (liftA2 (&&) ((== InsertMode) <$> bMode)
                               ((== InputPanel) <$> bFocus)) eArrowDown
        ]

  -- Halt signal
  bShouldHalt <- stepper False (True <$ eQuit)

  -- ══════════════════════════════════════════════════════════════════
  -- COMBINE INTO OUTPUT
  -- ══════════════════════════════════════════════════════════════════

  -- Convert events to Maybe for output
  bScrollCmd <- stepper Nothing (Just <$> eScrollCmd)
  bEditorCmd <- stepper Nothing (Just <$> eEditorCmd)

  -- Build output behavior
  let bOutput :: Behavior FRPOutput
      bOutput = FRPOutput
        <$> bMode
        <*> bFocus
        <*> bHistory
        <*> bEditorCmd
        <*> bScrollCmd
        <*> bShouldHalt

  -- React to changes - emit output on any input event
  reactimate $ outputHandler <$> bOutput <@ eInput
