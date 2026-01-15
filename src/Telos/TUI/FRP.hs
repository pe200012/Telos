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
  = EditorClear
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
-- syncMVar is used to synchronize when outputHandler completes
buildFRPNetwork
  :: AddHandler FRPEvent
  -> Handler FRPOutput
  -> MVar ()
  -> IO EventNetwork
buildFRPNetwork inputHandler outputHandler syncMVar = compile $ mdo
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
      -- Ctrl+Enter for submit (Enter alone inserts newline via Brick's default)
      eCtrlEnter  = () <$ filterE (\(k, m) -> k == Vty.KEnter && m == [Vty.MCtrl]) eKeyPress
      eEsc        = () <$ filterE (\(k, m) -> k == Vty.KEsc && null m) eKeyPress
      eArrowUp    = () <$ filterE (\(k, m) -> k == Vty.KUp && null m) eKeyPress
      eArrowDown  = () <$ filterE (\(k, m) -> k == Vty.KDown && null m) eKeyPress
      ePageUp     = () <$ filterE (\(k, m) -> k == Vty.KPageUp && null m) eKeyPress
      ePageDown   = () <$ filterE (\(k, m) -> k == Vty.KPageDown && null m) eKeyPress

      -- Other keys (for editor forwarding)
      isSpecialKey (k, m) = k `elem` [ Vty.KEsc, Vty.KUp, Vty.KDown, Vty.KPageUp, Vty.KPageDown ]
                         || (k == Vty.KEnter && m == [Vty.MCtrl])  -- Ctrl+Enter is submit, not forwarded
      isQuitKey (k, m) = k == Vty.KChar 'd' && m == [Vty.MCtrl]
      eOtherKey = filterE (\km -> not (isSpecialKey km) && not (isQuitKey km)) eKeyPress

  -- ══════════════════════════════════════════════════════════════════
  -- MODE BEHAVIOR
  -- ══════════════════════════════════════════════════════════════════

  -- Mode transitions:
  -- NormalMode + Enter -> InsertMode
  -- InsertMode + Esc -> NormalMode
  -- (Ctrl+Enter for submit doesn't affect mode)
  let eModeChange :: Event (Mode -> Mode)
      eModeChange = unions
        [ (\mode -> if mode == NormalMode then InsertMode else mode) <$ eEnter
        , const NormalMode <$ eEsc
        ]

  bMode <- accumB NormalMode eModeChange

  -- Compute the NEW mode at event time (for immediate output)
  let eNewMode :: Event Mode
      eNewMode = flip ($) <$> bMode <@> eModeChange

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

  -- Compute the NEW focus at event time (for immediate output)
  let eNewFocus :: Event FocusPanel
      eNewFocus = flip ($) <$> bFocus <@> eFocusChange

  -- ══════════════════════════════════════════════════════════════════
  -- HISTORY BEHAVIOR
  -- ══════════════════════════════════════════════════════════════════

  -- Submit happens when: InsertMode + InputPanel + Ctrl+Enter
  let bCanSubmit = (&&) <$> ((== InsertMode) <$> bMode)
                        <*> ((== InputPanel) <$> bFocus)
      eSubmit = whenE bCanSubmit eCtrlEnter

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
        -- Submit clears editor
        [ EditorClear <$ eSubmit
        -- Move cursor to end when entering insert mode on input panel
        , EditorMoveCursorEnd <$ whenE ((== InputPanel) <$> bFocus)
                                       (whenE ((== NormalMode) <$> bMode) eEnter)
        -- Forward Enter key to editor for newline (InsertMode + InputPanel)
        , EditorForward (Vty.EvKey Vty.KEnter []) <$ whenE bCanSubmit eEnter
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

  -- The key insight: when we sample a Behavior with <@, we get the value BEFORE
  -- the current event updates it. This causes a "one event delay" for commands.
  --
  -- Solution: Build output directly from events, embedding the NEW state values.

  -- Output event when mode changes (with NEW mode value)
  let eOutputModeChange :: Event FRPOutput
      eOutputModeChange = (\focus history shouldHalt newMode ->
                            FRPOutput newMode focus history Nothing Nothing shouldHalt)
        <$> bFocus <*> bHistory <*> bShouldHalt
        <@> eNewMode

  -- Output event when focus changes (with NEW focus value)
  let eOutputFocusChange :: Event FRPOutput
      eOutputFocusChange = (\mode history shouldHalt newFocus ->
                             FRPOutput mode newFocus history Nothing Nothing shouldHalt)
        <$> bMode <*> bHistory <*> bShouldHalt
        <@> eNewFocus

  -- For each input event, we emit an output with the commands that THIS event triggered
  let eOutputWithEditorCmd :: Event FRPOutput
      eOutputWithEditorCmd = (\mode focus history shouldHalt cmd ->
                               FRPOutput mode focus history (Just cmd) Nothing shouldHalt)
        <$> bMode <*> bFocus <*> bHistory <*> bShouldHalt
        <@> eEditorCmd

      eOutputWithScrollCmd :: Event FRPOutput
      eOutputWithScrollCmd = (\mode focus history shouldHalt cmd ->
                               FRPOutput mode focus history Nothing (Just cmd) shouldHalt)
        <$> bMode <*> bFocus <*> bHistory <*> bShouldHalt
        <@> eScrollCmd

      eOutputNoCmds :: Event FRPOutput
      eOutputNoCmds = (\mode focus history shouldHalt ->
                        FRPOutput mode focus history Nothing Nothing shouldHalt)
        <$> bMode <*> bFocus <*> bHistory <*> bShouldHalt
        <@ eInput

      -- Combine: prefer events with NEW state values, then commands, then default
      -- Priority: mode change > focus change > editor cmd > scroll cmd > no cmds
      eOutput :: Event FRPOutput
      eOutput = unionWith const eOutputModeChange
              $ unionWith const eOutputFocusChange
              $ unionWith const eOutputWithEditorCmd
              $ unionWith const eOutputWithScrollCmd
              $ eOutputNoCmds

  -- React to output events
  reactimate $ (\output -> outputHandler output >> putMVar syncMVar ()) <$> eOutput
