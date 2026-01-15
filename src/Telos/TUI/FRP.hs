{-# LANGUAGE RecursiveDo #-}

-- |
-- Module      : Telos.TUI.FRP
-- Description : FRP network for Telos chat TUI using bricki-banana pattern
-- License     : MIT
--
-- This module contains the reactive-banana FRP network definition for the
-- Telos chat TUI. It uses the bricki-banana pattern to bypass Brick's event
-- loop and maintain state purely in FRP Behaviors.

module Telos.TUI.FRP
  ( buildChatNetwork
  , handleEditorPure
  ) where

import           Brick                      ( AttrMap, CursorLocation(..), Widget, cursorLocationName )
import qualified Data.Text                  as T
import qualified Graphics.Vty               as Vty
import qualified Reactive.Banana            as Banana
import           Reactive.Banana.Frameworks ( MomentIO, fromPoll, reactimate, AddHandler, Handler )
import           Relude
import           Telos.TUI.BananaMain       ( Next(..), brickNetwork )
import           Telos.TUI.Chat
import           WEditor.Base
import           WEditor.LineWrap           ( breakExact )
import           WEditorBrick.WrappingEditor

-- | Pure editor event handling (bypasses EventM)
-- This is a pure version of handleEditor that doesn't require lookupExtent
handleEditorPure :: Vty.Event -> WrappingEditor Char Name -> WrappingEditor Char Name
handleEditorPure event = mapEditor action
  where
    action :: WrappingEditorAction Char
    action = case event of
      Vty.EvKey Vty.KBS []        -> editorBackspaceAction
      Vty.EvKey Vty.KDel []       -> editorDeleteAction
      Vty.EvKey Vty.KDown []      -> editorDownAction
      Vty.EvKey Vty.KEnd []       -> editorEndAction
      Vty.EvKey Vty.KEnter []     -> editorEnterAction
      Vty.EvKey Vty.KHome []      -> editorHomeAction
      Vty.EvKey Vty.KLeft []      -> editorLeftAction
      Vty.EvKey Vty.KPageDown []  -> editorPageDownAction
      Vty.EvKey Vty.KPageUp []    -> editorPageUpAction
      Vty.EvKey Vty.KRight []     -> editorRightAction
      Vty.EvKey Vty.KUp []        -> editorUpAction
      Vty.EvKey Vty.KDown [Vty.MMeta] -> viewerShiftDownAction 1
      Vty.EvKey Vty.KUp [Vty.MMeta]   -> viewerShiftUpAction 1
      Vty.EvKey Vty.KHome [Vty.MMeta] -> viewerFillAction
      Vty.EvKey (Vty.KChar c) [] | c `notElem` ("\t\r\n" :: String) -> editorAppendAction [c]
      _ -> id

-- | Build the complete FRP network for the chat TUI
--
-- This function sets up the entire reactive network including:
-- - Vty event processing via brickNetwork
-- - Mode and focus state management
-- - Editor handling via IORef
-- - Message history
-- - Widget rendering
-- - Cursor management
--
-- The network uses RecursiveDo to wire nextE, widgetsB, and cursorB
-- into brickNetwork while defining them based on eventE from brickNetwork.
buildChatNetwork ::
  MVar ()  -- ^ MVar to signal when application should halt
  -> IORef (WrappingEditor Char Name)  -- ^ IORef containing editor state
  -> (AddHandler (), Handler ())  -- ^ Startup event handler pair
  -> AttrMap  -- ^ Attribute map for rendering
  -> MomentIO ()
buildChatNetwork finMVar editorRef startup attrMap = mdo
  -- ══════════════════════════════════════════════════════════════════
  -- BRICK NETWORK SETUP
  -- ══════════════════════════════════════════════════════════════════

  -- brickNetwork gives us events from Vty and handles rendering
  -- Returns: (eventE :: Event (Maybe VtyEvent), finE :: Event (), suspendSetup)
  (eventE, finE, _suspendSetup) <- brickNetwork
    startup
    nextE
    widgetsB
    cursorB
    (pure attrMap)

  -- Extract just Vty events (ignore Nothing from startup)
  let eVty :: Banana.Event Vty.Event
      eVty = Banana.filterJust eventE

  -- ══════════════════════════════════════════════════════════════════
  -- KEY EVENT CLASSIFICATION
  -- ══════════════════════════════════════════════════════════════════

  let eKeyPress :: Banana.Event (Vty.Key, [Vty.Modifier])
      eKeyPress = Banana.filterJust $ fmap extractKey eVty

      extractKey (Vty.EvKey k mods) = Just (k, mods)
      extractKey _ = Nothing

  -- Specific key events
  let eQuit = () <$ Banana.filterE (\(k, m) -> k == Vty.KChar 'd' && m == [Vty.MCtrl]) eKeyPress
      eEnter = () <$ Banana.filterE (\(k, m) -> k == Vty.KEnter && null m) eKeyPress
      eCtrlEnter = () <$ Banana.filterE (\(k, m) -> k == Vty.KEnter && m == [Vty.MCtrl]) eKeyPress
      eEsc = () <$ Banana.filterE (\(k, m) -> k == Vty.KEsc && null m) eKeyPress
      eArrowUp = () <$ Banana.filterE (\(k, m) -> k == Vty.KUp && null m) eKeyPress
      eArrowDown = () <$ Banana.filterE (\(k, m) -> k == Vty.KDown && null m) eKeyPress

  -- ══════════════════════════════════════════════════════════════════
  -- MODE BEHAVIOR
  -- ══════════════════════════════════════════════════════════════════

  -- Mode transitions:
  -- NormalMode + Enter (when InputPanel focused) -> InsertMode
  -- InsertMode + Esc -> NormalMode
  let eModeChange :: Banana.Event (Mode -> Mode)
      eModeChange = Banana.unions
        [ (\mode -> if mode == NormalMode then InsertMode else mode)
            <$ Banana.whenE ((== InputPanel) <$> bFocus) eEnter
        , const NormalMode <$ eEsc
        ]

  bMode <- Banana.accumB NormalMode eModeChange

  -- ══════════════════════════════════════════════════════════════════
  -- FOCUS BEHAVIOR
  -- ══════════════════════════════════════════════════════════════════

  -- Focus transitions (only in NormalMode):
  -- NormalMode + Up -> HistoryPanel
  -- NormalMode + Down -> InputPanel
  let eFocusChange :: Banana.Event (FocusPanel -> FocusPanel)
      eFocusChange = Banana.unions
        [ const HistoryPanel <$ Banana.whenE ((== NormalMode) <$> bMode) eArrowUp
        , const InputPanel <$ Banana.whenE ((== NormalMode) <$> bMode) eArrowDown
        ]

  bFocus <- Banana.accumB InputPanel eFocusChange

  -- ══════════════════════════════════════════════════════════════════
  -- HISTORY BEHAVIOR
  -- ══════════════════════════════════════════════════════════════════

  -- Submit happens when: InsertMode + InputPanel + Ctrl+Enter
  let bCanSubmit = (&&) <$> ((== InsertMode) <$> bMode) <*> ((== InputPanel) <$> bFocus)
      eSubmit = Banana.whenE bCanSubmit eCtrlEnter

  -- On submit, read editor content and add messages
  -- History accumulates: new messages prepended
  let eAddMessages :: Banana.Event ([ChatMessage] -> [ChatMessage])
      eAddMessages = const [] <$ eSubmit  -- Will be replaced by reactimate side-effect

  bHistory <- Banana.accumB [] eAddMessages

  -- ══════════════════════════════════════════════════════════════════
  -- EDITOR HANDLING (via IORef)
  -- ══════════════════════════════════════════════════════════════════

  -- Forward key events to editor when in InsertMode + InputPanel
  let bInEditorMode = (&&) <$> ((== InsertMode) <$> bMode) <*> ((== InputPanel) <$> bFocus)

      -- Filter events that should go to editor
      eEditorKeys = Banana.filterJust $ Banana.whenE bInEditorMode (Just <$> eVty)

  -- Reactimate: update editor on key events (pure function, no EventM)
  reactimate $ (\ev -> do
    modifyIORef' editorRef (handleEditorPure ev)
    ) <$> eEditorKeys

  -- Reactimate: clear editor and update history on submit
  reactimate $ (\_ -> do
    editor <- readIORef editorRef
    let editorLines = dumpEditor editor
        inputText = T.unlines $ map T.pack editorLines
    -- Clear editor
    writeIORef editorRef $ newEditor breakExact InputField []
    -- Note: history update would need a different mechanism
    -- For now, just print
    unless (T.null $ T.strip inputText) $
      putStrLn $ "Submitted: " <> T.unpack inputText
    ) <$> eSubmit

  -- ══════════════════════════════════════════════════════════════════
  -- WIDGETS BEHAVIOR
  -- ══════════════════════════════════════════════════════════════════

  -- Poll editor state from IORef on every event
  -- fromPoll creates a Behavior that reads from IO on each network step
  bEditor <- fromPoll (readIORef editorRef)

  -- Build ChatState from behaviors
  let bChatState :: Banana.Behavior ChatState
      bChatState = ChatState
        <$> bHistory
        <*> bEditor
        <*> bMode
        <*> bFocus

  -- Widgets behavior - drawChatUI is pure
  let widgetsB :: Banana.Behavior [Widget Name]
      widgetsB = drawChatUI <$> bChatState

  -- Cursor behavior - show cursor when in insert mode on input panel
  -- cursorLocationName returns Maybe n, so we need to handle that
  let cursorB :: Banana.Behavior ([CursorLocation Name] -> Maybe (CursorLocation Name))
      cursorB = (\mode focus ->
        if mode == InsertMode && focus == InputPanel
        then listToMaybe . filter (\curLoc -> cursorLocationName curLoc == Just InputField)
        else const Nothing) <$> bMode <*> bFocus

  -- ══════════════════════════════════════════════════════════════════
  -- CONTROL FLOW
  -- ══════════════════════════════════════════════════════════════════

  -- Decide whether to redraw or halt
  -- Use unionWith since Next is a Monoid (Halt wins over Redraw)
  -- Note: eventE includes startup (Nothing) and Vty events (Just _)
  -- We need to redraw on startup too, hence using eventE not just eVty
  let nextE :: Banana.Event Next
      nextE = foldr (Banana.unionWith (<>)) Banana.never
        [ Halt <$ eQuit
        , Redraw <$ eventE  -- Redraw on any event including startup
        ]

  -- When FRP network finishes, signal main thread
  reactimate $ putMVar finMVar () <$ finE

  pure ()
