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
import           Reactive.Banana.Frameworks ( MomentIO, reactimate, AddHandler, Handler )
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
  -> WrappingEditor Char Name  -- ^ Initial editor state
  -> (AddHandler (), Handler ())  -- ^ Startup event handler pair
  -> AttrMap  -- ^ Attribute map for rendering
  -> MomentIO ()
buildChatNetwork finMVar initialEditor startup attrMap = mdo
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
  -- EDITOR BEHAVIOR (Pure FRP - No IORef!)
  -- ══════════════════════════════════════════════════════════════════

  -- Forward key events to editor when in InsertMode + InputPanel
  let bInEditorMode = (&&) <$> ((== InsertMode) <$> bMode) <*> ((== InputPanel) <$> bFocus)

      -- Filter events that should go to editor
      eEditorKeys = Banana.filterJust $ Banana.whenE bInEditorMode (Just <$> eVty)

      -- Editor update events (pure function application)
      eEditorUpdate :: Banana.Event (WrappingEditor Char Name -> WrappingEditor Char Name)
      eEditorUpdate = Banana.unions
        [ handleEditorPure <$> eEditorKeys  -- Apply key to editor
        , const (newEditor breakExact InputField []) <$ eSubmit  -- Clear on submit
        ]

  -- Accumulate editor state purely in FRP
  bEditor <- Banana.accumB initialEditor eEditorUpdate

  -- ══════════════════════════════════════════════════════════════════
  -- SIDE EFFECTS (Submit handling)
  -- ══════════════════════════════════════════════════════════════════

  -- On submit, sample current editor and print
  -- Note: We use <@> to sample the editor at the time of submit event
  let eSubmitWithEditor = bEditor Banana.<@ eSubmit

  reactimate $ (\editor -> do
    let editorLines = dumpEditor editor
        inputText = T.unlines $ map T.pack editorLines
    unless (T.null $ T.strip inputText) $
      putStrLn $ "Submitted: " <> T.unpack inputText
    ) <$> eSubmitWithEditor

  -- ══════════════════════════════════════════════════════════════════
  -- WIDGETS BEHAVIOR
  -- ══════════════════════════════════════════════════════════════════

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
