{-# LANGUAGE RecursiveDo #-}

-- |
-- Module      : Telos.TUI.FRP
-- Description : FRP network for Telos chat TUI using bricki-banana pattern
-- License     : MIT
--
-- This module contains the reactive-banana FRP network definition for the
-- Telos chat TUI. It uses the bricki-banana pattern to bypass Brick's event
-- loop and maintain state purely in FRP Behaviors.

module Telos.TUI.FRP ( buildChatNetwork, handleEditorPure ) where

import           Brick                       ( AttrMap
                                             , CursorLocation(..)
                                             , Extent
                                             , Widget
                                             , cursorLocationName
                                             , extentSize
                                             )

import qualified Data.Map.Strict             as M
import qualified Data.Text                   as T

import           Graphics.Vty                ( Event(EvKey), Key(..), Modifier(..) )

import           Reactive.Banana             ( (<@)
                                             , accumB
                                             , filterE
                                             , filterJust
                                             , never
                                             , stepper
                                             , unionWith
                                             , unions
                                             , whenE, (<@>)
                                             )
import qualified Reactive.Banana             as Banana
import           Reactive.Banana.Frameworks  ( AddHandler, Handler, MomentIO, reactimate )

import           Relude

import           Telos.TUI.BananaMain        ( Next(..), brickNetwork )
import           Telos.TUI.Chat

import           WEditor.Base
import           WEditor.LineWrap            ( breakExact )

import           WEditorBrick.WrappingEditor

-- | Pure editor event handling (bypasses EventM)
-- This is a pure version of handleEditor that doesn't require lookupExtent.
-- The viewport size is passed as a parameter from the extent system.
handleEditorPure :: ( Int, Int ) -> Event -> WrappingEditor Char Name -> WrappingEditor Char Name
handleEditorPure size event = mapEditor (action . viewerResizeAction size)
  where
    action :: WrappingEditorAction Char
    action = case event of
      EvKey KBS [] -> editorBackspaceAction
      EvKey KDel [] -> editorDeleteAction
      EvKey KDown [] -> editorDownAction
      EvKey KEnd [] -> editorEndAction
      EvKey KEnter [] -> editorEnterAction
      EvKey KHome [] -> editorHomeAction
      EvKey KLeft [] -> editorLeftAction
      EvKey KPageDown [] -> editorPageDownAction
      EvKey KPageUp [] -> editorPageUpAction
      EvKey KRight [] -> editorRightAction
      EvKey KUp [] -> editorUpAction
      EvKey KDown [ MMeta ] -> viewerShiftDownAction 1
      EvKey KUp [ MMeta ] -> viewerShiftUpAction 1
      EvKey KHome [ MMeta ] -> viewerFillAction
      EvKey (KChar c) []
        | c `notElem` ("\t\r\n" :: String) -> editorAppendAction [ c ]
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
buildChatNetwork :: MVar ()  -- ^ MVar to signal when application should halt
                 -> WrappingEditor Char Name  -- ^ Initial editor state
                 -> ( AddHandler (), Handler () )  -- ^ Startup event handler pair
                 -> AttrMap  -- ^ Attribute map for rendering
                 -> MomentIO ()
buildChatNetwork finMVar initialEditor startup attrMap = mdo
  -- ══════════════════════════════════════════════════════════════════
  -- BRICK NETWORK SETUP
  -- ══════════════════════════════════════════════════════════════════

  -- brickNetwork gives us events from Vty and handles rendering
  -- Returns: (eventE :: Event (Maybe VtyEvent), finE :: Event (), suspendSetup, extentE)
  ( eventE, finE, _suspendSetup, extentE )
    <- brickNetwork startup nextE widgetsB cursorB (pure attrMap)

  -- Extract just Vty events (ignore Nothing from startup)
  let eVty :: Banana.Event Event
      eVty = filterJust eventE

  -- ══════════════════════════════════════════════════════════════════
  -- KEY EVENT CLASSIFICATION
  -- ══════════════════════════════════════════════════════════════════

  let eKeyPress :: Banana.Event ( Key, [ Modifier ] )
      eKeyPress = filterJust $ fmap extractKey eVty

      extractKey (EvKey k mods) = Just ( k, mods )
      extractKey _ = Nothing

  -- Specific key events
  let eQuit      = void (filterE (\( k, m ) -> k == KChar 'd' && m == [ MCtrl ]) eKeyPress)
      eEnter     = void (filterE (\( k, m ) -> k == KEnter && null m) eKeyPress)
      -- Note: Ctrl+Enter is not distinguishable from Enter in most terminals
      -- (terminals send ASCII bytes, not key events). Use Alt+Enter instead.
      -- Alt is sent as MMeta in vty.
      eAltEnter  = void (filterE (\( k, m ) -> k == KEnter && MMeta `elem` m) eKeyPress)
      eEsc       = void (filterE (\( k, m ) -> k == KEsc && null m) eKeyPress)
      eArrowUp   = void (filterE (\( k, m ) -> k == KUp && null m) eKeyPress)
      eArrowDown = void (filterE (\( k, m ) -> k == KDown && null m) eKeyPress)

  -- ══════════════════════════════════════════════════════════════════
  -- MODE BEHAVIOR
  -- ══════════════════════════════════════════════════════════════════

  -- Mode transitions:
  -- NormalMode + Enter (when InputPanel focused) -> InsertMode
  -- InsertMode + Esc -> NormalMode
  let eModeChange :: Banana.Event (Mode -> Mode)
      eModeChange
        = unions
          [ (\mode -> if mode == NormalMode
               then InsertMode
               else mode) <$ whenE ((== InputPanel) <$> bFocus) eEnter, const NormalMode <$ eEsc ]

  bMode <- accumB NormalMode eModeChange

  -- ══════════════════════════════════════════════════════════════════
  -- FOCUS BEHAVIOR
  -- ══════════════════════════════════════════════════════════════════

  -- Focus transitions (only in NormalMode):
  -- NormalMode + Up -> HistoryPanel
  -- NormalMode + Down -> InputPanel
  let eFocusChange :: Banana.Event (FocusPanel -> FocusPanel)
      eFocusChange
        = unions
          [ const HistoryPanel <$ whenE ((== NormalMode) <$> bMode) eArrowUp
          , const InputPanel <$ whenE ((== NormalMode) <$> bMode) eArrowDown
          ]

  bFocus <- accumB InputPanel eFocusChange

  -- ══════════════════════════════════════════════════════════════════
  -- HISTORY BEHAVIOR
  -- ══════════════════════════════════════════════════════════════════

  -- Submit happens when: InsertMode + InputPanel + Alt+Enter
  let bCanSubmit = ((&&) . (== InsertMode) <$> bMode) <*> ((== InputPanel) <$> bFocus)
      eSubmit    = whenE bCanSubmit eAltEnter

  -- We need bEditor to sample it on submit, but bEditor is defined later
  -- Use mdo's recursive binding - bEditor will be available here

  -- On submit, sample editor content and create messages
  -- Note: <@ samples the behavior BEFORE the event's updates are applied
  -- So we get the editor content before it's cleared
  let eSubmitContent :: Banana.Event Text
      eSubmitContent = (\editor -> let
                            editorLines = dumpEditor editor
                          in
                            T.unlines $ map T.pack editorLines) <$> (bEditor <@ eSubmit)

      -- Create user message and placeholder AI response
      eAddMessages :: Banana.Event ([ ChatMessage ] -> [ ChatMessage ])
      eAddMessages
        = (\content history -> if T.null (T.strip content)
             then history  -- Don't add empty messages
             else let
                 userMsg = ChatMessage content UserMessage
                 aiMsg   = ChatMessage "[AI response placeholder]" AIMessage
               in
                 aiMsg : userMsg : history  -- Prepend new messages
           )
        <$> eSubmitContent

  bHistory <- accumB [] eAddMessages

  -- ══════════════════════════════════════════════════════════════════
  -- EDITOR SIZE BEHAVIOR (from Brick extents)
  -- ══════════════════════════════════════════════════════════════════

  -- Extract InputField extent size from the extent map
  -- Default to (80, 5) if extent is not yet available
  let extractEditorSize :: M.Map Name (Extent Name) -> ( Int, Int )
      extractEditorSize exts = maybe (80, 5) extentSize (M.lookup InputField exts)  -- Default fallback

      eEditorSize = extractEditorSize <$> extentE

  -- Stepper creates a Behavior from an Event with initial value
  bEditorSize <- stepper ( 80, 5 ) eEditorSize

  -- ══════════════════════════════════════════════════════════════════
  -- EDITOR BEHAVIOR (Pure FRP - No IORef!)
  -- ══════════════════════════════════════════════════════════════════

  -- Forward key events to editor when in InsertMode + InputPanel
  -- BUT exclude Alt+Enter which is for submit
  let bInEditorMode = ((&&) . (== InsertMode) <$> bMode) <*> ((== InputPanel) <$> bFocus)

      -- Check if event is Alt+Enter (should not go to editor, used for submit)
      isAltEnter (EvKey KEnter mods) = MMeta `elem` mods
      isAltEnter _ = False

      -- Filter events that should go to editor (exclude Alt+Enter)
      eEditorKeys = filterJust $ whenE bInEditorMode $ (\ev -> if isAltEnter ev
                                                          then Nothing
                                                          else Just ev) <$> eVty

      -- Editor update events - use dynamic size from bEditorSize
      -- We sample bEditorSize at the time of the key event using <@>
      eEditorUpdate :: Banana.Event (WrappingEditor Char Name -> WrappingEditor Char Name)
      eEditorUpdate
        = unions
          [ handleEditorPure <$> bEditorSize <@> eEditorKeys
          , const (newEditor breakExact InputField []) <$ eSubmit  -- Clear on submit
          ]

   -- Accumulate editor state purely in FRP
  bEditor <- accumB initialEditor eEditorUpdate

  -- ══════════════════════════════════════════════════════════════════
  -- SIDE EFFECTS (Submit handling)
  -- ══════════════════════════════════════════════════════════════════

  -- On submit, sample current editor and print
  -- Note: We use <@> to sample the editor at the time of submit event
  -- let eSubmitWithEditor = bEditor Banana.<@ eSubmit

  -- reactimate $ (\editor -> do
  --   let editorLines = dumpEditor editor
  --       inputText = T.unlines $ map T.pack editorLines
  --   unless (T.null $ T.strip inputText) $
  --     putStrLn $ "Submitted: " <> T.unpack inputText
  --   ) <$> eSubmitWithEditor

  -- ══════════════════════════════════════════════════════════════════
  -- WIDGETS BEHAVIOR
  -- ══════════════════════════════════════════════════════════════════

  -- Build ChatState from behaviors
  let bChatState :: Banana.Behavior ChatState
      bChatState = ChatState <$> bHistory <*> bEditor <*> bMode <*> bFocus

  -- Widgets behavior - drawChatUI is pure
  let widgetsB :: Banana.Behavior [ Widget Name ]
      widgetsB = drawChatUI <$> bChatState

  -- Cursor behavior - show cursor when in insert mode on input panel
  -- cursorLocationName returns Maybe n, so we need to handle that
  let cursorB :: Banana.Behavior ([ CursorLocation Name ] -> Maybe (CursorLocation Name))
      cursorB
        = (\mode focus -> if mode == InsertMode && focus == InputPanel
             then find (\curLoc -> cursorLocationName curLoc == Just InputField)
             else const Nothing) <$> bMode <*> bFocus

  -- ══════════════════════════════════════════════════════════════════
  -- CONTROL FLOW
  -- ══════════════════════════════════════════════════════════════════

   -- Decide whether to redraw or halt
   -- Use unionWith since Next is a Monoid (Halt wins over Redraw)
   -- Note: eventE includes startup (Nothing) and Vty events (Just _)
   -- We need to redraw on startup too, hence using eventE not just eVty
  let nextE :: Banana.Event Next
      nextE = foldr
          (unionWith (<>))
          never
          [ Halt <$ eQuit
          , Redraw <$ eventE  -- Redraw on any event including startup
          ]

  -- When FRP network finishes, signal main thread
  reactimate $ putMVar finMVar () <$ finE
