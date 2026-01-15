module Telos.TUI.Chat
  ( ChatState(..)
  , Name(..)
  , KeyEnter(..)
  , Mode(..)
  , FocusPanel(..)
  , initialChatState
  , initialAttrMap
  , drawChatUI
  , handleChatEvent
  ) where

import           Brick
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import           Brick.Widgets.Edit (Editor, editAttr, editFocusedAttr,
                                          editorText, getEditContents,
                                          handleEditorEvent, renderEditor)

import           Control.Lens               ( (.=), Lens', lens, use )

import qualified Data.Text                  as T

import qualified Graphics.Vty               as Vty

import           Relude

-- | Widget name type
data Name = InputField | HistoryViewport
  deriving ( Eq, Ord, Show )

-- | Enter key event (for custom app events)
data KeyEnter = KeyEnter
  deriving ( Eq, Show )

-- | Input mode (vim-like)
data Mode = NormalMode | InsertMode
  deriving ( Eq, Show )

-- | Which panel is focused
data FocusPanel = HistoryPanel | InputPanel
  deriving ( Eq, Show )

-- | Chat message
data ChatMessage = ChatMessage { messageText :: Text, messageTime :: Text }
  deriving ( Eq, Show )

-- | Main application state
data ChatState
  = ChatState { chatHistory :: [ ChatMessage ]
              , chatEditor  :: Editor Text Name
              , currentMode :: Mode
              , focusPanel  :: FocusPanel
              }
  deriving stock ( Show )

-- | Lens for editor field
editorL :: Lens' ChatState (Editor Text Name)
editorL = lens chatEditor (\s v -> s { chatEditor = v })

-- | Lens for mode field
modeL :: Lens' ChatState Mode
modeL = lens currentMode (\s v -> s { currentMode = v })

-- | Lens for focus field
focusL :: Lens' ChatState FocusPanel
focusL = lens focusPanel (\s v -> s { focusPanel = v })

-- | Initial chat state with empty history
initialChatState :: ChatState
initialChatState
  = ChatState { chatHistory = []
              , chatEditor  = editorText InputField (Just 1) ""
              , currentMode = NormalMode
              , focusPanel  = InputPanel
              }

-- | Initial attribute map
initialAttrMap :: AttrMap
initialAttrMap
  = attrMap
    Vty.defAttr
    [ ( attrName "timestamp", fg Vty.magenta )
    , ( attrName "focused", Vty.withStyle Vty.currentAttr Vty.bold )
    , ( attrName "mode.normal", fg Vty.cyan )
    , ( attrName "mode.insert", fg Vty.green )
      -- Panel colors
    , ( attrName "panel.history.focused", bg Vty.black `Vty.withStyle` Vty.bold )
    , ( attrName "panel.history.unfocused", bg Vty.black )
    , ( attrName "panel.input.focused", bg Vty.black `Vty.withStyle` Vty.bold )
    , ( attrName "panel.input.unfocused", bg Vty.black )
    , ( attrName "border.focused", fg Vty.cyan `Vty.withStyle` Vty.bold )
    , ( attrName "border.unfocused", fg Vty.brightBlack )
    , ( attrName "statusbar"
      , Vty.defAttr `Vty.withBackColor` Vty.blue `Vty.withForeColor` Vty.white
        `Vty.withStyle` Vty.bold
      )
    , ( attrName "message.text", fg Vty.white )
    , ( attrName "editor.focused", fg Vty.white )
    , ( attrName "editor.unfocused", fg Vty.brightBlack )
      -- Brick's built-in editor attributes (used by renderEditor internally)
    , ( editAttr, fg Vty.brightBlack )        -- Unfocused editor: gray text
    , ( editFocusedAttr, fg Vty.white )       -- Focused editor: white text
    ]

-- | Draw the chat UI
drawChatUI :: ChatState -> [ Widget Name ]
drawChatUI st = [ ui ]
  where
    ui = vBox [ historyWidget, B.hBorder, inputWidget, B.hBorder, statusBar ]

    -- History viewport (scrollable text display)
    historyWidget :: Widget Name
    historyWidget
      = let
          isFocused   = focusPanel st == HistoryPanel
          borderAttr
            = if isFocused
              then attrName "border.focused"
              else attrName "border.unfocused"
          panelAttr
            = if isFocused
              then attrName "panel.history.focused"
              else attrName "panel.history.unfocused"
          borderStyle
            = if isFocused && currentMode st == NormalMode
              then BS.unicodeBold
              else BS.unicode
        in 
          modifyDefAttr (const $ attrMapLookup borderAttr initialAttrMap)
          $ withBorderStyle borderStyle
          $ B.borderWithLabel (txt " History ")
          $ withAttr panelAttr
          $ viewport HistoryViewport Vertical
          $ padAll 1
          $ vBox (reverse $ map drawMessage (chatHistory st))

    -- Draw a single message
    drawMessage :: ChatMessage -> Widget Name
    drawMessage msg
      = padLeftRight 1
      $ padTop (Pad 1)
      $ hBox
        [ withAttr (attrName "timestamp") $ txt (messageTime msg)
        , txt " "
        , withAttr (attrName "message.text") $ txt (messageText msg)
        ]

    -- Input box at the bottom
    inputWidget :: Widget Name
    inputWidget
      = let
          isFocused   = focusPanel st == InputPanel
          borderAttr
            = if isFocused
              then attrName "border.focused"
              else attrName "border.unfocused"
          panelAttr
            = if isFocused
              then attrName "panel.input.focused"
              else attrName "panel.input.unfocused"
          borderStyle
            = if isFocused && currentMode st == NormalMode
              then BS.unicodeBold
              else BS.unicode
        in 
          modifyDefAttr (const $ attrMapLookup borderAttr initialAttrMap)
          $ withBorderStyle borderStyle
          $ B.borderWithLabel (txt " Input ")
          $ withAttr panelAttr
          $ padAll 1
          $ vLimit 3
          $ let editorTextAttr = if currentMode st == InsertMode && focusPanel st == InputPanel
                                 then attrName "editor.focused"
                                 else attrName "editor.unfocused"
            in withAttr editorTextAttr
             $ renderEditor
               (txt . T.unlines)
               (currentMode st == InsertMode && focusPanel st == InputPanel)
               (chatEditor st)

    -- Status bar at bottom
    statusBar :: Widget Name
    statusBar
      = withAttr (attrName "statusbar")
      $ hLimit 1000  -- Force full width
      $ padLeftRight 1
      $ hBox [ modeWidget, txt " | ", txt $ "Focus: " <> case focusPanel st of
        HistoryPanel -> "History"
        InputPanel   -> "Input", txt " | ", txt "Ctrl+D: Quit" ]

    modeWidget :: Widget Name
    modeWidget = case currentMode st of
      NormalMode -> txt "-- NORMAL --"
      InsertMode -> txt "-- INSERT --"

-- | Event handler
handleChatEvent :: BrickEvent Name KeyEnter -> EventM Name ChatState ()
-- Quit with Ctrl+D in any mode
handleChatEvent (VtyEvent (Vty.EvKey (Vty.KChar 'd') [ Vty.MCtrl ])) = halt

-- Normal mode: arrow key navigation (Up/Down arrows)
handleChatEvent (VtyEvent (Vty.EvKey Vty.KUp [])) = do
  mode <- use modeL
  focus <- use focusL
  case mode of
    NormalMode -> focusL .= HistoryPanel
    InsertMode -> case focus of
      HistoryPanel -> vScrollBy (viewportScroll HistoryViewport) (-1)
      InputPanel   -> zoom editorL $ handleEditorEvent (VtyEvent (Vty.EvKey Vty.KUp []))

handleChatEvent (VtyEvent (Vty.EvKey Vty.KDown [])) = do
  mode <- use modeL
  focus <- use focusL
  case mode of
    NormalMode -> focusL .= InputPanel
    InsertMode -> case focus of
      HistoryPanel -> vScrollBy (viewportScroll HistoryViewport) 1
      InputPanel   -> zoom editorL $ handleEditorEvent (VtyEvent (Vty.EvKey Vty.KDown []))

-- Normal mode: Enter to switch to Insert mode
handleChatEvent (VtyEvent (Vty.EvKey Vty.KEnter [])) = do
  mode <- use modeL
  focus <- use focusL
  when (mode == NormalMode) $ do
    modeL .= InsertMode
    -- When entering insert mode on input panel, position cursor at end
    when (focus == InputPanel) $ do
      zoom editorL $ handleEditorEvent (VtyEvent (Vty.EvKey Vty.KEnd []))

-- Insert mode: Esc to return to Normal mode
handleChatEvent (VtyEvent (Vty.EvKey Vty.KEsc [])) = do
  mode <- use modeL
  when (mode == InsertMode) $ modeL .= NormalMode

-- Insert mode on Input panel: handle custom Enter event for submission
handleChatEvent (AppEvent KeyEnter) = do
  mode <- use modeL
  focus <- use focusL
  when (mode == InsertMode && focus == InputPanel) $ do
    currentText <- use editorL
    let text = getEditContents currentText
    unless (null text) $ do
      let inputText = T.unlines text
      unless (T.null inputText) $ do
        let currentTime = ">>"
        let newMessage = ChatMessage inputText currentTime
        modify $ \s -> s { chatHistory = newMessage : chatHistory s }
        -- Clear the editor
        editorL .= editorText InputField (Just 1) ""
        -- Scroll to bottom of history
        vScrollToEnd $ viewportScroll HistoryViewport

-- Insert mode: PageUp/PageDown for scrolling history
handleChatEvent (VtyEvent (Vty.EvKey Vty.KPageUp [])) = do
  mode <- use modeL
  focus <- use focusL
  when (mode == InsertMode && focus == HistoryPanel)
    $ vScrollPage (viewportScroll HistoryViewport) Brick.Up

handleChatEvent (VtyEvent (Vty.EvKey Vty.KPageDown [])) = do
  mode <- use modeL
  focus <- use focusL
  when (mode == InsertMode && focus == HistoryPanel)
    $ vScrollPage (viewportScroll HistoryViewport) Brick.Down

-- Insert mode on Input panel: forward other events to editor
handleChatEvent e = do
  mode <- use modeL
  focus <- use focusL
  when (mode == InsertMode && focus == InputPanel) $ zoom editorL $ handleEditorEvent e
