{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}

module Telos.TUI.Chat
  ( ChatState(..)
  , Name(..)
  , Mode(..)
  , FocusPanel(..)
  , MessageSender(..)
  , ChatMessage(..)
  , initialChatState
  , initialAttrMap
  , drawChatUI
  -- Lenses
  , chatEditor
  , chatHistory
  , currentMode
  , focusedPanel
  , messageText
  , messageSender
  ) where

import           Brick
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS

import           Control.Lens               ( (^.), makeLenses )

import qualified Data.Text                  as T

import qualified Graphics.Vty               as Vty

import           Relude

import           WEditorBrick.WrappingEditor
import "WEditor" WEditor.LineWrap            ( BreakWords, breakExact )

-- | Widget name type
data Name = InputField | HistoryViewport
  deriving ( Eq, Ord, Show )

-- | Input mode (vim-like)
data Mode = NormalMode | InsertMode
  deriving ( Eq, Show )

-- | Which panel is focused
data FocusPanel = HistoryPanel | InputPanel
  deriving ( Eq, Show )

-- | Message sender
data MessageSender = UserMessage | AIMessage
  deriving ( Eq, Show )

-- | Chat message
data ChatMessage = ChatMessage
  { _messageText   :: Text
  , _messageSender :: MessageSender
  }
  deriving stock ( Eq, Show )
makeLenses ''ChatMessage

-- | Main application state
data ChatState = ChatState
  { _chatHistory :: [ChatMessage]
  , _chatEditor  :: WrappingEditor Char Name
  , _currentMode :: Mode
  , _focusedPanel  :: FocusPanel
  }
  deriving stock ( Show )
makeLenses ''ChatState

-- | Initial chat state with empty history
initialChatState :: ChatState
initialChatState
  = ChatState { _chatHistory = []
              , _chatEditor  = newEditor (breakExact :: BreakWords Char) InputField []
              , _currentMode = NormalMode
              , _focusedPanel  = InputPanel
              }

-- | Initial attribute map
initialAttrMap :: AttrMap
initialAttrMap = attrMap Vty.defAttr
  [ ( attrName "timestamp", fg Vty.magenta )
  , ( attrName "message.text", fg Vty.white )
  , ( attrName "message.user.bar", fg Vty.cyan )
  , ( attrName "border.focused", fg Vty.cyan `Vty.withStyle` Vty.bold )
  , ( attrName "border.unfocused", fg Vty.brightBlack )
  , ( attrName "panel.history.focused", bg Vty.black )
  , ( attrName "panel.history.unfocused", bg Vty.black )
  , ( attrName "panel.input.focused", bg Vty.black )
  , ( attrName "panel.input.unfocused", bg Vty.black )
  , ( attrName "statusbar", fg Vty.white `Vty.withStyle` Vty.bold `Vty.withBackColor` Vty.blue )
  , ( attrName "mode.normal", fg Vty.green )
  , ( attrName "mode.insert", fg Vty.yellow )
  ]

-- | Draw the chat UI
drawChatUI :: ChatState -> [Widget Name]
drawChatUI st =
  [ vBox [ historyWidget, inputWidget, statusBar ] ]
  where
    historyWidget :: Widget Name
    historyWidget = let
      isFocused = st ^. focusedPanel == HistoryPanel
      borderAttr = if isFocused
        then attrName "border.focused"
        else attrName "border.unfocused"
      panelAttr = if isFocused
        then attrName "panel.history.focused"
        else attrName "panel.history.unfocused"
      borderStyle = if isFocused && st ^. currentMode == NormalMode
        then BS.unicodeBold
        else BS.unicode
      in modifyDefAttr (const $ attrMapLookup borderAttr initialAttrMap)
        $ withBorderStyle borderStyle
        $ B.borderWithLabel (txt " History ")
        $ withAttr panelAttr
        $ viewport HistoryViewport Vertical
        $ padAll 1
        $ vBox (reverse $ map drawMessage (st ^. chatHistory))

    drawMessage :: ChatMessage -> Widget Name
    drawMessage msg = padLeftRight 1 $ padTop (Pad 1) $ case msg ^. messageSender of
      UserMessage -> vBox $ map (drawUserLine . txt) (T.lines $ msg ^. messageText)
      AIMessage -> withAttr (attrName "message.text") $ txtWrap (msg ^. messageText)

    drawUserLine :: Widget Name -> Widget Name
    drawUserLine content = hBox
      [ withAttr (attrName "message.user.bar") $ txt "│ "
      , withAttr (attrName "message.text") content
      ]

    inputWidget :: Widget Name
    inputWidget = let
      isFocused = st ^. focusedPanel == InputPanel
      borderAttr = if isFocused
        then attrName "border.focused"
        else attrName "border.unfocused"
      panelAttr = if isFocused
        then attrName "panel.input.focused"
        else attrName "panel.input.unfocused"
      borderStyle = if isFocused && st ^. currentMode == NormalMode
        then BS.unicodeBold
        else BS.unicode
      editorTextAttr = if st ^. currentMode == InsertMode && st ^. focusedPanel == InputPanel
        then attrName "editor.focused"
        else attrName "editor.unfocused"
      in modifyDefAttr (const $ attrMapLookup borderAttr initialAttrMap)
        $ withBorderStyle borderStyle
        $ B.borderWithLabel (txt " Input ")
        $ withAttr panelAttr
        $ padAll 1
        $ vLimit 3
        $ withAttr editorTextAttr
        $ renderEditor
          (st ^. currentMode == InsertMode && st ^. focusedPanel == InputPanel)
          (st ^. chatEditor)

    statusBar :: Widget Name
    statusBar = let
      modeWidget = case st ^. currentMode of
        NormalMode -> txt "-- NORMAL --"
        InsertMode -> txt "-- INSERT --"
      focusText = case st ^. focusedPanel of
        HistoryPanel -> "History"
        InputPanel -> "Input"
      in withAttr (attrName "statusbar")
        $ hLimit 1000
        $ padLeftRight 1
        $ hBox [ modeWidget, txt " | ", txt $ "Focus: " <> focusText, txt " | ", txt "Ctrl+Enter: Submit | Ctrl+D: Quit" ]
