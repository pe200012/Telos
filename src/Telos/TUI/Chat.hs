module Telos.TUI.Chat
  ( ChatState(..)
  , Name(..)
  , KeyEnter(..)
  , initialChatState
  , initialAttrMap
  , drawChatUI
  , handleChatEvent
  ) where

import           Brick
import qualified Brick.Widgets.Border as B
import           Brick.Widgets.Edit

import           Control.Lens         ( (.=), Lens', lens, use )

import qualified Data.Text            as T

import qualified Graphics.Vty         as Vty

import           Relude

-- | Widget name type
data Name = InputField | HistoryViewport
  deriving ( Eq, Ord, Show )

-- | Enter key event (for custom app events)
data KeyEnter = KeyEnter
  deriving ( Eq, Show )

-- | Chat message
data ChatMessage = ChatMessage { messageText :: Text, messageTime :: Text }
  deriving ( Eq, Show )

-- | Main application state
data ChatState = ChatState { chatHistory :: [ ChatMessage ], chatEditor :: Editor Text Name }
  deriving stock ( Show )

-- | Lens for editor field
editorL :: Lens' ChatState (Editor Text Name)
editorL = lens chatEditor (\s v -> s { chatEditor = v })

-- | Initial chat state with empty history
initialChatState :: ChatState
initialChatState = ChatState { chatHistory = [], chatEditor = editorText InputField (Just 1) "" }

-- | Initial attribute map
initialAttrMap :: AttrMap
initialAttrMap = attrMap Vty.defAttr [ ( attrName "timestamp", fg Vty.magenta ) ]

-- | Draw the chat UI
drawChatUI :: ChatState -> Widget Name
drawChatUI st = B.border $ padAll 1 $ vBox [ historyWidget, inputWidget ]
  where
    -- History viewport (scrollable text display)
    historyWidget :: Widget Name
    historyWidget
      = hLimit 60
      $ vLimit 20
      $ viewport HistoryViewport Vertical
      $ vBox (reverse $ map drawMessage (chatHistory st))

    -- Draw a single message
    drawMessage :: ChatMessage -> Widget Name
    drawMessage msg
      = padLeftRight 1
      $ padTop (Pad 1)
      $ hBox
        [ withAttr (attrName "timestamp") $ txt (messageTime msg), txt " ", txt (messageText msg) ]

    -- Input box at the bottom
    inputWidget :: Widget Name
    inputWidget = padTop (Pad 1) $ hLimit 60 $ renderEditor (txt . T.unlines) True (chatEditor st)

-- | Event handler
handleChatEvent :: BrickEvent Name KeyEnter -> EventM Name ChatState ()
handleChatEvent (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt
handleChatEvent (VtyEvent (Vty.EvKey Vty.KEsc [])) = halt
handleChatEvent (AppEvent KeyEnter) = do
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
handleChatEvent e = do
  zoom editorL $ handleEditorEvent e
