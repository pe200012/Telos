
module NewUI ( main ) where

import           Brick
import           Brick.Widgets.Edit

import           Control.Concurrent         ( forkIO )
import           Control.Concurrent.Chan

import qualified Data.Text                  as T

import qualified Graphics.Vty               as Vty
import qualified Graphics.Vty.CrossPlatform as VtyCross

import           Reactive.Banana.Frameworks ( actuate, newAddHandler )

import           Relude

import           Telos.TUI.Chat
import           Telos.TUI.FRP

-- | Main application entry point
main :: IO ()
main = do
  -- Create channel for receiving submitted messages
  submitChan <- newChan

  -- Setup FRP event handlers
  ( eventInput, eventTrigger ) <- newAddHandler

  -- Build and start FRP network
  network <- buildFRPNetwork eventInput submitChan
  actuate network

  -- Start a thread to listen to submitted messages and log them
  void $ forkIO $ listenToSubmissions submitChan

  -- Start Brick application
  let app
        = App { appDraw         = drawApp
              , appChooseCursor = showFirstCursor
              , appHandleEvent  = handleEvent eventTrigger
              , appStartEvent   = return ()
              , appAttrMap      = const initialAttrMap
              }

  let buildVty = VtyCross.mkVty Vty.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty Nothing app initialChatState

-- | Draw application
drawApp :: ChatState -> [ Widget Name ]
drawApp = drawChatUI

-- | Handle events and optionally trigger FRP events
handleEvent :: (FRPEvent -> IO ()) -> BrickEvent Name KeyEnter -> EventM Name ChatState ()
handleEvent _trigger (VtyEvent (Vty.EvKey Vty.KEnter [])) = do
  cs <- get
  -- Check if we're in Insert mode on Input panel - only then submit
  if currentMode cs == InsertMode && focusPanel cs == InputPanel
    then do
      let editorContent = getEditContents $ chatEditor cs
      let currentText = case editorContent of
            []      -> ""
            (x : _) -> x
      unless (T.null currentText) $ do
        -- Trigger FRP submit event (for future use)
        -- liftIO $ trigger SubmitInput
        handleChatEvent (AppEvent KeyEnter)
    else 
      -- Otherwise, let the normal event handler deal with it (mode switching)
      handleChatEvent (VtyEvent (Vty.EvKey Vty.KEnter []))
handleEvent _trigger e = do
  -- Pass other events through
  handleChatEvent e

-- | Listen to submissions from channel (demonstrates FRP connectivity)
listenToSubmissions :: Chan Text -> IO ()
listenToSubmissions chan = forever $ do
  msg <- readChan chan
  -- In a real app, this would be where we'd send message to LLM
  -- For now, we just log it
  putStrLn $ "Submitted: " <> toString msg
