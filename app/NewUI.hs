module NewUI ( main ) where

import           Brick                       hiding ( zoom )
import qualified Brick                       as Brick
import           Brick.Widgets.Edit         ( editorText, getEditContents
                                            , handleEditorEvent )

import           Control.Lens               ( (.=), use, (^. ) )

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
  -- Setup FRP event handlers
  (frpInput, frpTrigger) <- newAddHandler

  -- IORef to store latest FRP output for Brick to consume
  outputRef <- newIORef Nothing

  -- Build and start FRP network
  let outputHandler output = writeIORef outputRef (Just output)
  network <- buildFRPNetwork frpInput outputHandler
  actuate network

  -- Start Brick application
  let app
        = App { appDraw         = drawChatUI
              , appChooseCursor = showFirstCursor
              , appHandleEvent  = handleEvent frpTrigger outputRef
              , appStartEvent   = return ()
              , appAttrMap      = const initialAttrMap
              }

  let buildVty = VtyCross.mkVty Vty.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty Nothing app initialChatState

-- | Handle events: feed to FRP, then apply FRP output
handleEvent
  :: (FRPEvent -> IO ())
  -> IORef (Maybe FRPOutput)
  -> BrickEvent Name e
  -> EventM Name ChatState ()
handleEvent frpTrigger outputRef (VtyEvent vtyEvent) = do
  -- Feed event to FRP network
  liftIO $ frpTrigger (FRPVtyEvent vtyEvent)

  -- Read FRP output
  mOutput <- liftIO $ readIORef outputRef

  case mOutput of
    Nothing -> pass
    Just output -> applyFRPOutput output

-- Ignore non-VTY events for now
handleEvent _ _ _ = pass

-- | Apply FRP output to Brick state
applyFRPOutput :: FRPOutput -> EventM Name ChatState ()
applyFRPOutput output = do
  -- Check for halt first
  when (outShouldHalt output) halt

  -- Update mode
  currentMode .= outMode output

  -- Update focus
  focusedPanel .= outFocus output

   -- Handle editor commands
  case outEditorCmd output of
    Just EditorInsertNewline -> do
      Brick.zoom editor $ handleEditorEvent (VtyEvent (Vty.EvKey Vty.KEnter []))
    Just EditorClear -> do
      -- Get current text before clearing for message creation
      currentEditor <- use editor
      let editorContent = getEditContents currentEditor
          inputText = T.unlines editorContent
      -- Only add messages if there's actual content
      unless (T.null $ T.strip inputText) $ do
        -- Create actual messages (replacing FRP placeholders)
        let userMessage = ChatMessage inputText UserMessage
            echoText = "Echo: " <> inputText
            echoMessage = ChatMessage echoText AIMessage
        -- Update history with real messages
        chatHistory .= echoMessage : userMessage : filter (not . isPending) (outHistory output)
       -- Clear editor
      editor .= editorText InputField Nothing ""
    Just EditorMoveCursorEnd -> do
      Brick.zoom editor $ handleEditorEvent (VtyEvent (Vty.EvKey Vty.KEnd []))
    Just (EditorForward vtyEvent) -> do
      Brick.zoom editor $ handleEditorEvent (VtyEvent vtyEvent)
    Nothing -> pass

  -- Handle scroll commands
  case outScrollCmd output of
    Just ScrollUp -> vScrollBy (viewportScroll HistoryViewport) (-1)
    Just ScrollDown -> vScrollBy (viewportScroll HistoryViewport) 1
    Just ScrollPageUp -> vScrollPage (viewportScroll HistoryViewport) Up
    Just ScrollPageDown -> vScrollPage (viewportScroll HistoryViewport) Brick.Down
    Just ScrollToEnd -> vScrollToEnd (viewportScroll HistoryViewport)
    Nothing -> pass

-- | Check if a message is a pending placeholder
isPending :: ChatMessage -> Bool
isPending msg = (msg ^. messageText) == "<<PENDING>>" || (msg ^. messageText) == "<<PENDING_ECHO>>"
