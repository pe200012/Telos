{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Telos.TUI.BananaMain
-- Description : Reactive-banana integration for Brick TUI
-- License     : BSD-3-Clause
--
-- This module is derived from bricki-banana by Lennart Spitzner
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Original: https://github.com/lspitzner/bricki-banana
--
-- Provides a reactive-banana interface to brick that bypasses Brick's
-- event loop entirely, giving full FRP control over the UI.
module Telos.TUI.BananaMain
  ( brickNetwork,
    Next (..),
    brickNetworkNoFix,
  )
where

import Brick.AttrMap (AttrMap)
import Brick.Types
  ( CursorLocation (..),
    Widget,
    locationColumnL,
    locationRowL,
  )
import Brick.Types.Internal (RenderState (..))
import Brick.Widgets.Internal (renderFinal)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan (readTChan, writeTChan)
import Control.Exception (finally)
import Control.Monad.Fix (mfix)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Graphics.Vty
  ( Cursor (..),
    Event (..),
    Picture (..),
    Vty,
    defaultConfig,
    displayBounds,
    inputIface,
    outputIface,
    shutdown,
    update,
  )
import Graphics.Vty.Input (eventChannel)
import Graphics.Vty.Input.Events (InternalEvent(..))
import Graphics.Vty.CrossPlatform (mkVty)
import Control.Lens ((^.))
import qualified Reactive.Banana as Banana
import qualified Reactive.Banana.Frameworks as Banana
import Relude hiding ((<>))

-- | Signal to brick: redraw or halt
data Next
  = Redraw
  | Halt
  deriving stock (Eq, Show)

instance Semigroup Next where
  Redraw <> Redraw = Redraw
  _ <> _ = Halt

instance Monoid Next where
  mempty = Redraw

-- | Main interface of the reactive-banana interface to brick.
--
-- This interface is designed in such a way that you will most likely have to
-- make use of the MonadFix instance of 'MomentIO' one way or another
-- (@mdo@\/@do rec@\/@mfix@), because one of the return values of @brickNetwork@
-- is the input event that most likely determines the content of the widgets
-- to draw.
--
-- All 'Event's other than the one in 'Maybe Event' are reactive-banana Events.
-- The remaining one is vty's input event type.
--
-- For the startup event, the handler must be provided as well because it
-- is used internally, too. The handler must be called once by the user.
brickNetwork ::
  forall n a.
  (Ord n) =>
  -- | startup event and handler. handler must be fired once at program
  --  start to get things in motion (after actuating the network).
  (Banana.AddHandler (), Banana.Handler ()) ->
  -- | signal brick to either redraw or halt.
  Banana.Event Next ->
  -- | behaviour that contains the widget(s) to draw when redrawing
  Banana.Behavior [Widget n] ->
  -- | behaviour that contains the cursor selection function
  Banana.Behavior
    ([CursorLocation n] -> Maybe (CursorLocation n)) ->
  -- | behaviour that contains the attribute-map
  Banana.Behavior AttrMap ->
  -- | returns three results:
  --  1) A (reactive-banana) event containing any (vty) input events
  --  2) An event that fires once when shutdown of the brick interface is
  --   completed
  --  3) a function that can be used to implement suspension of the brick
  --   ui to run some external commands etc.
  Banana.MomentIO
    ( Banana.Event (Maybe Event),
      Banana.Event (),
      ( Banana.Event (IO a) ->
        Banana.MomentIO (Banana.Event a)
      )
    )
brickNetwork (startupAH, startupH) triggerE widgetB cursorB attrB = do
  let initialRS :: RenderState n
      initialRS = RS M.empty [] S.empty mempty [] S.empty M.empty

  (eventEvent, eventH) <- Banana.newEvent
  (shutdownEvent, shutdownH) <- Banana.newEvent
  startupEvent <- Banana.fromAddHandler startupAH
  (redrawE, redrawH) <- Banana.newEvent
  (suspendE, suspendH) <- Banana.newEvent

  Banana.reactimate $ (void . forkIO) <$> suspendE

  let suspendSetup ::
        forall b. Banana.Event (IO b) -> Banana.MomentIO (Banana.Event b)
      suspendSetup ioE = do
        (resultE, resultH) <- Banana.newEvent
        Banana.reactimate
          $ ioE
          <&> \io ->
            void $ suspendH $ ((io >>= resultH) `finally` startupH ())
        pure resultE

  initState <- mfix $ \initState -> do
    let e1 =
          startupEvent <&> \() -> liftIO $ do
            vty <- mkVty defaultConfig
            haltIORef <- newIORef False
            let loop = do
                  internalEv <- atomically $ readTChan $ eventChannel $ inputIface vty
                  shouldHalt <- readIORef haltIORef
                  unless shouldHalt $ do
                    case internalEv of
                      ResumeAfterInterrupt -> loop  -- Ignore resume events
                      InputEvent ev -> do
                        case ev of
                          (EvResize _ _) ->
                            eventH
                              . Just
                              . (\(w, h) -> EvResize w h)
                              =<< (displayBounds $ outputIface vty)
                          _ -> eventH $ Just ev
                        loop
            void $ forkIO loop
            void $ forkIO $ eventH Nothing
            let stopper = do
                  writeIORef haltIORef True
                  void
                    $ forkIO
                    $ atomically
                    $ writeTChan
                      (eventChannel $ inputIface vty)
                      (error "shutdown signal")
                  shutdown vty
            pure $ pure $ Just (vty, stopper)
    let e2 =
          flip Banana.apply suspendE
            $ initState
            <&> \mState _ ->
              liftIO $ do
                forM_ mState snd
                pure $ pure Nothing
        e3 =
          Banana.filterJust
            $ triggerE
            <&> \case
              Redraw -> Nothing
              Halt -> Just $ pure $ pure Nothing
    eB <-
      Banana.execute
        $ Banana.unionWith
          (error "brick internal error: simultaneous startup/suspend")
          e1
          (Banana.unionWith const e2 e3)
    Banana.switchB (pure Nothing) eB

  Banana.reactimate
    $ flip Banana.apply triggerE
    $ initState
    <&> \mState -> \case
      Redraw -> redrawH ()
      Halt -> do
        forM_ mState snd
        shutdownH ()

  rsRef <- liftIO $ newIORef initialRS

  let redrawF ::
        Maybe (Vty, IO ()) ->
        [Widget n] ->
        ([CursorLocation n] -> Maybe (CursorLocation n)) ->
        AttrMap ->
        IO ()
      redrawF mState widgetStack chooseCursor attrs = do
        case mState of
          Nothing -> pass
          Just (vty, _) -> do
            renderState <- readIORef rsRef
            (renderState', _exts) <-
              render
                vty
                widgetStack
                chooseCursor
                attrs
                renderState
            writeIORef rsRef renderState'

  Banana.reactimate
    $ redrawF
    <$> initState
    <*> widgetB
    <*> cursorB
    <*> attrB
    Banana.<@ redrawE

  pure (eventEvent, shutdownEvent, suspendSetup)

-- | Alternative interface that doesn't require MonadFix usage.
brickNetworkNoFix ::
  forall a n.
  (Ord n) =>
  (Banana.AddHandler (), Banana.Handler ()) ->
  ( Banana.Event (Maybe Event) ->
    Banana.Event () ->
    (Banana.Event (IO a) -> Banana.MomentIO (Banana.Event a)) ->
    Banana.MomentIO
      ( Banana.Event Next,
        Banana.Behavior [Widget n],
        Banana.Behavior
          ([CursorLocation n] -> Maybe (CursorLocation n)),
        Banana.Behavior AttrMap
      )
  ) ->
  Banana.MomentIO ()
brickNetworkNoFix (startupAH, startupH) interfaceF = mdo
  (triggerE, widgetB, cursorB, attrB) <-
    interfaceF
      eventEvent
      shutdownEvent
      suspendSetup
  (eventEvent, shutdownEvent, suspendSetup) <-
    brickNetwork
      (startupAH, startupH)
      triggerE
      widgetB
      cursorB
      attrB
  pass

-- | Render the UI
render ::
  Ord n =>
  Vty ->
  [Widget n] ->
  ([CursorLocation n] -> Maybe (CursorLocation n)) ->
  AttrMap ->
  RenderState n ->
  IO (RenderState n, [a])
render vty widgetStack chooseCursor attrMapCur rs = do
  sz <- displayBounds $ outputIface vty
  let (newRS, pic, theCursor, _exts) =
        renderFinal attrMapCur widgetStack sz chooseCursor rs
      picWithCursor = case theCursor of
        Nothing -> pic {picCursor = NoCursor}
        Just loc ->
          pic
            { picCursor =
                AbsoluteCursor
                  (loc ^. locationColumnL)
                  (loc ^. locationRowL)
            }

  update vty picWithCursor

  pure (newRS, [])
