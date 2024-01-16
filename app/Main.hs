{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Lens
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import FRP.PlayingWithBalls
import FRP.Yampa
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.ViewPort as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import Linear
import qualified System.Exit as System

import Graphics

data World = World
  { _worldWire :: ReactHandle () [PrivateState]
  , _worldViewport :: Gloss.ViewPort
  , _worldState :: IORef [PrivateState]
  , _terminateRun :: Bool
  }

makeLenses ''World

main :: IO ()
main = do
  stateRef <- newIORef $ []
  rh <- reactInit (return ()) (actuate stateRef) stateEvolution
  world0 <- return $ set worldWire rh $ world stateRef
  Gloss.playIO disp Gloss.black fps world0 render handleEvent step
  where
    fps = 60
    disp = Gloss.InWindow "one ball" winSize (0, 0)
    winSize = (512, 512)
    stateEvolution = multiballEvolution
      [ PrivateState { _position = V2 1 0, _velocity = V2 0 1, _mass = 1.0 }
      , PrivateState { _position = V2 (-1) 0, _velocity = V2 0 (-1.5), _mass = 1.0 }
      , PrivateState { _position = V2 0 0, _velocity = V2 0 0, _mass = 20.0 }
      ] (multiballAccel 0.1)
    actuate ref _ _ st = do
      writeIORef ref st
      return True
    world ref = World
      { _worldWire     = undefined
      , _worldState    = ref
      , _worldViewport = viewPort winSize
      , _terminateRun  = False
      }

render :: World -> IO Gloss.Picture
render world = do
  state <- readIORef $ view worldState world
  return $ Gloss.applyViewPortToPicture vp $ pic state
  where
    vp = view worldViewport world

handleEvent :: Gloss.Event -> World -> IO World
handleEvent (Gloss.EventResize wh) world =
  return $ set worldViewport (viewPort wh) world
handleEvent (Gloss.EventKey (Gloss.Char 'q') Gloss.Up _ _) world =
  return $ set terminateRun True world
handleEvent _ world = return world

step :: Float -> World -> IO World
step delta world = case view terminateRun world of
  False -> do
    _ <- react rh increment
    return world
  True  -> System.exitSuccess
  where
    rh = view worldWire world
    increment = (realToFrac delta, Nothing)
