{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Main (main) where

--import FRP.PlayingWithBalls
import Control.Lens
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import FRP.Yampa
import Linear
import Prelude hiding (id, (.))
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.ViewPort as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import qualified System.Exit as System

type a ->> b = SF a b

data State = State
  { _pos :: !(V2 Float)
  , _vel :: !(V2 Float)
  }
  deriving Show

data World = World
  { _worldWire :: ReactHandle () State
  , _worldViewport :: Gloss.ViewPort
  , _worldState :: IORef State
  , _terminateRun :: Bool
  }

makeLenses ''State
makeLenses ''World

instance RealFloat a => VectorSpace (V2 a) a where
  zeroVector = Linear.zero
  (*^) = (Linear.*^)
  (^+^) = (Linear.^+^)
  dot v1 v2 = sum $ v1 * v2

fps :: Int
fps = 60

main :: IO ()
main = do
  stateRef <- newIORef $ State { _pos = zero, _vel = zero }
  rh <- reactInit (return ()) (actuate stateRef) stateEvolution
  world0 <- return $ set worldWire rh $ world stateRef
  Gloss.playIO disp Gloss.black fps world0 render handleEvent step
  where
    disp = Gloss.InWindow "one ball" winSize (0, 0)
    winSize = (512, 512)
    stateEvolution = simpleOrbit $ State { _pos = V2 1.0 0.0, _vel = V2 0.0 1.0 }
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

simpleOrbit :: State -> a ->> State
simpleOrbit st0 = proc _ -> do
  rec
    x <- iPre x0 -< x' :: V2 Float
    a <- arr (zero-) -< x :: V2 Float
    v <- imIntegral v0 -< a :: V2 Float
    x' <- imIntegral x0 -< v :: V2 Float
  returnA -< State{ _pos = x', _vel = v }
  where
    x0 = view pos st0 :: V2 Float
    v0 = view vel st0 :: V2 Float

pic :: State -> Gloss.Picture
pic State{..} = Gloss.pictures $
  [ Gloss.color bgColor $ Gloss.circle 1.0
  , ballPic _pos
  ]

bgColor :: Gloss.Color
bgColor = Gloss.greyN 0.7

ballRadius :: Float
ballRadius = 0.1

ballColor :: Gloss.Color
ballColor = Gloss.violet

ballPic :: V2 Float -> Gloss.Picture
ballPic (V2 x y) = Gloss.translate x y primitive
  where
    primitive = Gloss.scale ballRadius ballRadius $ circleFilled ballColor
    circleFilled color =
      Gloss.color color (Gloss.circleSolid 1)
      <> Gloss.color (border color) (Gloss.circle 1)

border :: Gloss.Color -> Gloss.Color
border color = Gloss.makeColor r g b 0.5
  where
    (r, g, b, _) = Gloss.rgbaOfColor $ Gloss.mixColors 0.5 0.5 color bgColor

viewPort :: (Int, Int) -> Gloss.ViewPort
viewPort (w, h) = Gloss.viewPortInit { Gloss.viewPortScale = scale }
  where
    scale = min scaleX scaleY
    scaleX = fromIntegral w / 4 / screenBoundX
    scaleY = fromIntegral h / 4 / screenBoundY

screenWidth :: Float
screenWidth = 2

screenHeight :: Float
screenHeight = 2

screenBoundX :: Float
screenBoundX = screenWidth / 2

screenBoundY :: Float
screenBoundY = screenHeight / 2

-- sample :: Show a => SF Double a -> IO ()
-- sample = reactimate
--   (return (1.0::Double))
--   (\_ -> return (0.01, Nothing))
--   (\_ x -> do { putStr ("\r" ++ show x) ; return False })

-- sampleOutput :: (a -> IO ()) -> SF Double a -> IO ()
-- sampleOutput outFn = reactimate
--   (return (1.0::Double))
--   (\_ -> return (0.01, Nothing))
--   (\_ x -> do { outFn x ; return False })
