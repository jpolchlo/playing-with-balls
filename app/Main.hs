{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Main (main) where

--import FRP.PlayingWithMyBalls
import Control.Lens
import Control.Monad (liftM)
import Control.Wire
import qualified Data.Set as Set
import Linear
import Prelude hiding (id, (.))
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.ViewPort as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import qualified System.Exit as System

type a ->> b = Wire (Timed Float ()) () Identity a b

data State = State
  { _pos :: !(V2 Float)
  , _vel :: !(V2 Float)
  }

data World = World
  { _worldWire :: () ->> Gloss.Picture
  , _worldViewport :: Gloss.ViewPort
  , _worldPic :: Gloss.Picture
  , _terminateRun :: Bool
  }

makeLenses ''State
makeLenses ''World

fps :: Int
fps = 60

main :: IO ()
main = Gloss.playIO disp Gloss.black fps world render handleEvent step
  where
    disp = Gloss.InWindow "spanout" winSize (0, 0)
    winSize = (512, 512)
    world = World
      { _worldWire     = simulate $ State { _pos = V2 1.0 0.0, _vel = V2 0.1 1.1 }
      , _worldPic      = Gloss.blank
      , _worldViewport = viewPort winSize
      , _terminateRun  = False
      }

render :: World -> IO Gloss.Picture
render world = return $ Gloss.applyViewPortToPicture vp pic
  where
    vp = view worldViewport world
    pic = view worldPic world

handleEvent :: Gloss.Event -> World -> IO World
handleEvent (Gloss.EventResize wh) world =
  return $ set worldViewport (viewPort wh) world
handleEvent (Gloss.EventKey (Gloss.Char 'q') Gloss.Up _ _) world =
  return $ set terminateRun True world
handleEvent _ world = return world

step :: Float -> World -> IO World
step delta world = case view terminateRun world of
  False -> return $ set worldWire wire' $ set worldPic pic world
  True  -> System.exitSuccess
  where
    timed = Timed delta ()
    input = Right ()
    mb = stepWire (view worldWire world) timed input
    (Right pic, wire') = runIdentity mb

simulate :: State -> a ->> Gloss.Picture
simulate st0 = select $ proc _ -> do
  rec
    prevX <- delay $ view pos st0 -< currX
    prevV <- delay $ view vel st0 -< currV
    currX <- accum (\dt x v -> x + dt *^ v) $ view pos st0 -< prevV
    currV <- accum (\dt v a -> v + dt *^ (normalize a)) $ view vel st0 -< -currX
  let st' = State { _pos = currX, _vel = currV }
  returnA -< Right $ pic st'

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

-- keyPressed :: Gloss.Key -> a ->> Bool
-- keyPressed key = constM $ views envKeys (Set.member key)

accum :: HasTime t s => (t -> b -> a -> b) -> b -> Wire s e m a b
accum f b = mkSF $ \s a ->
  let b' = f (dtime s) b a
  in  (b', accum f b')

constM :: Monad m => m b -> Wire s e m a b
constM m = mkGen_ . const $ liftM Right m

select :: (Monoid s, Monad m)
  => Wire s e m a (Either (Wire s e m a b) b) -> Wire s e m a b
select w = mkGen $ \s a -> do
  (eb, w') <- stepWire w s (Right a)
  case eb of
    Right (Left w'') -> stepWire w'' mempty (Right a)
    Right (Right b)  -> return (Right b, select w')
    Left e           -> return (Left e, select w')
