{-# LANGUAGE RecordWildCards #-}

module Graphics
  ( pic
  , viewPort
  ) where

import FRP.PlayingWithBalls
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.ViewPort as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import Linear

pic :: Located a => [a] -> Gloss.Picture
pic xs = Gloss.pictures $
  (Gloss.color bgColor $ Gloss.circle 1.0) : map (ballPic . location) xs

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
