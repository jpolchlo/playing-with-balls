{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module FRP.PlayingWithBalls
    ( State (..)
    , PrivateState (..)
    , PublicState
    , Located (location)
    , simpleOrbit
    , sample
    , sampleOutput
    , multiballEvolution
    , multiballAccel
    , zeroAccel
    , orbitOriginAccel
    ) where

import Control.Lens
import Data.List (mapAccumL)
import FRP.Yampa hiding (norm)
import FRP.Yampa.Switches (parC, parZ)
import Linear

type a ->> b = SF a b

instance RealFloat a => VectorSpace (V2 a) a where
  zeroVector = Linear.zero
  (*^) = (Linear.*^)
  (^+^) = (Linear.^+^)
  dot v1 v2 = sum $ v1 * v2

class Located a where
  location :: a -> V2 Float

data State = State
  { _pos :: !(V2 Float)
  , _vel :: !(V2 Float)
  }
  deriving Show

makeLenses ''State

instance Located State where
  location = view pos

data PrivateState = PrivateState
  { _mass :: !(Float)
  , _position  :: !(V2 Float)
  , _velocity  :: !(V2 Float)
  }
  deriving Show

makeLenses ''PrivateState

instance Located PrivateState where
  location = view position

type PublicState = (Float, V2 Float)

observeState :: PrivateState -> PublicState
observeState st = (view mass st, view position st)

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

multiballEvolution :: [PrivateState] -> ((PrivateState, [PublicState]) ->> V2 Float)
  -> a ->> [PrivateState]
multiballEvolution sts0 accel = proc _ -> do
  rec
    sts <- iPre sts0 -< sts'
    ctxts <- arr withContext -< sts
    accels <- parC accel -< ctxts
    vs <- parZ velocityIntegrals -< accels
    xs <- parZ positionIntegrals -< vs
    xvs <- arr $ (\(xl,vl) -> zip3 ms0 xl vl) -< (xs, vs)
    sts' <- arr $ map (\(m,x,v) -> PrivateState { _mass = m, _position = x, _velocity = v }) -< xvs
  returnA -< sts'
  where
    all1Strips lst = (snd $ mapAccumL (\seen el -> (el:seen, seen)) [] lst) `zwf` scanr (:) [] lst
    zwf = zipWith (\a b -> a ++ tail b)
    allPubsFrom = map (map observeState) . all1Strips
    withContext priv = zip priv $ allPubsFrom priv
    xs0 = map (view position) sts0
    vs0 = map (view velocity) sts0
    ms0 = map (view mass) sts0
    velocityIntegrals = map imIntegral vs0
    positionIntegrals = map imIntegral xs0

gravity :: Float -> PublicState -> PublicState -> V2 Float
gravity g (_, x0) (m1, x1) = accel #*^ diff
  where
    (#*^) = (Linear.*^)
    diff = x1 - x0
    accel = g * m1 / norm diff ** 2

multiballAccel :: Float -> (PrivateState, [PublicState]) ->> V2 Float
multiballAccel g = proc (me, them) -> do
  let
    accel = foldl (+) (V2 0 0) $ map (gravity g $ observeState me) them
  returnA -< accel

zeroAccel :: (PrivateState, [PublicState]) ->> V2 Float
zeroAccel = proc _ -> do
  returnA -< zero

orbitOriginAccel :: (PrivateState, [PublicState]) ->> V2 Float
orbitOriginAccel = proc (me, _) -> do
  arr ((zero-) . view position) -< me

sample :: Show a => SF Double a -> IO ()
sample = reactimate
  (return (1.0::Double))
  (\_ -> return (0.01, Nothing))
  (\_ x -> do { putStr ("\r" ++ show x) ; return False })

sampleOutput :: (a -> IO ()) -> SF Double a -> IO ()
sampleOutput outFn = reactimate
  (return (1.0::Double))
  (\_ -> return (0.01, Nothing))
  (\_ x -> do { outFn x ; return False })
