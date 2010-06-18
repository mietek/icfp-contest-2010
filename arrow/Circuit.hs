{-# LANGUAGE Arrows #-}

module Circuit where

import Control.Arrow (returnA)
import Debug.Trace (trace)

import Filter


data Trit = T0 | T1 | T2
  deriving Eq

instance Show Trit where
  show T0 = "0"
  show T1 = "1"
  show T2 = "2"

type Gate = Filter (Trit, Trit) (Trit, Trit)
type Circuit = Filter Trit Trit


gate :: Gate
gate = lift g where
  g (T0, T0) = (T0, T2)
  g (T0, T1) = (T2, T2)
  g (T0, T2) = (T1, T2)
  g (T1, T0) = (T1, T2)
  g (T1, T1) = (T0, T0)
  g (T1, T2) = (T2, T1)
  g (T2, T0) = (T2, T2)
  g (T2, T1) = (T1, T1)
  g (T2, T2) = (T0, T0)


config1 :: Circuit
config1 = proc x -> do
  rec r <- delay T0 -< r'
      (y, r') <- gate -< (x, r)
  returnA -< y

config2 :: Circuit
config2 = proc x -> do
  rec r <- delay T0 -< r'
      (r', y) <- gate -< (x, r)
  returnA -< y

config3 :: Circuit
config3 = proc x -> do
  rec r <- delay T0 -< r'
      (y, r') <- gate -< (r, x)
  returnA -< y

config4 :: Circuit
config4 = proc x -> do
  rec r <- delay T0 -< r'
      (r', y) <- gate -< (r, x)
  returnA -< y


serverInput :: [Trit]
serverInput = [T0, T1, T2, T0, T2, T1, T0, T1, T2, T1, T0, T2, T0, T1, T2, T0, T2]


config1Output :: [Trit]
config1Output = [T0, T2, T1, T2, T0, T1, T1, T2, T1, T0, T0, T0, T0, T2, T1, T2, T0]

config2Output :: [Trit]
config2Output = [T2, T2, T1, T2, T0, T2, T2, T1, T0, T2, T2, T0, T2, T2, T1, T2, T0]

config3Output :: [Trit]
config3Output = [T0, T1, T2, T1, T0, T2, T2, T1, T2, T0, T0, T0, T0, T1, T2, T1, T0]

config4Output :: [Trit]
config4Output = [T2, T2, T0, T2, T2, T0, T2, T2, T0, T2, T2, T0, T2, T2, T0, T2, T2]


config1Test :: Bool
config1Test = run config1 serverInput == config1Output

config2Test :: Bool
config2Test = run config2 serverInput == config2Output

config3Test :: Bool
config3Test = run config3 serverInput == config3Output

config4Test :: Bool
config4Test = run config4 serverInput == config4Output
