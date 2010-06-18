{-# LANGUAGE Arrows #-}

module Gate where

import Control.Arrow (returnA)

import Filter


data Trit = T0 | T1 | T2

instance Show Trit where
  show T0 = "0"
  show T1 = "1"
  show T2 = "2"

type Gate = Filter (Trit, Trit) (Trit, Trit)
type Circuit = Filter Trit Trit


mkGate :: ((Trit, Trit) -> (Trit, Trit)) -> Gate
mkGate f = lift f

mkConfig1 :: Gate -> Circuit
mkConfig1 g = proc x -> do
  rec (y, r) <- g -< (x, r')
      r' <- delay T0 -< r
  returnA -< y

mkConfig2 :: Gate -> Circuit
mkConfig2 g = proc x -> do
  rec (r, y) <- g -< (x, r')
      r' <- delay T0 -< r
  returnA -< y

mkConfig3 :: Gate -> Circuit
mkConfig3 g = proc x -> do
  rec (y, r) <- g -< (r', x)
      r' <- delay T0 -< r
  returnA -< y

mkConfig4 :: Gate -> Circuit
mkConfig4 g = proc x -> do
  rec (r, y) <- g -< (r', x)
      r' <- delay T0 -< r
  returnA -< y
