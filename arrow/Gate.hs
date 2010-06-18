{-# LANGUAGE Arrows #-}

module Gate where

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


mkGate :: ((Trit, Trit) -> (Trit, Trit)) -> Gate
mkGate f = lift f

mkConfig1 :: Gate -> Circuit
mkConfig1 g = proc x -> do
  rec r <- delay T0 -< r'
      (y, r') <- g -< (x, r)
  returnA -< y

mkConfig2 :: Gate -> Circuit
mkConfig2 g = proc x -> do
  rec r <- delay T0 -< r'
      (r', y) <- g -< (x, r)
  returnA -< y

mkConfig3 :: Gate -> Circuit
mkConfig3 g = proc x -> do
  rec r <- delay T0 -< r'
      (y, r') <- g -< (r, x)
  returnA -< y

mkConfig4 :: Gate -> Circuit
mkConfig4 g = proc x -> do
  rec r <- delay T0 -< r'
      (r', y) <- g -< (r, x)
  returnA -< y


finrod :: (Trit, Trit) -> (Trit, Trit)
finrod (l, r) =
  trace ("<<" ++ show l ++ " " ++ show r ++ ">>") $
    case (l, r) of
      (T0, T0) -> (T0, T2)
      (T0, T1) -> (T2, T2)
      (T0, T2) -> (T1, T2)
      (T1, T0) -> (T1, T2)
      (T1, T1) -> (T0, T0)
      (T1, T2) -> (T2, T1)
      (T2, T0) -> (T2, T2)
      (T2, T1) -> (T1, T1)
      (T2, T2) -> (T0, T0)

input :: [Trit]
input = [T0, T1, T2, T0, T2, T1, T0, T1, T2, T1, T0, T2, T0, T1, T2, T0, T2]

output1 :: [Trit]
output1 = [T0, T2, T1, T2, T0, T1, T1, T2, T1, T0, T0, T0, T0, T2, T1, T2, T0]

output2 :: [Trit]
output2 = [T2, T2, T1, T2, T0, T2, T2, T1, T0, T2, T2, T0, T2, T2, T1, T2, T0]

output3 :: [Trit]
output3 = [T0, T1, T2, T1, T0, T2, T2, T1, T2, T0, T0, T0, T0, T1, T2, T1, T0]

output4 :: [Trit]
output4 = [T2, T2, T0, T2, T2, T0, T2, T2, T0, T2, T2, T0, T2, T2, T0, T2, T2]

test1 :: [Trit]
test1 =
  let g = mkGate finrod
      c1 = mkConfig1 g
  in run c1 input

test2 :: [Trit]
test2 =
  let g = mkGate finrod
      c2 = mkConfig2 g
  in run c2 input

test3 :: [Trit]
test3 =
  let g = mkGate finrod
      c3 = mkConfig3 g
  in run c3 input

test4 :: [Trit]
test4 =
  let g = mkGate finrod
      c4 = mkConfig4 g
  in run c4 input
