{-# LANGUAGE Arrows #-}

module Circuit where

import Control.Arrow (returnA)
import Debug.Trace (trace)

import Filter hiding (step, run)
import qualified Filter as F


data Trit = T0 | T1 | T2
  deriving (Enum, Eq)

instance Show Trit where
  show = show . fromEnum

instance Read Trit where
  readsPrec _ "0" = [(T0, "")]
  readsPrec _ "1" = [(T1, "")]
  readsPrec _ "2" = [(T2, "")]


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


run :: Circuit -> String -> String
run c s = concatMap show (F.run c (map (read . return) s))


input = "01202101210201202"
output1 = "02120112100002120"
output2 = "22120221022022120"
output3 = "01210221200001210"
output4 = "22022022022022022"
test1 = run config1 input == output1
test2 = run config2 input == output2
test3 = run config3 input == output3
test4 = run config4 input == output4
