{-# LANGUAGE Arrows #-}

module Clean where

import Control.Arrow (returnA)
import Filter hiding (step, run)
import qualified Filter as F


data Trit = T0 | T1 | T2
  deriving (Enum, Eq)

instance Num Trit where
  (+) = toTrit (+)
  (*) = toTrit (*)
  (-) = toTrit (-)
  abs = id
  signum = const 0
  fromInteger 0 = T0
  fromInteger 1 = T1
  fromInteger 2 = T2
  fromInteger n = fromInteger (n `mod` 3)

toTrit :: (Enum a) => (Int -> Int -> Int) -> a -> a -> a
toTrit f x y  = toEnum $ (fromEnum x `f` fromEnum y) `mod` 3

instance Show Trit where
  show = show . fromEnum

readAll :: String -> [Trit]
readAll = map (fromIntegral . read . return)

showAll :: [Trit] -> String
showAll = concatMap show

run :: Circuit -> String -> String
run c s = concatMap show (F.run c (readAll s))


input  = "01202101210201202"
output = "11021210112101221"


type Gate = Filter (Trit, Trit) (Trit, Trit)
type Circuit = Filter Trit Trit


gate :: Gate
gate = lift g where
  g (x, y) = (x - y, x * y - 1)

c02 :: Circuit
c02 = proc x -> do
  rec r <- delay 0 -< r'
      (r', y) <- gate -< (x, r)
  returnA -< y

c20 :: Circuit
c20 = proc x -> do
  rec r <- delay 0 -< r'
      (a, b)  <- gate -< (x, r)
      (y, r') <- gate -< (a, b)
  returnA -< y

