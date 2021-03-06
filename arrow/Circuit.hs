{-# LANGUAGE Arrows #-}

module Circuit where

import Control.Arrow (returnA)
import Control.Arrow
import qualified Control.Arrow as C
import Filter hiding (step, run)
import qualified Filter as F
import System.Environment (getArgs)

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

toTrit f x y  = toEnum $ (fromEnum x `f` fromEnum y) `mod` 3

instance Show Trit where
  show = show . fromEnum

instance Read Trit where
  readsPrec _ "0" = [(T0, "")]
  readsPrec _ "1" = [(T1, "")]
  readsPrec _ "2" = [(T2, "")]


type Gate = Filter (Trit, Trit) (Trit, Trit)
type Circuit = Filter Trit Trit


gate :: Gate
gate = lift $ \(x,y) -> (x - y, x * y -1)
  -- g (T0, T0) = (T0, T2)
  -- g (T0, T1) = (T2, T2)
  -- g (T0, T2) = (T1, T2)
  -- g (T1, T0) = (T1, T2)
  -- g (T1, T1) = (T0, T0)
  -- g (T1, T2) = (T2, T1)
  -- g (T2, T0) = (T2, T2)
  -- g (T2, T1) = (T1, T1)
  -- g (T2, T2) = (T0, T0)
{-  h (a, b) = let (x, y) = (fromEnum a, fromEnum b)
                 (c, d) = ((x - y) `mod` 3, (x * y - 1) `mod` 3) in
             (toEnum c, toEnum d)-}


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

mietek1 :: Circuit
mietek1 = proc x -> do
  rec r <- delay T0 -< r'
      (r', y) <- gate -< (r, x)
  returnA -< y

sample :: Circuit
sample = proc xili19 -> do
  rec ro9li2  <- delay T0 -< ro9li2'
      ro10ri2 <- delay T0 -< ro10ri2'
      ro11ri6 <- delay T0 -< ro11ri6'
      ro12li0 <- delay T0 -< ro12li0'
      ro13ri0 <- delay T0 -< ro13ri0'
      ro14li1 <- delay T0 -< ro14li1'
      ro15li4 <- delay T0 -< ro15li4'
      ro16ri7 <- delay T0 -< ro16ri7'
      ro17ri3 <- delay T0 -< ro17ri3'
      ro18ri5 <- delay T0 -< ro18ri5'
      ro19li7 <- delay T0 -< ro19li7'
      (lo0ri1,   ro0ri12)  <- gate -< (ro12li0,  ro13ri0)
      (lo1ri4,   ro1li9)   <- gate -< (ro14li1,  lo0ri1)
      (lo2li3,   ro2li8)   <- gate -< (ro9li2,   ro10ri2)
      (lo3li5,   ro3ri9)   <- gate -< (lo2li3,   ro17ri3)
      (lo4ri10,  ro4ri13)  <- gate -< (ro15li4,  lo1ri4)
      (lo5li6,   ro5li15)  <- gate -< (lo3li5,   ro18ri5)
      (lo6li13,  ro6li12)  <- gate -< (lo5li6,   ro11ri6)
      (lo7ri11,  ro7ri8)   <- gate -< (ro19li7,  ro16ri7)
      (lo8li11,  ro8li10)  <- gate -< (ro2li8,   ro7ri8)
      (lo9li18,  ro9li2')  <- gate -< (ro1li9,   ro3ri9)
      (lo10li16, ro10ri2') <- gate -< (ro8li10,  lo4ri10)
      (lo11ri15, ro11ri6') <- gate -< (lo8li11,  lo7ri11)
      (lo12li14, ro12li0') <- gate -< (ro6li12,  ro0ri12)
      (lo13ri14, ro13ri0') <- gate -< (lo6li13,  ro4ri13)
      (lo14li17, ro14li1') <- gate -< (lo12li14, lo13ri14)
      (lo15ri16, ro15li4') <- gate -< (ro5li15,  lo11ri15)
      (lo16ri17, ro16ri7') <- gate -< (lo10li16, lo15ri16)
      (lo17ri18, ro17ri3') <- gate -< (lo14li17, lo16ri17)
      (lo18ri19, ro18ri5') <- gate -< (lo9li18,  lo17ri18)
      (lo19xo,   ro19li7') <- gate -< (xili19,   lo18ri19)
  returnA -< lo19xo


run :: Circuit -> String -> String
run c s = concatMap show (F.run c (map (read . return) s))


serverInput   = "01202101210201202"
serverOutput1 = "02120112100002120"
serverOutput2 = "22120221022022120"
serverOutput3 = "01210221200001210"
serverOutput4 = "22022022022022022"
testServer1 = run config1 serverInput == serverOutput1
testServer2 = run config2 serverInput == serverOutput2
testServer3 = run config3 serverInput == serverOutput3
testServer4 = run config4 serverInput == serverOutput4

taskInput  = "02222220210110011"
taskOutput = "11021210112101221"
taskInputq  = "00000000000000000"
testTask = run sample taskInput == taskOutput

arcs :: Trit -> Trit -> [((Trit, Trit), (Trit, Trit))]
arcs T0 T0 = [((T0, T0), (T0, T2))]
arcs T0 T1 = [((T0, T2), (T1, T2)),
              ((T1, T0), (T1, T2))]
arcs T0 T2 = [((T0, T0), (T0, T2)),
              ((T0, T1), (T2, T2)),
              ((T0, T2), (T1, T2)),
              ((T1, T0), (T1, T2)),
              ((T2, T0), (T2, T2))]
arcs T1 T0 = [((T1, T1), (T0, T0))]
arcs T1 T1 = [((T1, T0), (T1, T2)),
              ((T1, T2), (T2, T1)),
              ((T2, T1), (T1, T1))]
arcs T1 T2 = [((T0, T1), (T2, T2)),
              ((T1, T0), (T1, T2)),
              ((T1, T2), (T2, T1))]
arcs T2 T0 = [((T2, T2), (T0, T0))]
arcs T2 T1 = [((T0, T2), (T1, T2)),
              ((T1, T2), (T2, T1)),
              ((T2, T1), (T1, T1))]
arcs T2 T2 = [((T0, T2), (T1, T2)),
              ((T1, T2), (T2, T1)),
              ((T2, T0), (T2, T2))]

-- kod jami

x ^^^ 1 = x
x ^^^ k = x >>> (x^^^(k-1))

-- przesun o 1
foo = proc inp -> do
  rec
      rL0' <- delay T0 -< rL0
      rR1' <- delay T0 -< rR1
      rR0' <- delay T0 -< rR0
      (out, rR2) <- gate -< (rL0', rR0')
      (rL0, rL2) <- gate -< (inp, rR1')
      (rR0, rR1) <- gate -< (rL2, rR2)
  returnA -< out
-- 02*
foo1t = proc inp -> do
  rec
      rR0' <- delay T0 -< rR0
      (out, rR0) <- gate -< (inp, rR0')
  returnA -< out

foo2t = proc inp -> do
  rec
      rR0' <- delay T0 -< rR0
      (rR0, out) <- gate -< (rR0', inp)
  returnA -< out

finrod= proc inp -> do
  rec
      rR2' <- delay T0 -< rR2
      rR5' <- delay T0 -< rR5
      (rL0, rR0) <- gate -< (rR2', inp)
      (rL1, rR1) <- gate -< (rR0, rL0)
      (rL2, rR2) <- gate -< (rL1, rR1)
      (rL3, rR3) <- gate -< (rR5', rL2)
      (rL4, rR4) <- gate -< (rR3, rL3)
      (out, rR5) <- gate -< (rR4, rL4)
  returnA -< out

g1 = proc inp -> do
  rec
r




