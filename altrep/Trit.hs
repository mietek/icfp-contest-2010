module Trit where


data Trit = T0 | T1 | T2
  deriving (Enum, Eq, Ord)

instance Num Trit where
  (+) = viaEnum (+)
  (*) = viaEnum (*)
  (-) = viaEnum (-)
  abs = id
  signum = const 0
  fromInteger 0 = T0
  fromInteger 1 = T1
  fromInteger 2 = T2
  fromInteger n = error ("Expected trit, got " ++ show n)

instance Show Trit where
  show T0 = "0"
  show T1 = "1"
  show T2 = "2"

instance Read Trit where
  readsPrec _ "0" = [(T0, "")]
  readsPrec _ "1" = [(T1, "")]
  readsPrec _ "2" = [(T2, "")]
  readsPrec _ cs = error ("Expected trit, got " ++ show cs)


viaEnum :: (Int -> Int -> Int) -> Trit -> Trit -> Trit
viaEnum f x y  = toEnum (f (fromEnum x) (fromEnum y) `mod` 3)

gate :: (Trit, Trit) -> (Trit, Trit)
gate (x, y) = (x - y, x * y - 1)
