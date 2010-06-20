module Types where

import Data.Array

data Trit = Z | O | T
zero = Z
z = Z
one = O
o = one
two = T
t = T

data Delay = Delay | NoDelay deriving (Show, Eq)
data Conn  = L | R deriving (Show, Eq)
data Wire  = External | GateConn Int Conn Delay deriving (Show, Eq)
data Gate  = Gate (Wire, Wire) (Wire, Wire) deriving (Show, Eq)
data Circuit = Circuit
    { cGates  :: Array Int Gate
    , cInput  :: Wire
    , cOutput :: Wire
    } deriving (Show, Eq)

showWire External = "X"
showWire (GateConn i c _) = show i ++ show c
showGate (Gate (iL,iR) (oL,oR)) = showWire iL ++ showWire iR ++ "0#" ++ showWire oL ++ showWire oR ++ ",\n"
showCircuit (Circuit cg ci co) = showWire ci ++ ":\n" ++ body ++ showWire co ++ "\n"
    where body = reverse .  ("\n:" ++)  . drop 2 . reverse . concatMap ( showGate . (cg!)) $ [0..(snd $ bounds cg)]