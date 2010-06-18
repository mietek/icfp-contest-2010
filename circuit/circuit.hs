{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Text.ParserCombinators.Parsec
import Data.Char
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Array
import System.Environment
import System.Exit

data Trit = Z | O | T
zero = Z
z = Z
one = O
o = one
two = T
t = T

data Delay = Delay | NoDelay deriving Show
data Conn  = L | R deriving Show
data Wire  = External | GateConn Int Conn Delay deriving Show
data Gate  = Gate (Wire, Wire) (Wire, Wire) deriving Show
data Circuit = Circuit
    { cGates  :: Array Int Gate
    , cInput  :: Wire
    , cOutput :: Wire
    } deriving Show

prWire n c = concat ["r", show c, show n]

prInWire External = "inp"
prInWire (GateConn n c d) = prWire n c ++ primeDelay d

prOutWire External _ _ = "out"
prOutWire _ c i = prWire i c

primeDelay Delay = "'"
primeDelay _ = ""

prGate (Gate (li,ri)(lo,ro)) i = concat [ "(", prOutWire lo L i, ", ", prOutWire ro R i
                                        , ") <- gate -< ("
                                        , prInWire li, ", ", prInWire ri, ")"
                                        ]

delayWires gates = map delayWire delayedWires
    where delayedWires = filter delayed (leftOutputs ++ rightOutputs)
          (leftOutputs, rightOutputs) = unzip $ map (\(Gate out _) -> out) gates
          delayed (GateConn _ _ Delay) = True
          delayed _ = False
          delayWire (GateConn n c _) = prWire n c ++ "' <- delay T0 -< " ++ prWire n c

printGates c = unlines $ intro ++ body ++ outro
    where gates = elems $ cGates c
          body = map ("      "++) (delayWires gates ++ zipWith prGate gates [0..])
          intro = ["foo = proc inp -> do", "  rec"]
          outro = ["  returnA -< out"]

whiteSpace = choice [ char ' '
                    , char '\n'
                    , char '\t'
                    ]

skipWhiteSpace = many whiteSpace

pConn =  (char 'L' >> return L)
     <|> (char 'R' >> return R)

pNumber = foldl foo 0 `liftM` many1 digit
    where foo x y = 10*x + digitToInt y

pWire defaultDelay =  (char 'X' >> return External)
                  <|> (liftM3 GateConn pNumber pConn $ return defaultDelay)

pGate = do
  let pWire' = pWire $ error "default, tmp delay"
  leftInput <- pWire'
  rightInput <- pWire'
  skipWhiteSpace >> char '0' >> skipWhiteSpace >> char '#' >> skipWhiteSpace
  leftOutput <- pWire'
  rightOutput <- pWire'
  return $ Gate (leftInput,rightInput) (leftOutput, rightOutput)

setDelays :: [Gate] -> [Gate]
setDelays = zipWith fixGate [0..]
    where fixGate i (Gate (lI,rI) (lO,rO)) = Gate (fixInpWire i lI, fixInpWire i rI) (fixOutWire i lO, fixOutWire i rO)
          fixInpWire _ External = External
          fixInpWire i (GateConn n conn _) =
              let delay = if n < i then -- jestesmy w i-tej bramce, dostajemy kablem z n-tej
                              NoDelay
                          else
                              Delay
              in GateConn n conn delay
          fixOutWire i External = External
          fixOutWire i (GateConn n conn _) =
              let delay = if n <= i then -- jestesmy w i-tej bramce, wypuszczamy kabel do n-tej
                              Delay
                          else
                              NoDelay
              in GateConn n conn delay

pGates = do
  gates <- pGate `sepBy1` (char ',' >> skipWhiteSpace)
  let len    = length gates
      gates' = setDelays gates
  return $ listArray (0,len-1) gates'

pCircuit = do
  inp <- pWire NoDelay -- ??
  char ':' >> skipWhiteSpace
  gates <- pGates
  char ':' >> skipWhiteSpace
  out <- pWire NoDelay -- ??
  return $ Circuit gates inp out

parseCircuit s = parse pCircuit "" s

parseTernaryStream input = aux input []
    where aux [] acc     = Just acc
          aux (x:xs) acc = case x of
                             '0' -> aux xs $ z : acc
                             '1' -> aux xs $ o : acc
                             '2' -> aux xs $ t : acc
                             _ -> Nothing

showTernaryStream = map f
    where f Z = '0'
          f O = '1'
          f T = '2'

evalCircuit :: Circuit -> [Trit] -> [Trit]
evalCircuit = undefined

main :: IO ()
main = do
  args <- getArgs
  case args of
    [factorySchemaFile] -> do
        schema <- readFile factorySchemaFile
        case parseCircuit schema of
          Left e -> print e >> exitFailure
          Right factory -> putStrLn $ printGates factory
              -- do
              -- rawInput <- getContents
              -- case parseTernaryStream rawInput of
              --   Nothing -> putStrLn "broken input" >> exitFailure
              --   Just input -> putStrLn $ showTernaryStream $ evalCircuit factory input
    _ -> putStrLn "usage: ./circuit shema.txt" >> exitFailure
