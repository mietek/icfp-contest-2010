{-# LANGUAGE NoMonomorphismRestriction #-}
module Parser where

import Text.ParserCombinators.Parsec
import Data.Char
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Data.Array
import Types

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
