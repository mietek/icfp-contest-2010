{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Text.ParserCombinators.Parsec
import Data.Char
import Control.Applicative hiding ((<|>), many)
import Array

data Delay = Delay | NoDelay deriving Show
data Wire  = External | GateConn Int Delay deriving Show
data Gate  = Gate (Wire, Wire) (Wire, Wire) deriving Show
data Circuit = Circuit
    { cGates  :: Array Int Gate
    , cInput  :: Wire
    , cOutput :: Wire
    } deriving Show

whiteSpace = choice [ char ' '
                    , char '\n'
                    , char '\t'
                    ]

skipWhiteSpace = many whiteSpace

pDelay =  (char 'L' >> return Delay)
      <|> (char 'R' >> return NoDelay)

pNumber = foldl foo 0 <$> many1 digit
    where foo x y = 10*x + digitToInt y

pWire =  (char 'X' >> return External)
     <|> GateConn <$> pNumber <*> pDelay

pGate = do
  leftInput <- pWire
  rightInput <- pWire
  skipWhiteSpace >> char '0' >> skipWhiteSpace >> char '#' >> skipWhiteSpace
  leftOutput <- pWire
  rightOutput <- pWire
  return $ Gate (leftInput,rightInput) (leftOutput, rightOutput)

pGates = do
  gates <- pGate `sepBy1` (char ',' >> skipWhiteSpace)
  let len = length gates
  return $ listArray (0,len-1) gates

pCircuit = do
  inp <- pWire
  char ':' >> skipWhiteSpace
  gates <- pGates
  char ':' >> skipWhiteSpace
  out <- pWire
  return $ Circuit gates inp out

parseCircuit s = parse pCircuit "" s

main :: IO ()
main = print . parseCircuit =<< getContents
