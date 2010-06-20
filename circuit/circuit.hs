{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where


import System.Environment
import System.Exit
import Data.List
import System.Environment (getArgs)
import Data.Array
import Types
import Parser

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
    where delayedWires = nub $ filter delayed (leftOutputs ++ rightOutputs)
          (leftOutputs, rightOutputs) = unzip $ map (\(Gate out _) -> out) gates
          delayed (GateConn _ _ Delay) = True
          delayed _ = False
          delayWire (GateConn n c _) = prWire n c ++ "' <- delay T0 -< " ++ prWire n c

printGates c = unlines $ intro ++ body ++ outro
    where gates = elems $ cGates c
          body = map ("      "++) (delayWires gates ++ zipWith prGate gates [0..])
          intro = ["foo = proc inp -> do", "  rec"]
          outro = ["  returnA -< out"]


showTernaryStream = map f
    where f Z = '0'
          f O = '1'
          f T = '2'

evalCircuit :: Circuit -> [Trit] -> [Trit]
evalCircuit = undefined

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
