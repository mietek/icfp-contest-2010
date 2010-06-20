{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Text.ParserCombinators.Parsec
import Data.Char
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Array
import System.Environment
import System.Exit
import Data.List
import System.Environment (getArgs)

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

emptyGate = Gate (External,External) (External,External)

arraySize t = 1 + snd (bounds t)

shiftCircuit n c = Circuit (shiftGates n $ cGates c) (shiftWire n $ cInput c) (shiftWire n $ cOutput c)
shiftGates n gs = array (0,n + arraySize gs - 1) $ [(i+n, shiftGate n (gs ! i)) | i <- [0..arraySize gs - 1]]
                                                    ++ [(i,emptyGate) | i <- [0..n-1]]
shiftGate n (Gate (a,b) (c,d)) = Gate (shiftWire n a, shiftWire n b) (shiftWire n c, shiftWire n d)
shiftWire _ External = External
shiftWire k (GateConn n c d) = GateConn (n + k) c d


combineCircuits :: Circuit -> Circuit -> Circuit
combineCircuits c1 c2 = Circuit gates3 inp out
    where
      Circuit gates1 inp (GateConn outN1 outC1 _) = c1
      Gate outC1inp (outC1L, outC1R)  = gates1 ! outN1
      out1' = Gate outC1inp $ if outC1 == L then (inpC2shifted, outC1R) else (outC1L, inpC2shifted)
      gates1' = gates1 // [(outN1, out1')]
      shift = arraySize gates1
      inpC2shifted = cInput $ shiftCircuit shift c2
      Circuit gates2 (GateConn inpN2 inpC2 _) out = shiftCircuit shift c2
      Gate (inpC2L, inpC2R) outC2out = gates2 ! inpN2
      inp2' = Gate (if inpC2 == L then (cOutput c1, inpC2R) else (inpC2L, cOutput c1)) outC2out
      gates2' = gates2 // [(inpN2, inp2')]
      finalSize = arraySize gates1 + arraySize gates2' - 1
      gates3 = gates2' // [(i,gates1' ! i) | i <- [0..arraySize gates1' - 1]]

appendFoo2t c = Circuit gates2 inp foo2tGR
    where
      Circuit gates1 inp out@(GateConn outN outC _) = c
      Gate outC1inp (outC1L, outC1R)  = gates1 ! outN
      out1' = Gate outC1inp $ if outC == L then (foo2tGR, outC1R) else (outC1L, foo2tGR)
      gates2 = array (0,foo2tN) $ (assocs (gates1 // [(outN,out1')])) ++ [(foo2tN,foo2tGate)]
      foo2tGate = Gate (foo2tGL,out) (foo2tGL,External)
      foo2tN = arraySize gates1
      foo2tGR = GateConn foo2tN R Delay
      foo2tGL = GateConn foo2tN L Delay
appendFoo1t c = Circuit gates2 foo1tGL foo1tGL
    where
      Circuit gates1 inp@(GateConn inN inC _ ) out@(GateConn outN outC _) = c
      Gate outC1inp (outC1L, outC1R)  = gates1 ! outN
      Gate (inC1L,inC1R) inC1out = gates1 ! inN
      out1' = Gate outC1inp $ if outC == L then (foo1tGR, outC1R) else (outC1L, foo1tGR)
      in1' = Gate (if inC == L then (foo1tGR, inC1R) else (inC1L, foo1tGR)) inC1out
      gates1' = gates1 // [(inN,in1'),(outN,out1')]
      gates2 = array (0,foo1tN) $ assocs gates1' ++ [(foo1tN,foo1tGate)]
      foo1tN = arraySize gates1
      foo1tGate = Gate (External,out) (External,inp)
      foo1tGR = GateConn foo1tN R Delay
      foo1tGL = GateConn foo1tN L Delay


foo = either (error "f**k handcraft") id $ parseCircuit "1L:\n1L2R0#X2R,\nX2L0#0L2L,\n1R0R0#1R0R:\n0L\n"
foo2t = either (error "f**k handcraft") id  $ parseCircuit "0L:\n0LX0#0LX:\n0L\n"
foo1t = either (error "f**k handcraft") id $ parseCircuit "0L:\nX0R0#X0R:\n0L\n"
x ^^^ 1 = x
x ^^^ k = combineCircuits x (x^^^(k-1))
p1 m 0 = appendFoo2t $ foo ^^^ m
p1 m k = (appendFoo2t $foo ^^^ (m-k))  `combineCircuits` (foo ^^^ k)


-- czyste lenistwo
mehInt '0' = 0
mehInt '1' = 1
mehInt '2' = 2
mehStr 0 = '0'
mehStr 1 = '1'
mehStr 2 = '2'

dodajChar x y = mehStr $ ((mehInt x) + (mehInt y)) `mod` 3
ujmijChar x y = mehStr $ ((mehInt x) - (mehInt y)) `mod` 3

-- dodawanie ciagow
dodaj [] [] = []
dodaj (c1:cs) (c2:cs2) = (dodajChar c1 c2): (dodaj cs cs2)

-- wyszukiwanie potrzebnych bramek jesli tylko dodajemy
szukaj _ [] [] _ = []
szukaj m (x:xs) (y:ys) k =  gs ++ (szukaj m zs ys (k+1))
    where gs = generuj m x y k
          zs = foldl dodaj xs (map (drop (k+1)) gs)

generuj m x y k = take (mehInt (ujmijChar y x)) [ ciag01 m k | x <- [0..]]

znajdz m x y = szukaj m x y 0

korekta m x y =  map sum $ map (map mehInt) $ znajdz m x y
wejscia m x y = map (m-) (korekta m x y)

ciag01 m k= (take  k ['0' | x <- [0..]]) ++ (take (m-k) ['1' | x <- [1..]])

serverInput = "01202101210201202"
taskOutput = "11021210112101221"


genDodaj m k = appendFoo1t $ p1 m k
compGenDodaj m (x:xs) = foldl (\c y-> c `combineCircuits` genDodaj m y) c1 xs
    where c1 = genDodaj m x

compFooDodaj m k inps = (foo ^^^ k) `combineCircuits` (compGenDodaj m inps)
mkFactory str = showCircuit $  compFooDodaj l l $ wejscia l (replicate l '0') (taskOutput ++ str)
    where l = length str + (length taskOutput)

isTrit :: Char -> Bool
isTrit '0' = True
isTrit '1' = True
isTrit '2' = True
isTrit _ = False

debug = mapM_ print . zip [1..] .  lines . showCircuit

main :: IO ()
main = putStrLn . mkFactory . filter isTrit . head =<< getArgs
 {-do
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
-}