module Main where
import Data.List
import System.Environment (getArgs)
import Types
import Data.Array
import Parser (parseCircuit)

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


-- daje bramke x na koniec fabryki, bramka przejmuje cale io
-- x mowi do wejscia c i na swiat
-- x slucha z wyjscia c i na swiat
-- X?L0#X?L
appendAsdf c = Circuit gates2 foo1tGL foo1tGL
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

-- ?LX0#X?L
appendAsdf2 c = Circuit gates2 foo1tGR foo1tGL
    where
      Circuit gates1 inp@(GateConn inN inC _ ) out@(GateConn outN outC _) = c
      Gate outC1inp (outC1L, outC1R)  = gates1 ! outN
      Gate (inC1L,inC1R) inC1out = gates1 ! inN
      out1' = Gate outC1inp $ if outC == L then (foo1tGL, outC1R) else (outC1L, foo1tGL)
      in1' = Gate (if inC == L then (foo1tGR, inC1R) else (inC1L, foo1tGR)) inC1out
      gates1' = gates1 // [(inN,in1'),(outN,out1')]
      gates2 = array (0,foo1tN) $ assocs gates1' ++ [(foo1tN,foo1tGate)]
      foo1tN = arraySize gates1
      foo1tGate = Gate (out,External) (External,inp)
      foo1tGR = GateConn foo1tN R Delay
      foo1tGL = GateConn foo1tN L Delay

--foo = either (error "f**k handcraft") id $ parseCircuit "1L:\n1L2R0#X2R,\nX2L0#0L2L,\n1R0R0#1R0R:\n0L\n"
foo = Circuit {cGates = array (0,2) [(0,Gate (GateConn 1 L Delay,GateConn 2 R Delay) (External,GateConn 2 R NoDelay)),(1,Gate (External,GateConn 2 L Delay) (GateConn 0 L Delay,GateConn 2 L NoDelay)),(2,Gate (GateConn 1 R NoDelay,GateConn 0 R NoDelay) (GateConn 1 R Delay,GateConn 0 R Delay))], cInput = GateConn 1 L NoDelay, cOutput = GateConn 0 L NoDelay}
foo2t = Circuit {cGates = array (0,0) [(0,Gate (GateConn 0 L Delay,External) (GateConn 0 L Delay,External))], cInput = GateConn 0 R NoDelay, cOutput = GateConn 0 R NoDelay}
foo1t = Circuit {cGates = array (0,0) [(0,Gate (External,GateConn 0 R Delay) (External,GateConn 0 R Delay))], cInput = GateConn 0 L NoDelay, cOutput = GateConn 0 L NoDelay}
finrod = Circuit {cGates = array (0,5) [(0,Gate (GateConn 2 R Delay,External) (GateConn 1 R NoDelay,GateConn 1 L NoDelay)),(1,Gate (GateConn 0 R NoDelay,GateConn 0 L NoDelay) (GateConn 2 L NoDelay,GateConn 2 R NoDelay)),(2,Gate (GateConn 1 L NoDelay,GateConn 1 R NoDelay) (GateConn 3 R NoDelay,GateConn 0 L Delay)),(3,Gate (GateConn 5 R Delay,GateConn 2 L NoDelay) (GateConn 4 R NoDelay,GateConn 4 L NoDelay)),(4,Gate (GateConn 3 R NoDelay,GateConn 3 L NoDelay) (GateConn 5 R NoDelay,GateConn 5 L NoDelay)),(5,Gate (GateConn 4 R NoDelay,GateConn 4 L NoDelay) (External,GateConn 3 L Delay))], cInput = GateConn 0 R NoDelay, cOutput = GateConn 5 L NoDelay}

--finrod = either (error "f**k handcraft") id . parseCircuit $ "0R:\n2RX0#1R1L,\n0R0L0#2L2R,\n1L1R0#3R0L,\n5R2L0#4R4L,\n3R3L0#5R5L,\n4R4L0#X3L:\n5L"

--foo2t = either (error "f**k handcraft") id  $ parseCircuit "0R:\n0LX0#0LX:\n0R\n"
--foo1t = either (error "f**k handcraft") id $ parseCircuit "0L:\nX0R0#X0R:\n0L\n"

x ^^^ 1 = x
x ^^^ k = combineCircuits x (x^^^(k-1))
--p1 m 0 =  foo ^^^ m `combineCircuits` foo2t
--p1 m k = (foo ^^^ (m-k)) `combineCircuits` foo2t `combineCircuits` (foo ^^^ k)
p1 m 0 = finrod `combineCircuits` foo2t
p1 m k = finrod `combineCircuits` foo2t `combineCircuits` (foo ^^^ k)

-- finroda gadzet pierwszy
-- map (2-) | g1
g1 =  appendAsdf2 (finrod `combineCircuits` foo2t)
-- finroda gadzet drugi
g2 =  appendAsdf2 (finrod `combineCircuits` foo1t)
g0 =  appendAsdf2 finrod


s12 '0' = '0'
s12 '1' = '2'
s12 '2' = '1'

s02 '0' = '2'
s02 '1' = '1'
s02 '2' = '0'

s01 '0' = '1'
s01 '1' = '0'
s01 '2' = '2'

data Tok = G0 -- 0* z lewej
            | G1 -- 2* z lewej
            | G2 -- 01* z lewej
            | D Int
            deriving Show

factory :: String -> [Tok]
factory [] = []
factory inp@('1':xs) = G0 : factory (map s12 inp)
factory inp@('2':xs) = G1 : factory (map s02 inp)
factory ('0':'1':xs) = G2 : factory ('0' : '0' : map s01 xs)
factory ('0':xs) = aux 1 xs
  where aux n [] = [D n]
        aux n ('0':xs) = aux (n+1) xs
        aux n ('1':xs) = if n > 1 then D (n-1):factory ('0':'1':xs) else factory ('0':'1':xs)
        aux n ('2':xs) = D n : factory ('2':xs)

-- data Tok = G0 | G1 | G2 | D Int
fromTok G0 = g0
fromTok G1 = g1
fromTok G2 = g2
fromTok (D n) = foo ^^^ n

finishIt =  foldl combineCircuits finrod . map fromTok
{-
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
-}
serverInput = "01202101210201202"
taskOutput = "11021210112101221"

{-
genDodaj m k = appendAsdf $ p1 m k
compGenDodaj m (x:xs) = foldl (\c y-> c `combineCircuits` genDodaj m y) c1 xs
    where c1 = genDodaj m x
-
compFooDodaj m k inps = {-(foo ^^^ k)-} finrod `combineCircuits` (compGenDodaj m inps)
-}
mkFactory = showCircuit . finishIt . factory . (taskOutput ++)
--compFooDodaj l l $ wejscia l (replicate l '0') (taskOutput ++ str)
 --   where l = length str + (length taskOutput)

isTrit :: Char -> Bool
isTrit '0' = True
isTrit '1' = True
isTrit '2' = True
isTrit _ = False

debug = mapM_ print . zip [0..] .  lines . showCircuit
--genPrefix file = writeFile file $ showCircuit $ compGenDodaj 17 $ wejscia 17 serverInput taskOutput

main :: IO ()
main = putStrLn . mkFactory . filter isTrit . head =<< getArgs