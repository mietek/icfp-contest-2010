module Main where

import Data.List
import System.IO

pow3s = map (3^) [0..]

dig :: Char -> Int
dig '0' = 0
dig '1' = 1
dig '2' = 2

tern :: String -> Int -> Int
tern [] _ = 0
tern (x:xs) n = (dig x * pow3s !! n) + tern xs (n-1)

parseNum :: String -> (Int, String)
parseNum ('0':xs) = (0, xs)
parseNum ('1':x:xs) = (dig x + 1, xs)
parseNum ('2':'2':xs) = (sum (take nlen pow3s) + fnum, sec)
  where (len, rest) = parseNum xs
        nlen = len+2
        fnum = tern (take nlen rest) (nlen-1)
        sec = drop nlen rest

findRest n k (x:xs)
  | k < x = (n, k)
  | otherwise = findRest (n+1) (k-x) xs

type Pipe = [Int]
data Mode = Main | Aux deriving Show
type Cham = (Pipe, Mode, Pipe)
type Eng  = [Cham]

takeN :: Int -> (String -> (a, String)) -> String -> ([a], String)
takeN 0 _ s = ([], s)
takeN n f s = (fs:rs, xs)
  where (fs, tl) = f s
        (rs, xs) = takeN (n-1) f tl

parsePipe :: String -> (Pipe, String)
parsePipe ('0':xs) = ([], xs)
parsePipe ('1':xs) = takeN 1 parseNum xs
parsePipe ('2':'2':xs) = takeN (n+2) parseNum rst
  where (n, rst) = parseNum xs

parseCham :: String -> (Cham, String)
parseCham s = ((up, mode, lp), t3)
  where (up, t1) = parsePipe s
        (n, t2)  = parseNum t1
        (lp, t3) = parsePipe t2
        mode = case n of
          0 -> Main
          1 -> Aux

parseEngine :: String -> (Eng, String)
parseEngine ('0':xs) = ([], xs)
parseEngine ('1':xs) = takeN 1 parseCham xs
parseEngine ('2':'2':xs) = takeN (n+2) parseCham rst
  where (n, rst) = parseNum xs

type Int6 = (Int,Int,Int,Int,Int,Int)
type Int5 = (Int,Int,Int,Int,Int)


mnozMacierze (a,b,c,d) (a',b',c',d') = (a*a'+b*c',a*b'+b*d',c*a'+d*c',c*b'+d*d')

mnozListe = foldr mnozMacierze (1,0,0,1)

sumujWsio xs = foldr sumujKrotki (0,0,0,0,0,0) zs
  where zs = sumujPary xs

czyCztery xs = a+b+c==0
  where (_,_,_,c,b,a) = sumujWsio xs


zamien (v, _, _) 0 = v
zamien (_, v, _) 1 = v
zamien (_, _, v) 2 = v
  
slabowieksze (a, b, c, d) (a', b', c', d') = a>=a' && b>=b && c>= c' && d>=d'

wieksze (a, b, c, d) (a', b', c', d') = a>a' && b>=b && c>= c' && d>=d'
  
sprawdzRownanie (l, Aux, r) v = slabowieksze (mnozListe (map (zamien v) l)) (mnozListe (map (zamien v) r))

sprawdzRownanie (l, Main, r) v = wieksze w1 w2 
  where w1 = mnozListe (map (zamien v) l)
        w2 = mnozListe (map (zamien v) r)

sprawdzRownania [] _ = True
sprawdzRownania (r:rs) wartosciowanie = if (sprawdzRownanie r wartosciowanie) then (sprawdzRownania rs wartosciowanie) else False

parse6 nr w e = if ([] == rozw)
       then putStr ("nierozwiazane:" ++ (show nr) ++ "," ++ w ++ "\n")
       else putStr ((show nr)++ "," ++ (show (head rozw)) ++ "\n")
  where rozw = [((v11, v12,v13, v14),(v21, v22, v23, v24),(v31, v32, v33, v34)) | v34 <- [0..1], v24 <- [0..1], v14 <- [0..1], v33 <- [0..2], v32 <- [0..2], v31 <- [1..4], v23 <- [0..4],v22 <- [0..4],v12 <- [0..6],v13 <- [0..6],v21 <- [1..6],v11 <- [1..6],(sprawdzRownania e ((v11, v12,v13, v14),(v21, v22, v23, v24),(v31, v32, v33, v34)))]

parseE1 :: (Int, String) -> IO ()
parseE1 (nr,w) = (if (czyCztery e) then parse6 nr w e  else putStr ((show nr) ++ " - za duza macierz.\n"))
   >>
  (hFlush stdout)
  where (e, s) = parseEngine w


main = (map read . lines) `fmap` readFile "cars_list" >>= mapM_ parseE1


printNum :: Int -> String
printNum k = case ngr of
  0 -> "0"
  1 -> '1':prsuf suf 1
  _ -> '2':'2':printNum (ngr-2)++prsuf suf ngr
  where (ngr, suf) = findRest 0 k pow3s

prsuf 0 k = take k ['0','0'..]
prsuf n k = prsuf t (k-1) ++ case u of
  0 -> "0"
  1 -> "1"
  2 -> "2"
  where t = n `div` 3
        u = n `mod` 3

printList :: [Int] -> String
printList [] = "0"
printList [x] = '1':printNum x
printList xss = "22" ++ printNum (length xss - 2) ++ concatMap printNum xss

printMat :: [[Int]] -> String
printMat [] = "0"
printMat [x] = '1':printList x
printMat xss = "22" ++ printNum (length xss - 2) ++ concatMap printList xss

printFuel :: [[[Int]]] -> String
printFuel [] = "0"
printFuel [x] = '1':printMat x
printFuel xss = "22" ++ printNum (length xss - 2) ++ concatMap printMat xss

zmienNotacje :: [Int] -> Int6
zmienNotacje [] = (0, 0, 0, 0, 0, 0)
zmienNotacje (0:xs) = (a+1, b, c, d, e, f)
  where (a,b,c,d,e,f) = zmienNotacje xs
zmienNotacje (1:xs) = (a, b+1, c, d, e, f)
  where (a,b,c,d,e,f) = zmienNotacje xs
zmienNotacje (2:xs) = (a, b, c+1, d, e, f)
  where (a,b,c,d,e,f) = zmienNotacje xs
zmienNotacje (3:xs) = (a, b, c, d+1, e, f)
  where (a,b,c,d,e,f) = zmienNotacje xs
zmienNotacje (4:xs) = (a, b, c, d, e+1, f)
  where (a,b,c,d,e,f) = zmienNotacje xs
zmienNotacje (5:xs) = (a, b, c, d, e, f+1)
  where (a,b,c,d,e,f) = zmienNotacje xs

zmienNotacje5 :: [Int] -> Int5
zmienNotacje5 xs = (a,b,c,d,e)
  where (a,b,c,d,e, f) = zmienNotacje xs

sumujKrotki :: Int6 -> Int6 -> Int6
sumujKrotki (a,b,c,d,e,f) (a',b',c',d',e',f') = (a+a',b+b',c+c',d+d',e+e',f+f')

sumujPary [] = []
sumujPary ((a,b,c):xs) = ((sumujKrotki (zmienNotacje a) (zmienNotacje c)) :) $ sumujPary xs