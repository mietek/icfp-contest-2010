import Data.List

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


