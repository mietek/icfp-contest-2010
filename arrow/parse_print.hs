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

zmienNotacje5 xs = (a,b,c,d,e)
  where (a,b,c,d,e, f) = zmienNotacje xs
  
sumujKrotki (a,b,c,d,e,f) (a',b',c',d',e',f') = (a+a',b+b',c+c',d+d',e+e',f+f')

sumujPary [] = []
sumujPary ((a,b,c):xs) = ((sumujKrotki (zmienNotacje a) (zmienNotacje c)) :) $ sumujPary xs

sumujWsio xs = foldr sumujKrotki (0,0,0,0,0,0) zs
  where zs = sumujPary xs
  
czySzesc xs = a>0
  where (_,_,_,_,_,a) = sumujWsio xs


sprawdzRownanie ((a,b,c,d,e,f), Aux,(a',b',c',d',e',f')) (v1,v2,v3, v4, v5, v6) = v1^a+v2^b+v3^c+v4^d+v5^e+v6^f >= v1^a'+v2^b'+v3^c'+v4^d'+v5^e'+v6^f'
sprawdzRownanie ((a,b,c,d,e,f), Main,(a',b',c',d',e',f')) (v1,v2,v3, v4, v5, v6) = v1^a+v2^b+v3^c+v4^d+v5^e+v6^f > v1^a'+v2^b'+v3^c'+v4^d'+v5^e'+v6^f'

sprawdzRownanie5 ((a,b,c,d,e), Aux,(a',b',c',d',e')) (v1,v2,v3, v4, v5) = v1^a+v2^b+v3^c+v4^d+v5^e >= v1^a'+v2^b'+v3^c'+v4^d'+v5^e'
sprawdzRownanie5 ((a,b,c,d,e), Main,(a',b',c',d',e')) (v1,v2,v3, v4, v5) = v1^a+v2^b+v3^c+v4^d+v5^e > v1^a'+v2^b'+v3^c'+v4^d'+v5^e'


sprawdzRownania [] _ = True
sprawdzRownania ((l,t,p):rs) wartosciowanie = if (sprawdzRownanie ((zmienNotacje l), t,(zmienNotacje p)) wartosciowanie) then (sprawdzRownania rs wartosciowanie) else False

sprawdzRownania5 [] _ = True
sprawdzRownania5 ((l,t,p):rs) wartosciowanie = if (sprawdzRownanie5 ((zmienNotacje5 l), t,(zmienNotacje5 p)) wartosciowanie) then (sprawdzRownania5 rs wartosciowanie) else False


doListy (v1, v2, v3, v4, v5, v6) = printFuel [[[v1]],[[v2]],[[v3]],[[v4]],[[v5]],[[v6]]]

doListy5 (v1, v2, v3, v4, v5) = printFuel [[[v1]],[[v2]],[[v3]],[[v4]],[[v5]]]

parse6 nr w e = if ([] == rozw)
       then putStr ("nierozwiazane:" ++ (show nr) ++ "," ++ w)
       else putStr ((show nr)++ "," ++ (doListy (head rozw)))
  where rozw = [ (v1,v2,v3,v4,v5,v6) | v6 <- [1..30], v5 <- [1..30], v3 <- [1..30], v4 <- [1..30], v2 <- [1..30], v1 <- [1..30], (sprawdzRownania e (v1,v2,v3,v4,v5,v6))]

parse5 nr w e = if ([] == rozw)
       then putStr ("nierozwiazane:" ++ (show nr) ++ "," ++ w)
       else putStr ((show nr)++ "," ++ (doListy5 (head rozw)))
  where rozw = [ (v1,v2,v3,v4,v5) | v5 <- [1..32], v4 <- [1..32], v3 <- [1..64], v2 <- [1..64], v1 <- [1..64], sprawdzRownania5 e (v1,v2,v3,v4,v5)]


parseE1 (nr,w) = if (czySzesc e) then parse6 nr w e  else parse5 nr w e
  where (e, s) = parseEngine w

main = parseE1 (1, "122000010")
  
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


car ="2210101022010102201111102211111001122100101102211110110"