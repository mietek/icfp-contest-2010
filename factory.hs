
s12 '0' = '0'
s12 '1' = '2'
s12 '2' = '1'

s02 '0' = '2'
s02 '1' = '1'
s02 '2' = '0'

s01 '0' = '1'
s01 '1' = '0'
s01 '2' = '2'

data Gadget = G0 -- 0* z lewej
            | G1 -- 01* z lewej
            | G2 -- 2* z lewej
            | Delay Int
            deriving Show

factory :: String -> [Gadget]
factory [] = []
factory inp@('1':xs) = G0 : factory (map s12 inp)
factory inp@('2':xs) = G2 : factory (map s02 inp)
factory ('0':'1':xs) = G1 : factory ('0' : '0' : map s01 xs)
factory ('0':xs) = aux 1 xs
  where aux n [] = [Delay n]
        aux n ('0':xs) = aux (n+1) xs
        aux n ('1':xs) = if n > 1 then Delay (n-1):factory ('0':'1':xs) else factory ('0':'1':xs)
        aux n ('2':xs) = Delay n : factory ('2':xs)