module Circuit where

import Data.Char (isDigit)
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M


data Side = L | R
  deriving (Eq, Ord, Show)

data Plug = X | G Integer Side
  deriving (Eq, Ord, Show)

type Wire = (Plug, Plug)
type Gate = (Plug, Plug, Plug, Plug)

data Circuit = C {
    cSrc :: Map Plug Plug,
    cDst :: Map Plug Plug
  } deriving (Eq, Ord, Show)


readInteger :: String -> (Integer, String)
readInteger = head . reads

readSide :: String -> (Side, String)
readSide ('L' : cs) = (L, cs)
readSide ('R' : cs) = (R, cs)
readSide cs = error ("Expected side, got " ++ show cs)

readPlug :: String -> (Plug, String)
readPlug ('X' : cs) = (X, cs)
readPlug (d : cs) | isDigit d = (G n s, cs2)
  where (n, cs1) = readInteger (d : cs)
        (s, cs2) = readSide cs1
readPlug cs = error ("Expected plug, got " ++ show cs)

readGate :: String -> Gate
readGate cs =
  let (li, cs1) = readPlug cs in
  case readPlug cs1 of
    (ri, '0' : '#' : cs2) ->
      let (lo, cs3) = readPlug cs2 in
      case readPlug cs3 of
        (ro, cs4) | cs4 == "," || cs4 == ":" -> (li, ri, lo, ro)
        (_, cs4) -> error ("Expected terminator, got " ++ show cs4)      
    (_, cs2) -> error ("Expected separator, got " ++ show cs2)

readHeader :: String -> Plug
readHeader cs =
  case readPlug cs of
    (xo, ":") -> xo
    (_, cs1) -> error ("Expected separator, got " ++ show cs1)

readFooter :: String -> Plug
readFooter cs =
  case readPlug cs of
    (xi, "") -> xi
    (_, cs1) -> error ("Expected nothing, got " ++ show cs1)

readCircuit :: String -> Circuit
readCircuit cs = c3
  where (l : ls) = lines (filter (/= ' ') cs)
        c = circuit
        c1 = addWire c (X, readHeader l)
        c2 = foldl addGate c1 (zip [0 ..] (map readGate (init ls)))
        c3 = addWire c2 (readFooter (last ls), X)
        () = verifyCircuit c3


verifyGateAt :: Circuit -> Integer -> ()
verifyGateAt c n =
  if not (hasWireTo c (G n L))
    then error ("No wire to " ++ show (G n L))
    else if not (hasWireTo c (G n R))
      then error ("No wire to " ++ show (G n R))
      else if not (hasWireFrom c (G n L))
        then error ("No wire from " ++ show (G n L))
        else if not (hasWireFrom c (G n R))
          then error ("No wire from " ++ show (G n R))
          else ()

verifyCircuit :: Circuit -> ()
verifyCircuit c =
  if not (hasWireFrom c X)
    then error "No wire from X"
    else if not (hasWireTo c X)
      then error "No wire to X"
      else const () (map (verifyGateAt c) [0 .. maxGateN c])


showPlug :: Plug -> String
showPlug X = "X"
showPlug (G n s) = show n ++ show s

showGate :: Gate -> String
showGate (li, ri, lo, ro) = concat ss
  where ss = [showPlug li, showPlug ri, "0#", showPlug lo, showPlug ro]

showHeader :: Plug -> String
showHeader p = showPlug p ++ ":"

showFooter :: Plug -> String
showFooter p = showPlug p

showCircuit :: Circuit -> String
showCircuit c = concat (intersperse "\n" ls)
  where gs = [showGate (gateAt c n) | n <- [0 .. maxGateN c]]
        gs1 = map (++ ",") (init gs) ++ [last gs ++ ":"]
        ls = [showHeader (wireFrom c X)] ++ gs1 ++ [showFooter (wireTo c X)]


circuit :: Circuit
circuit = C { cSrc = M.empty, cDst = M.empty }

addWire :: Circuit -> Wire -> Circuit
addWire c (i, o) =
  if conflict i o (cSrc c)
    then error ("Source " ++ show i ++ " already connected to destination " ++ show (cSrc c M.! i) ++ ", not " ++ show o)
    else if conflict o i (cDst c)
           then error ("Destination " ++ show o ++ " already connected to source " ++ show (cDst c M.! o) ++ ", not " ++ show o)
           else C { cSrc = M.insert i o (cSrc c), cDst = M.insert o i (cDst c) }

addGate :: Circuit -> (Integer, Gate) -> Circuit
addGate c (n, (li, ri, lo, ro)) = c4
  where c1 = addWire c (li, (G n L))
        c2 = addWire c1 (ri, (G n R))
        c3 = addWire c2 ((G n L), lo)
        c4 = addWire c3 ((G n R), ro)


wireFrom :: Circuit -> Plug -> Plug
wireFrom c i = cSrc c M.! i

wireTo :: Circuit -> Plug -> Plug
wireTo c o = cDst c M.! o

hasWireFrom :: Circuit -> Plug -> Bool
hasWireFrom c i = M.member i (cSrc c)

hasWireTo :: Circuit -> Plug -> Bool
hasWireTo c o = M.member o (cDst c)

gateAt :: Circuit -> Integer -> Gate
gateAt c n = (wireTo c (G n L), wireTo c (G n R), wireFrom c (G n L), wireFrom c (G n R))

gateN :: Plug -> Integer
gateN X = error "External gate has no number"
gateN (G n _) = n

maxGateN :: Circuit -> Integer
maxGateN c = maximum ns
  where ns = map gateN (filter (/= X) (concat [M.keys (cSrc c), M.elems (cSrc c), M.keys (cDst c), M.elems (cDst c)]))


conflict :: (Ord k, Eq a) => k -> a -> Map k a -> Bool
conflict k a m = M.member k m && m M.! k /= a
