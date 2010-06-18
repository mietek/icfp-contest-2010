module Filter where

import qualified Control.Arrow as A
import qualified Control.Category as C


newtype Filter a b = F (a -> (b, Filter a b))

instance Show (Filter a b) where
  show _ = "Filter"

instance C.Category Filter where
  id     = pass
  (.)    = flip join

instance A.Arrow Filter where
  arr    = lift
  first  = first
  second = second
  (***)  = split
  (&&&)  = fanout

instance A.ArrowLoop Filter where
  loop   = loop


lift :: (a -> b) -> Filter a b
lift pf = F $ \a ->
  (pf a, lift pf)

pass :: Filter a a
pass = lift id

join :: Filter a b -> Filter b c -> Filter a c
join (F f1) (F f2) = F $ \a ->
  let (b, f1') = f1 a
      (c, f2') = f2 b in
  (c, join f1' f2')

first :: Filter a b -> Filter (a, c) (b, c)
first (F f) = F $ \(a, c) ->
  let (b, f') = f a in
  ((b, c), first f')

second :: Filter a b -> Filter (c, a) (c, b)
second (F f) = F $ \(c, a) ->
  let (b, f') = f a in
  ((c, b), second f')

split :: Filter a b -> Filter c d -> Filter (a, c) (b, d)
split (F f1) (F f2) = F $ \(a, c) ->
  let (b, f1') = f1 a
      (d, f2') = f2 c in
  ((b, d), split f1' f2')

fanout :: Filter a b -> Filter a c -> Filter a (b, c)
fanout (F f1) (F f2) = F $ \a ->
  let (b, f1') = f1 a
      (c, f2') = f2 a in
  ((b, c), fanout f1' f2')

delay :: a -> Filter a a
delay a0 = F $ \a ->
  (a0, delay a)

loop :: Filter (a, c) (b, c) -> Filter a b
loop (F f) = F $ \a ->
  let ((b, c), f') = f (a, c) in
  (b, loop f')


step :: Filter a b -> a -> (b, Filter a b)
step (F f) a = f a

run :: Filter a a -> [a] -> ([a], Filter a a)
run f [] = ([], f)
run f (a : as) =
  let (b, f') = step f a
      (bs, f'') = run f' as in
  (b : bs, f'')
