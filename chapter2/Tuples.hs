module Tuples where
import Prelude hiding (fst, snd)

fst :: (a,b,c) -> a
fst (x,_,_) = x

snd :: (a,b,c) -> b
snd (_,y,_) = y

thrd :: (a,b,c) -> c
thrd (_,_,z) = z
