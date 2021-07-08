module Data.StringN

import Data.Nat

%default total

export
data StringN : Nat -> Type where
  MkStringN : (s : String) -> (0 _ : length s `LTE` m) => StringN m

mkLTE : (n , m : Nat) -> Maybe (n `LTE` m)
mkLTE 0     0     = Just LTEZero
mkLTE 0     (S k) = Just LTEZero
mkLTE (S k) 0     = Nothing
mkLTE (S k) (S j) = do
  proofLTE <- mkLTE k j
  Just (LTESucc proofLTE)

export
create : (n : Nat) -> String -> Maybe (StringN n)
create n s = do
  proofLTE <- mkLTE (length s) n
  Just $ MkStringN s

export
value : StringN x -> String
value (MkStringN s) = s
