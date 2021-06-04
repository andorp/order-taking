module OrderTaking.Domain.Prelude

import Data.Nat

public export
data Result r e
  = Ok r
  | Error e

namespace Between

  export
  data Between : (numType : Type) -> (l : numType) -> (h : numType) -> Type where
    MkBetween : (x : numType) -> Between numType l h

  export
  mkBetween : {n : Type} -> Ord n => {l , h : n} -> (x : n) -> Maybe (Between n l h)
  mkBetween x = do
    let True = l <= h
        | _ => Nothing
    let True = l <= x
        | _ => Nothing
    let True = x <= h
        | _ => Nothing
    Just $ MkBetween x

  export
  value : Between n l h -> n
  value (MkBetween v) = v

namespace StringN 

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
