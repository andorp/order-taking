module Data.StringN

import Data.Nat

%default total

||| A string of maximum length n.
|||
||| StringN boxes a String and a compile time proof
||| (compile time because its quantity is 0) that the string
||| is shorter then 'n'.
export
data StringN : Nat -> Type where
  MkStringN : (s : String) -> (length s `LTE` m) => StringN m

||| A simple way of proving, or calculating the proof of 'LTE n m'
|||
||| This approach is simpler than creating a DecEq as we know that
||| the negative case of the proof, when 'n' is greater then 'm',
||| is irrelevant for us, and we don't spend time developing that
||| proof. See below.
mkLTE : (n , m : Nat) -> Maybe (n `LTE` m)
mkLTE 0     0     = Just LTEZero
mkLTE 0     (S k) = Just LTEZero
mkLTE (S k) 0     = Nothing
mkLTE (S k) (S j) = do
  proofLTE <- mkLTE k j
  Just (LTESucc proofLTE)

||| Smart constructor of the StringN type.
|||
||| If the given String is longer than the given length limit
||| the value can not be created.
export
create : (n : Nat) -> String -> Maybe (StringN n)
create n s = do
  -- As creating a value of StringN requires a proof that
  -- the length is ok, we need to call the function that
  -- creates that proof object for us, which is just a value
  -- of the type 'LTE n m' If we can create that value via
  -- the 'mkLTE' function we are safe, otherwise we can't create
  -- the value for 'StringN n'. When we are unable to create the LTE n m,
  -- because we got a longer String as the parameter, then mkLTE returns
  -- Nothing, which is picked up by the Monad instance of the Maybe, so
  -- at that point the create function returns Nothing.
  proofLTE <- mkLTE (length s) n
  -- If we are able to get a proof of 'LTE n m' than we can construct
  -- the StringN, because in the context we have a value which has the
  -- right type and the '-> (length s `LTE` m) =>' part of the StringN
  -- is able to pick that type up.
  -- Check for '=>' behavior in the Idris documentation
  Just $ MkStringN s

||| Value extraction
export
(.value) : StringN x -> String
(.value) (MkStringN s) = s
