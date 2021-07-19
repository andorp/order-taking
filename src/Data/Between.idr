module Data.Between

%default total

||| Simple datatype which ensures that the boxed value is in the given range, using smart
||| constructors.
export
data Between : (numType : Type) -> (l : numType) -> (h : numType) -> Type where
  MkBetween : (x : numType) -> Between numType l h

||| Smart constructor for the Between datatype.
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

||| Value extraction.
export
value : Between n l h -> n
value (MkBetween v) = v
