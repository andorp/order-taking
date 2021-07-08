module Data.Form

%default total

public export
data Form : Type -> Type -> Type where
  Error : List e -> Form e a
  Value : a      -> Form e a

export
Functor (Form e) where
  map f (Error es) = Error es
  map f (Value x)  = Value (f x)

export
Applicative (Form e) where
  pure x = Value x
  Error es1 <*> Error es2 = Error (es1 ++ es2)
  Error es1 <*> Value x   = Error es1
  Value f   <*> Error es1 = Error es1
  Value f   <*> Value x   = Value (f x)

export
field : a -> (a -> Maybe b) -> e -> Form e b
field v f e = maybe (Error [e]) Value $ f v

export
optionalField : Maybe a -> (a -> Maybe b) -> e -> Form e (Maybe b)
optionalField Nothing f e  = Value Nothing
optionalField (Just v) f e = Just <$> field v f e
