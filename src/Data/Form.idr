module Data.Form

%default total

||| Simple Applicative Form which collects the errors of the form.
||| 
||| During the form processing error can occur for any fields,
||| its better to collect them all, rather stopping at the first
||| one. 
|||
||| This abstraction is used in OrderTaking processing.
||| It is not a general form processing for the UI.
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

||| A field value and its parser.
|||
||| If the parsing fails, the error is emmited.
export
field : a -> (a -> Maybe b) -> Lazy e -> Form e b
field v f e = maybe (Error [e]) Value $ f v

||| An optional field and its parser.
|||
||| If the value is present, than parsing happens, on
||| fail the error is emmited.
export
optionalField : Maybe a -> (a -> Maybe b) -> Lazy e -> Form e (Maybe b)
optionalField Nothing f e  = Value Nothing
optionalField (Just v) f e = Just <$> field v f e
