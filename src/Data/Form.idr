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

-- The definition of the 'Form' is another way to introduce
-- a datatype. This datatype has two type parameters and
-- two data constructors.
-- The two type parameter come from the ': Type -> Type ->' part of
-- the definition. In a bit more technical term, the Form
-- datatype has two parameters/indices. In this case the parameters
-- refer to other types. Not only 'Type' can be an index, but
-- any other value of any type, such as values of 'Nat'. We will
-- a complex example of that.

export
Functor (Form e) where
  map f (Error es) = Error es
  map f (Value x)  = Value (f x)

-- In Idris we can define interfaces which represent a set of functions
-- associated with one or more types. The implementation of the
-- functions in the interface should differ from implementation to
-- implementation.
--
-- As technicality; the interfaces are compiled to dependent records.
--
-- One of the standard interfaces is the Functor
-- interface, which gives the ability of mapping, if the given
-- datatype is parametrized.

export
Applicative (Form e) where
  pure x = Value x
  Error es1 <*> Error es2 = Error (es1 ++ es2)
  Error es1 <*> Value x   = Error es1
  Value f   <*> Error es1 = Error es1
  Value f   <*> Value x   = Value (f x)

-- Another standard interface in Idris is the Applicative, which is
-- an extension of the function and it gives the ability of
-- apply a wrapped function to a wraped value inside the datatype.

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
