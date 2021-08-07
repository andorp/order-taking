module Language.JSON.Schema

-- Schema aware JSON representation.

import Data.List.Quantifiers
import Decidable.Equality
import Language.JSON
import Data.List

-- JSON schema can be represented as a simple dataype, which will be in use to
-- index a JSON value. We can pattern match on the expected shape of the JSON,
-- and client codes rely on the Schema, rather than doing JSON processing
-- blindly.
namespace Schema

  mutual

    -- Here we use mutual block to define datatypes that refer to each other
    -- during their definition.

    ||| Description of a JSON schema
    public export
    data Schema
        -- The JSON value of this schema is Null
      = Null
        -- The JSON value of this schema contains a Boolean
      | Boolean
        -- The JSON value of this schema contains a Number
      | Number
        -- The JSON value of this schema contains a String
      | Str
        -- The JSON value of this schema constains an Array where
        -- the elements have the schema from the list.
      | Array  (List Schema)
        -- The JSON value of this schema contains an Object where
        -- the fields of the object have the schema from the field
        -- list.
      | Object FieldList
        -- The JSON  value of this schema contains a value which
        -- has one of the schemas.
      | Either Schema Schema

    ||| A field could be optional or required in the JSON Object.
    public export
    data Presence = Optional | Required

    ||| Declaration of type for the Field description of a JSON object.
    |||
    ||| A field in the JSON object has a name, could be required or optional
    ||| field, and should have the given schema.
    public export
    Field : Type
    Field = (String, Presence, Schema)

    ||| Field description for a JSON object.
    public export
    FieldList : Type
    FieldList = List Field

-- Schema indexed JSON data representation.
namespace Indexed

  mutual
    
    ||| Schema indexed JSON representation.
    |||
    ||| The index holds a Schema definition which can be in use of determine
    ||| the expected shape of the JSON value. Having a schema as index helps
    ||| us to create clients codes and have strong assertions on data
    ||| coming from external sources, but validated with the schema,
    ||| which is computed from a separate input.
    |||
    ||| This way we can make connections beetwen SQL queries, their result
    ||| JSON and the client code that uses the result.
    public export
    data JSON : Schema -> Type where
      JNull    :                                        JSON Null
      JBoolean : Bool                                -> JSON Boolean
      JNumber  : Double                              -> JSON Number
      JString  : String                              -> JSON Str
      JArray   : {xs : List Schema}  -> All JSON xs  -> JSON (Array xs)
      JObject  : {xs : FieldList}    -> All Field xs -> JSON (Object xs)
      JLeft    : {l  : Schema}       -> JSON l       -> JSON (Either l r)
      JRight   : {r  : Schema}       -> JSON r       -> JSON (Either l r)

    -- The 'All' type requires some explanation.
    --
    -- A proof that all elements of a list satisfy a property. It is a list of
    -- proofs, corresponding element-wise to the `List`.
    -- data All : (0 p : a -> Type) -> List a -> Type where
    --   Nil  : All p Nil
    --   (::) : {0 xs : List a} -> p x -> All p xs -> All p (x :: xs)
    --
    -- It is a list like construction, but also holds to a predicate (something that is 'a -> Type')
    -- and applies creates a list using the predice applied to the list of its index 'xs'.
    -- If the index of 'All' has type of 'List Type', and the predicate is the identity we
    -- get the HList construction, which is able to store elements of different type, because we
    -- have the information about which index has which type. Example;
    -- 
    -- hlist : HList [Int, Bool, Int]
    -- hlist = [0,False,1] -- we can use syntactical sugar for list, because we have Nil and (::)
    --
    -- Another usecase for All is to create a list where the index represents index values for
    -- a given indexed types, and we create a list of indexed values of the same indexed type.
    -- In this case the JArray; All the elements of the JArray must be a JSON which requires
    -- a schema as index, for that reason we have to have a list of indexes, and we can use
    -- All to create a well-types list. For example
    -- 
    -- jarray : JSON (Array [Number, Boolean, Number])
    -- jarray = JArray [JNumber 0, JBoolean False, JNumber 1]
    -- 
    -- Similarly as we done in the case of 'hlist' example.

    ||| Field in the JSON object.
    |||
    ||| The field must have name, its presence can optional and its value
    ||| should have the given schema.
    public export
    data Field : (String, Presence, Schema) -> Type where
      ||| Required field in the JSON object; the value must be present.
      RequiredField : {f : String} ->         JSON s -> Field (f,Required,s)
      ||| Optional field in the JSON object; the value can be absent.
      OptionalField : {f : String} -> Maybe (JSON s) -> Field (f,Optional,s)

-- Our first constuctive proof like tool.
--
-- When we write a function that extracts a field from a JSON, lets call it
-- 'getField' we have the following design choices as engineers.
--
-- - Make the function partial, meaning that when the JSON value is not an Object
--   or the field is not in the object we don't cover that case, and we get a
--   runtime error.
--   In this case we lie to the client code, the code which calls our function,
--   about the nature of our function. Type tells us 'getField : FieldName -> JSON -> JSON' works for all
--   inputs without any problem, but also Idris tells us this function is 'partial'
--   and marks the function partial, also marks any other client codes partial.
--   Writing error correction code in this case is impossible.
--
-- - Make a precondition for the 'getFieldOk : String -> JSON -> Bool' and only call the 'getField' when the
--   precondition applies; such as;
--
--   if getFieldOk f json
--     then getField f json
--     else JNull
--
--   We can use this approach within the implementation of the getField or the client code of the getField.
--   In the former case we distort information, in the later, the client code's responsibility
--   to handle the error case accordingly, but the nature of the error why the field was not ok it stays
--   hidden.
--
-- - Make the partial function total, insead of creating 'getField : FieldName -> JSON -> JSON' partial
--   construction for all the non-handled inputs we can return Nothing, for
--   the handled parts we can return 'Just x'. Something like this; ''getField : FieldName -> JSON -> Maybe JSON'.
--   This forces the client code to act on partial nature of the problem, and handle
--   wrong inputs after learning the fact that the input was not ok, for the 'getField' function, becase
--   it returns Nothing. The client code not neceserily knows the reason why the getField returned
--   Nothing. Writing error correction code is possible, but the error sitation is not recoverable
--   from the result of the function.
--
-- - Make the function total, using its own return Domain, this domain enumerates all the possible
--   good and bad cases and let the client code act on it accordingly, something like
--
--   data GetFieldResult
--     = NotAnObject
--     | FieldNotFound
--     | FieldValue JSON
--
--   getField : FieldName -> JSON -> GetFeildResult
--
--   The client can act on the kind of the errourneous situations accordingly.
--
-- - And the last one which is only option if we have dependent types:
--   Make sure that the 'getField' function can be called only with a restricted set of
--   inputs, via an indexed JSON representation and indexed ObjectHasRequiredField datatype.
--   'getField : (f : FieldName) -> (s : Schema) -> JSON s -> ObjectHasRequiredField f s -> (z : Schema ** JSON z)'
--   We can call the ObjectHasRequiredField a witness of the field is being part of the schema, or we
--   can can call this witness as a constructive proof; which shows us where to find the field
--   in the Schema definition if it can be found at all. If there field is not in the Schema represented
--   JSON object, than we can not create this witness at all, meaning that we don't have a value
--   which we would invoke the getField function.
--   The getField function fits to the general pattern of 'f : (x : a) -> (P x) -> b', which explanation as
--   if you can give me an 'x' and a some information that the 'x' will be ok for the 'f',
--   f is going to be ok. 
--     More technically, if you give me an x and the proof that x is an
--   element in the co-domain of the function, the function can executed safely.
--   This is another form of defensive programming, where the assertions before the
--   function call are precise and there is an explicit connection between the precondition
--   of the function and the implementation of the function.
--     We can improve the 'getField' function with auto search implicit parameters;
--   where Idris fills out these details if it is possible.
--   'getField : JSON s -> (f : String) -> {auto ok : ObjectHasRequiredField f s} -> (z : Schema ** JSON z)'
--   In cases where the parameters of the 'getField' are constant like values Idris is easily
--   able to create the witness for the field being in the schema, but in cases if we acquire the fieldName
--   or the schema definition from runtime value, we need to be able to create such a witness, for that
--   we need to create the function which creates the witness from the given values at runtime.
--   'hasJSONField : (f : String) -> (s : Schema) -> Maybe (ObjectHasRequiredField f s)'
--   In this case we also provide the assumption check for the 'getField' function, which can
--   be used if Idris is not able to find the witness of the condition. If the runtime witness
--   creation returns Nothing, than the client code execute its defensive code-path.

namespace ObjectHasFieldProof

  ||| A witness for an Object has the field, which is required.
  |||
  ||| The witness points to the element of the field list, in the structure of Here, There linked
  ||| list like construction, which helps us to traverse the Object when looking up the value.
  |||
  ||| NOTE: These constructions should be optimized to a simple integer index by the Idris compiler.
  public export
  data ObjectHasRequiredField : (0 f : String) -> (0 s : Schema) -> Type where
    Here  :                                          ObjectHasRequiredField f  (Object ((f,Required,_) :: _ ))
    There : ObjectHasRequiredField f0 (Object fs) -> ObjectHasRequiredField f0 (Object ((f1,_,_)       :: fs))

  ||| Find the ObjectHasRequiredField, which is equivalent to the index of the element in the fields of Object.
  export
  hasJSONField : (f : String) -> (s : Schema) -> Maybe (ObjectHasRequiredField f s)
  hasJSONField f (Object ((x, Required, _) :: xs)) = case decEq f x of
    Yes f_is_x => Just (rewrite f_is_x in Here)
    No contra  => map There (hasJSONField f (Object xs))
  hasJSONField f _ = Nothing

  ||| Traverse the field of object and retrieve the JSON value of the field, alongside its Schema.
  total
  -- This function is total, because the indexes in the ObjectHasRequiredField determines that this function
  -- only be called with Object JSON, and rest of constructors don't need to be inspected.
  getFieldSafe : (s : Schema) -> JSON s -> (f : String) -> (1 ok : ObjectHasRequiredField f s) -> (z : Schema ** JSON z)
  getFieldSafe (Object ((_, (Required, _)) ::  _)) x                  f Here      = MkDPair _ x
  getFieldSafe (Object ((f1, (_, _))       :: fs)) (JObject (_ :: x)) f (There y) = getFieldSafe (Object fs) (JObject x) f y

  ||| Traverse the field of object and retrieve the JSON value of the field, alongside its Schema.  
  export
  -- Use implicit parameters where possible.
  getField : {s : Schema} -> JSON s -> (f : String) -> {auto 1 ok : ObjectHasRequiredField f s} -> (z : Schema ** JSON z)
  getField {s} json f {ok} = getFieldSafe s json f ok

mutual

  ||| Forget about the schema index of the JSON and convert it to the non idexed JSON representation.
  export
  toNonIndexed : Indexed.JSON s -> Data.JSON
  toNonIndexed JNull        = JNull
  toNonIndexed (JBoolean x) = JBoolean x
  toNonIndexed (JNumber x)  = JNumber x
  toNonIndexed (JString x)  = JString x
  toNonIndexed (JArray xs)  = JArray (toNonIndexedArray xs)
  toNonIndexed (JObject xs) = JObject (toNonIndexedObjectFields xs)
  toNonIndexed (JLeft x)    = toNonIndexed x
  toNonIndexed (JRight x)   = toNonIndexed x

  toNonIndexedArray : {xs : List Schema} -> All Indexed.JSON xs -> List Data.JSON
  toNonIndexedArray []        = []
  toNonIndexedArray (x :: xs) = toNonIndexed x :: toNonIndexedArray xs

  toNonIndexedObjectFields : {xs : FieldList} -> All Field xs -> List (String, Data.JSON)
  toNonIndexedObjectFields [] = []
  toNonIndexedObjectFields ((RequiredField {f} x) :: xs)        = (f, toNonIndexed x) :: toNonIndexedObjectFields xs
  toNonIndexedObjectFields ((OptionalField {f} Nothing) :: xs)  =                        toNonIndexedObjectFields xs
  toNonIndexedObjectFields ((OptionalField {f} (Just x)) :: xs) = (f, toNonIndexed x) :: toNonIndexedObjectFields xs

mutual

  ||| Parse a non-indexed JSON with the given schema.
  |||
  ||| If the given non-indexed JSON fits the schema, we can create an indexed
  ||| version of it. As schemas can be this parsing backtracks on schema
  ||| Either constructions. During the creation of the indexed object, it is
  ||| not required from the fields to have the same order as in the schema,
  ||| leading to another search when the indexed object is assembled.
  export
  toIndexed : (s : Schema) -> Data.JSON -> Maybe (Indexed.JSON s)
  toIndexed Null          JNull        = Just JNull
  toIndexed Boolean       (JBoolean b) = Just (JBoolean b)
  toIndexed Number        (JNumber n)  = Just (JNumber n)
  toIndexed Str           (JString s)  = Just (JString s)
  toIndexed (Array xs)    (JArray ys)  = do
    elems <- toIndexedArray xs ys
    Just (JArray elems)
  toIndexed (Object xs)   (JObject ys) = do
    fields <- toIndexedObject xs ys
    Just (JObject fields)
  toIndexed (Either x y)  json = (map JLeft (toIndexed x json)) <|> (map JRight (toIndexed y json))
  toIndexed _             _    = Nothing

  toIndexedArray : (xs : List Schema) -> List JSON -> Maybe (All JSON xs)
  toIndexedArray [] [] = Just []
  toIndexedArray [] (x :: xs) = Nothing
  toIndexedArray (x :: xs) [] = Nothing
  toIndexedArray (x :: xs) (y :: ys) = do
    json <- toIndexed x y
    rest <- toIndexedArray xs ys
    Just (json :: rest)

  -- Looks up the field, if a required is missing it fails, otherwise succeeds, if
  -- there are more fields in the object, that is expected, the parsing still be
  -- successful.
  toIndexedObject : (xs : FieldList) -> List (String, JSON) -> Maybe (All Field xs)
  toIndexedObject [] []
    -- Base case, no field to lookup, in an empty list, that is successful conversion
    = Just []
  toIndexedObject [] ys@(_ :: _)
    -- No field to lookup, but there are some extra left, the JSON has more information
    -- than needed, but that is not a failure.
    = Just []
  toIndexedObject ((f, Optional, s) :: xs) []
    -- Optional field, if not present it can be ignored and OptionalField needs to be used
    = do rest <- toIndexedObject xs []
         Just (OptionalField Nothing :: rest)        
  toIndexedObject ((f, Required, s) :: xs) []
    -- Required field, if not present the parsing should fail, and we have no fields given.
    = Nothing
  toIndexedObject ((f, Optional, s) :: xs) ys@(_ :: _)
    -- Optional field, if not present it can be ignored and OptionalField needs to be used
    = case lookup f ys of
        Nothing => map (OptionalField Nothing ::) (toIndexedObject xs ys)
        Just jdata => do
          field <- toIndexed s jdata -- if the field is present, but it fails to parse,
                                     -- that is an error.
          rest  <- toIndexedObject xs ys
          Just (OptionalField (Just field) :: rest)
  toIndexedObject ((f, Required, s) :: xs) ys@(_ :: _)
    -- Required field, if not present the parsing should fail, and we have no fields given.
    = do jdata <- lookup f ys
         field <- toIndexed s jdata
         rest  <- toIndexedObject xs ys
         Just (RequiredField field :: rest)
