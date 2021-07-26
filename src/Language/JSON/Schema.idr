module Language.JSON.Schema

import Data.List.Quantifiers
import Decidable.Equality
import Language.JSON
import Data.List

namespace Schema

    public export
    Field : Type

    public export
    FieldList : Type
    FieldList = List Field

    public export
    data Presence = Optional | Required

    public export
    data Schema
      = Null
      | Boolean
      | Number
      | Str
      | Array  (List Schema)
      | Object FieldList
      | Either Schema Schema

    Field = (String, Presence, Schema)

namespace Indexed

  mutual
    public export
    data Field : (String, Presence, Schema) -> Type where
      RequiredField : {f : String} ->         JSON s -> Field (f,Required,s)
      OptionalField : {f : String} -> Maybe (JSON s) -> Field (f,Optional,s)

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

  public export
  data HasJSONField : String -> Schema -> Type where
    Here  : {f : String}                -> HasJSONField f  (Object ((f,_,_) :: _))
    There : HasJSONField f0 (Object fs) -> HasJSONField f0 (Object ((f1,_,_) :: fs))

  export
  hasJSONField : (f : String) -> (s : Schema) -> Maybe (HasJSONField f s)
  hasJSONField f (Object ((x, _, _) :: xs)) = case decEq f x of
    Yes f_is_x => Just (rewrite f_is_x in Here)
    No contra  => map There (hasJSONField f (Object xs))
  hasJSONField f _ = Nothing

  total
  export
  getField : {s : Schema} -> JSON s -> (f : String) -> {auto ok : HasJSONField f s} -> Maybe (z : Schema ** JSON z)
  getField (JObject ((RequiredField x) :: xs))  f {ok = Here}       = Just (MkDPair _ x)
  getField (JObject ((OptionalField x) :: xs))  f {ok = Here}       = map (MkDPair _) x
  getField (JObject (_ :: xs))                  f {ok = (There y)}  = getField (JObject xs) f {ok = y}

mutual
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
