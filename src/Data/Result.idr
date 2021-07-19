module Data.Result

||| Result
|||
||| This type is mentioned during the definition of the
||| OrderTaking process. I tried to follow Scott's
||| book as closely as possible.
public export
data Result r e
  = Ok r
  | Error e
