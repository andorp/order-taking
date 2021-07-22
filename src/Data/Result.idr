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

-- The 'Result' is an example of introducing an Algebraic Datatype with
-- two type parameters 'r' and 'e'. The 'Result' has two data constructors;
-- 'Ok' and 'Error', both has one argument.

-- A note on the 'public export'. Idris controls visibility with these keywords.
-- For different things the keywords have different meaning. If nothing is written
-- then the definition will be private, only accessible withing the module or the
-- namespace. Every module introduces a namespace. In this example 'Data.Result'
-- If we want to use the definition outside of the namespace we have to add the
-- 'export' or the 'public export' keyword. The 'export' means we only export
-- the datatype but we e hide its constructors. The 'public export' exposes
-- the data constructors too.
