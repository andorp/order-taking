module Rango.BoundedContext.Command

public export
data DateTime = MkDateTime

public export
record Command (a : Type) where
  constructor MkCommand
  datum     : a
  timeStamp : DateTime
  userId    : String


