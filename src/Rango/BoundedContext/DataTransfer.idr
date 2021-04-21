module Rango.BoundedContext.DataTransfer

public export
record DataTransfer (t : Type) where
  constructor MkDataTransfer
  value : t

-- Input Gate: Does parsing/validation
-- Output Gate: Does security checking, private information shouldn't leak out.
