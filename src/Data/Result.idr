module Data.Result

public export
data Result r e
  = Ok r
  | Error e
