module Rango.DataTransfer.SQL.Derive

import public Language.Reflection


public export covering
deriveSQL : (name : Name) -> Elab ()
deriveSQL name = pure () -- TODO
