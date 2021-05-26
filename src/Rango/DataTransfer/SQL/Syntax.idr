module Rango.DataTransfer.SQL.Syntax

import Data.Vect

public export
TableName : Type
TableName = String

public export
FieldName : Type
FieldName = String

public export
data SQLType
  = SQL_Integer
  | SQL_Number
  | SQL_Text

public export
data Value : (sqlType : SQLType) -> Type where
  SQLIntVal     : Int     -> Value SQL_Integer
  SQLDoubleVal  : Double  -> Value SQL_Number
  SQLIntegerVal : Integer -> Value SQL_Number
  SQLTextVal    : String  -> Value SQL_Text

public export
data Modifier
  = PrimaryKey
  | AutoIncrement
  | NotNull

public export
data Field : Type where
  MkField
    :  (name      : FieldName)
    -> (sqlType   : SQLType)
    -> (modifiers : List Modifier)
    -> Field

public export
data Constraint : Type where
  Unique
    :  (name : String)
    -> (fields : List FieldName)
    -> Constraint
  ForeignKey
    :  (field : FieldName)
    -> (foreignTable : TableName)
    -> (foreignField : FieldName)
    -> Constraint

public export
data Filter : Type

public export
data ValueList : (fields : List Field) -> Type where
  Nil  : ValueList []
  (::) : Value t -> ValueList fs -> ValueList (MkField n t ms :: fs)

public export
data Command : Type where
  CreateTable
    :  (table       : TableName)
    -> (fields      : List Field)
    -> (constraints : List Constraint)
    -> Command
  Select
    :  (fields  : List Field)
    -> (table   : TableName)
    -> (filters : List Filter)
    -> Command
  Insert
    :  (table  : TableName)
    -> (fields : List Field)
    -> (values : ValueList fields)
    -> Command

export
renderCommand : Command -> String
renderCommand = ?rc1
