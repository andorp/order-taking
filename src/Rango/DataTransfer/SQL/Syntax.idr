module Rango.DataTransfer.SQL.Syntax

import Data.String
import Data.List
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
  | SQL_Text
  | SQL_Double
  | SQL_Datetime

public export
data Value : (sqlType : SQLType) -> Type where
  SQLIntVal     : Int     -> Value SQL_Integer
  SQLDoubleVal  : Double  -> Value SQL_Double
  SQLTextVal    : String  -> Value SQL_Text

public export
data Modifier
  = PrimaryKey
  | AutoIncrement
  | NotNull

public export
record Field where
  constructor MkField
  name      : FieldName
  sqlType   : SQLType
  modifiers : List Modifier

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

renderSQLType : SQLType -> String
renderSQLType SQL_Integer   = "INTEGER"
renderSQLType SQL_Double    = "DOUBLE PRECISION"
renderSQLType SQL_Text      = "TEXT"
renderSQLType SQL_Datetime  = "DATETIME"

renderModifier : Modifier -> String
renderModifier PrimaryKey    = "PRIMARY KEY"
renderModifier AutoIncrement = "AUTOINCREMENT"
renderModifier NotNull       = "NOT NULL"

withCommas : List String -> String
withCommas xs = concat (intersperse ", " xs)

renderConstraint : Constraint -> String
renderConstraint (Unique name fields) =
  "CONSTRAINT \{name} UNIQUE(\{withCommas fields})"
renderConstraint (ForeignKey field foreignTable foreignField) =
  "FOREIGN KEY(\{field}) REFERENCES \{foreignTable}(\{foreignField})"

renderCreateField : Field -> String
renderCreateField (MkField name sqlType modifiers) =
  show name ++ " " ++ renderSQLType sqlType ++ " " ++ unwords (map renderModifier modifiers)

export
renderCommand : Command -> String
renderCommand (CreateTable table fields constraints) =
  "CREATE TABLE \{table} (\{withCommas (map renderCreateField fields ++ map renderConstraint constraints)})"
renderCommand (Select fields table filters) = "SOMETHING"
renderCommand (Insert table fields values) = "SOMETHING"
