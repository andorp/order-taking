module Rango.DataTransfer.SQL.Syntax

import public Data.List.Quantifiers -- for HList

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

public export
data SQLValue : SQLType -> Type where
  SQLInteger : Int    -> SQLValue SQL_Integer
  SQLText    : String -> SQLValue SQL_Text
  SQLDouble  : Double -> SQLValue SQL_Double

public export
record Field where
  constructor MkField
  name          : FieldName
  sqlType       : SQLType
  primaryKey    : Bool
  autoIncrement : Bool
  notNull       : Bool

public export
data Modifier 
  = PrimaryKey
  | AutoIncrement
  | NotNull

fieldModifiers : Field -> List Modifier
fieldModifiers (MkField name sqlType primaryKey autoIncrement notNull) =
  if primaryKey     then [PrimaryKey]    else [] ++
  if autoIncrement  then [AutoIncrement] else [] ++
  if notNull        then [NotNull]       else []

public export
field : FieldName -> SQLType -> List Modifier -> Field
field f t ms = setModifiers ms $ MkField f t False False False
  where
    setModifiers : List Modifier -> Field -> Field
    setModifiers []                    f = f
    setModifiers (PrimaryKey    :: xs) f = setModifiers xs (record { primaryKey    = True } f)
    setModifiers (AutoIncrement :: xs) f = setModifiers xs (record { autoIncrement = True } f)
    setModifiers (NotNull       :: xs) f = setModifiers xs (record { notNull       = True } f)

public export
FieldType : Field -> Type
FieldType (MkField name sqlType False      autoIncrement False)   = Maybe (SQLValue sqlType)
FieldType (MkField name sqlType primaryKey autoIncrement notNull) = SQLValue sqlType

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
data ValidTable : List Field -> List Constraint -> Type where
  YesOfCourseValid : ValidTable fields constraints
  -- TODO: Implement this check

public export
record Table where
  constructor MkTable
  name         : TableName
  fields       : List Field
  constraints  : List Constraint
  0 validTable : ValidTable fields constraints

public export
data FieldOfTy : String -> Type -> Type where
  FieldOf : (n : String) -> t -> FieldOfTy n t

||| Calculate the list of types which is required for the insert command, skipping the
||| auto incremental fields.
public export
InsertValues : List Field -> List Type
InsertValues []        = []
InsertValues (f@(MkField name sqlType primaryKey False notNull) :: fs) = FieldOfTy name (FieldType f) :: InsertValues fs
InsertValues (f@(MkField name sqlType primaryKey True  notNull) :: fs) = InsertValues fs -- Do not ask for auto fields

public export
data FieldDefinedInTable : (field : FieldName) -> (fields : List Field) -> Type where
  Here  : FieldDefinedInTable f (MkField f _ _ _ _ :: fs)
  There : FieldDefinedInTable f fs -> FieldDefinedInTable f (g :: fs)

namespace FieldsExsistenceCheck
  public export
  data SelectedFieldsDefinedInTable : (names : List FieldName) -> (fields : List Field) -> Type where
    Nil  : SelectedFieldsDefinedInTable [] fields
    (::) : FieldDefinedInTable f fields -> SelectedFieldsDefinedInTable fs fields -> SelectedFieldsDefinedInTable (f :: fs) fields

namespace FilterExsistenceCheck
  public export
  data FilteredFieldsDefinedInTable : (filters : List (FieldName, String, String)) -> (fields : List Field) -> Type where
    Nil  : FilteredFieldsDefinedInTable [] fields
    (::) : FieldDefinedInTable f fields -> FilteredFieldsDefinedInTable fs fields -> FilteredFieldsDefinedInTable ((f, _, _) :: fs) fields

public export
data Command : Type where
  CreateTable
    :  (table : Table)
    -> Command
  Insert
    :  (table : Table)
    -> (values : HList (InsertValues table.fields))
    -> Command

%name Command cmd

public export
data Query : Type where
  Select
    :  (  fields    : List FieldName)
    -> (  table     : Table)
    -> (  okFields  : SelectedFieldsDefinedInTable fields table.fields)
    => (  filters   : List (FieldName, String, String))
    -> (0 okFilters : FilteredFieldsDefinedInTable filters table.fields)
    => Query

%name Query qry

withCommas : List String -> String
withCommas xs = concat (intersperse ", " xs)

renderSQLType : SQLType -> String
renderSQLType SQL_Integer   = "INTEGER"
renderSQLType SQL_Double    = "DOUBLE PRECISION"
renderSQLType SQL_Text      = "TEXT"

renderModifier : Modifier -> String
renderModifier PrimaryKey    = "PRIMARY KEY"
renderModifier AutoIncrement = "AUTOINCREMENT"
renderModifier NotNull       = "NOT NULL"

renderConstraint : Constraint -> String
renderConstraint (Unique name fields) =
  "CONSTRAINT \{name} UNIQUE(\{withCommas fields})"
renderConstraint (ForeignKey field foreignTable foreignField) =
  "FOREIGN KEY(\{field}) REFERENCES \{foreignTable}(\{foreignField})"

renderCreateField : Field -> String
renderCreateField f =
  show f.name ++ " " ++ renderSQLType f.sqlType ++ " " ++ unwords (map renderModifier (fieldModifiers f))

renderSQLValue : SQLValue s -> String
renderSQLValue (SQLInteger x) = show x
renderSQLValue (SQLText x)    = show x
renderSQLValue (SQLDouble x)  = show x

-- Instead of doing a lot of proof around the type safety of this construction, we use
-- type level programming in a local hack and we dispatch on the type that we expect to
-- see in these cases. Use this function with care.
renderFieldValue : {t : Type} -> t -> String
renderFieldValue {t=FieldOfTy n s} (FieldOf _ x) = renderFieldValue x
renderFieldValue {t=Maybe (SQLValue s)} (Just x) = renderFieldValue x
renderFieldValue {t=Maybe (SQLValue s)} Nothing  = "NULL"
renderFieldValue {t=SQLValue s} x = renderSQLValue x
renderFieldValue _ = "(╯°□°)╯︵ ┻━┻" -- This shouldn't happen

-- This HList should contain a list of FieldOfTy entries, but we don't
-- check type-safety here, instead we just dynamically dispatch on the
-- actual type case and render the field, or render gibberish if we got the
-- types wrong. Use this function with care.
renderInsertValues : {ts : List Type} -> HList ts -> List String
renderInsertValues []        = []
renderInsertValues (a :: as) = renderFieldValue a :: renderInsertValues as

renderInsertNames : List Field -> List String
renderInsertNames [] = []
renderInsertNames ((MkField name sqlType primaryKey False notNull) :: xs) = name :: renderInsertNames xs
renderInsertNames ((MkField name sqlType primaryKey True  notNull) :: xs) = renderInsertNames xs

export
renderCommand : Command -> String
renderCommand (CreateTable table)
  = "CREATE TABLE \{table.name} (\{withCommas (map renderCreateField table.fields ++ map renderConstraint table.constraints)})"
renderCommand (Insert table values)
  = "INSERT INTO \{table.name} (\{withCommas (renderInsertNames table.fields)}) VALUES (\{withCommas (renderInsertValues values)})"

export
renderQuery : Query -> String
renderQuery (Select fields table filters)
  = "SELECT \{withCommas fields} FROM \{table.name}" ++
    (case filters of
      [] => ""
      fs => " WHERE " ++ (withCommas $ map (\(field, op, cond) => "(\{field} \{op} \{cond})") fs)) ++
    ";"
