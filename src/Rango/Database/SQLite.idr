module Rango.Database.SQLite

import Language.JSON.Schema
import Rango.DataTransfer.SQL.Syntax
import Service.NodeJS.SQLite
import Service.NodeJS.Promise

public export
resultSchema : Query -> Schema
resultSchema (Select fields table {okFields} filters) = Object (fieldList okFields)
  where
    jsonType : SQLType -> Schema
    jsonType SQL_Integer  = Number
    jsonType SQL_Text     = Str
    jsonType SQL_Double   = Number

    fieldInfo : SQL.Syntax.Field -> Schema.Field
    fieldInfo (MkField name sqlType primaryKey autoIncrement False) = (name, Optional, jsonType sqlType)
    fieldInfo (MkField name sqlType primaryKey autoIncrement True)  = (name, Required, jsonType sqlType)

    fieldInTable : {n : FieldName} -> {fields : List SQL.Syntax.Field} -> FieldDefinedInTable n fields -> SQL.Syntax.Field
    fieldInTable {n = n} {fields = (f@(MkField n _ _ _ _) :: fs)} Here      = f
    fieldInTable {n = n} {fields = (g :: fs)}                     (There t) = fieldInTable t

    fieldList : {ns : List FieldName} -> {fs : List SQL.Syntax.Field} -> SelectedFieldsDefinedInTable ns fs -> FieldList
    fieldList [] = []
    fieldList (x :: xs) = (fieldInfo (fieldInTable x)) :: fieldList xs

||| Execute command
export
command : Database -> Command -> Promise SomeError
command db cmd = Database.run db $ renderCommand cmd

||| Execute query
export
query : Database -> (qry : Query) -> Promise (Safe.Result (resultSchema qry))
query db qry = Database.Safe.get db (resultSchema qry) $ renderQuery qry
