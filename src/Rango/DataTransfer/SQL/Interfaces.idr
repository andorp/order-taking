module Rango.DataTransfer.SQL.Interfaces

import Rango.DataTransfer.SQL.Syntax

export
GroupField : Type
GroupField = String

interface SQL (t : Type) where
  tableInfo : (TableName, GroupField, List Field, List Constraint)
