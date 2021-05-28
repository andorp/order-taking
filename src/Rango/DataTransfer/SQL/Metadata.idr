module Rango.DataTransfer.SQL.Metadata

import Rango.DataTransfer.SQL.Syntax

export
metadataTable : Command
metadataTable = CreateTable
  "metadata"
  [ MkField "meta_id"  SQL_Integer  [PrimaryKey]
  , MkField "id"       SQL_Text     [NotNull]
  , MkField "created"  SQL_Datetime [NotNull]
  , MkField "modified" SQL_Datetime []
  , MkField "user"     SQL_Text     []
  ]
  [ Unique "unique_id" ["id"]
  ]

export
createMetadataTable : String
createMetadataTable = renderCommand metadataTable
