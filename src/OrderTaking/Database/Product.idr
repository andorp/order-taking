module OrderTaking.Database.Product

import Data.String
import Service.NodeJS.SQLite

productTable : String
productTable =
  unlines
    [ "CREATE TABLE product"
    , "( id INTEGER PRIMARY KEY AUTOINCREMENT"
    , ", code        TEXT NOT NULL"
    , ", description TEXT NOT NULL"
    , ", price       NUMBER NOT NULL"
    , ", CONSTRAINT code_unique UNIQUE(code)"
    , ")"
    ]

export
productCodeExists : HasIO io => String -> io Bool
-- TODO

export
productPrice : HasIO io => String -> io (Maybe Double)
-- TODO

logError : String -> Error -> IO ()
logError _ err = case !(occured err) of
  Nothing => pure ()
  Just e  => putStrLn !(toString e)

export
initDB : IO ()
initDB = do
  sqlite <- SQLite.require
  db <- SQLite.database sqlite "./db/product.db"
  Database.run db productTable logError
  Database.close db
