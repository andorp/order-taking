module OrderTaking.Database.Product

import Data.String
import Service.NodeJS.SQLite
import Service.NodeJS.Promise


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
productCodeExists _ = pure True
-- TODO

export
productPrice : HasIO io => String -> io (Maybe Double)
productPrice _ = pure $ Just 1.0
-- TODO

export
initDB : IO ()
initDB = do
  sqlite <- SQLite.require
  resolve' (\_ => putStrLn "OK.") putStrLn $ do
    db <- either !(SQLite.database sqlite "./db/product.db")
    ignore $ Database.run db productTable
    ignore $ Database.close db
