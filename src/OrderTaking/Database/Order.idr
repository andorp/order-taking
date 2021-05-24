module OrderTaking.Database.Order

import Data.String
import Service.NodeJS.SQLite
import OrderTaking.DTO.PlaceOrder


addressTable : String
addressTable =
  unlines
    [ "CREATE TABLE address"
    , "( id INTEGER PRIMARY KEY AUTOINCREMENT"
    , ", line1 TEXT NOT NULL"
    , ", line2 TEXT"
    , ", line3 TEXT"
    , ", line4 TEXT"
    , ", city  TEXT NOT NULL"
    , ", zip   TEXT NOT NULL"
    , ")"
    ]

customerTable : String
customerTable =
  unlines
    [ "CREATE TABLE customer"
    , "( id INTEGER PRIMARY KEY AUTOINCREMENT"
    , ", first_name TEXT NOT NULL"
    , ", last_name  TEXT NOT NULL"
    , ", email      TEXT NOT NULL"
    , ", CONSTRAINT email_unique UNIQUE (email)"
    , ")"
    ]

pricedOrderLineTable : String
pricedOrderLineTable =
  unlines
    [ "CREATE TABLE priced_order_line"
    , "( id INTEGER PRIMARY KEY AUTOINCREMENT"
    , ", order_line_id TEXT NOT NULL"
    , ", product_code  TEXT NOT NULL"
    , ", quantity      TEXT NOT NULL"
    , ", price         NUMBER NOT NULL" -- ???
    , ", CONSTRAINT line_id_unqiue UNIQUE(order_line_id)"
    , ")"
    ]

orderLinesTable : String
orderLinesTable =
  unlines
    [ "CREATE TABLE order_lines"
    , "( id INTEGER PRIMARY KEY AUTOINCREMENT"
    , ", key        INTEGER NOT NULL"
    , ", order_line INTEGER NOT NULL"
    , ", CONSTRAINT unique_key_lines UNIQUE(key, order_line)"
    , ", FOREIGN KEY (order_line) REFERENCES priced_order_line(id)"
    , ")"
    ]

productOrderTable : String
productOrderTable =
  unlines
    [ "CREATE TABLE priced_order"
    , "( id INTEGER PRIMARY KEY AUTOINCREMENT"
    , ", order_id         TEXT NOT NULL"
    , ", customer         INTEGER NOT NULL"
    , ", shipping_address INTEGER NOT NULL"
    , ", billing_address  INTEGER NOT NULL"
    , ", order_lines      INTEGER NOT NULL"
    , ", amount_to_bill   NUMBER NOT NULL"
    , ", FOREIGN KEY (customer)         REFERENCES customer(id)"
    , ", FOREIGN KEY (shipping_address) REFERENCES address(id)"
    , ", FOREIGN KEY (billing_address)  REFERENCES address(id)"
    , ", FOREIGN KEY (order_lines)      REFERENCES order_lines(id)"
    , ")"
    ]

export
saveAddress : HasIO io => AddressDTO -> io (Either String Int)
-- TODO

logError : Error -> IO ()
logError err = case !(occured err) of
  Nothing => pure ()
  Just e  => putStrLn !(toString e)

export
initDB : IO ()
initDB = do
  sqlite <- SQLite.require
  db <- SQLite.database sqlite "./db/order.db"
  Database.run db addressTable          logError
  Database.run db customerTable         logError
  Database.run db pricedOrderLineTable  logError
  Database.run db orderLinesTable       logError
  Database.run db productOrderTable     logError
  Database.close db
