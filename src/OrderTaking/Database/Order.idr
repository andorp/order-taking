module OrderTaking.Database.Order

import Data.String
import Service.NodeJS.SQLite
import OrderTaking.DTO.PlaceOrder
import Rango.DataTransfer.SQL.Syntax


createAddressTable : Command
createAddressTable = CreateTable
  "address"
  [ MkField "id"    SQL_Integer [PrimaryKey, AutoIncrement]
  , MkField "line1" SQL_Text    [NotNull]
  , MkField "line2" SQL_Text    []
  , MkField "line3" SQL_Text    []
  , MkField "line4" SQL_Text    []
  , MkField "city"  SQL_Text    [NotNull]
  , MkField "zip"   SQL_Text    [NotNull]
  ]
  []

addressTable : String
addressTable = renderCommand createAddressTable

createCustomerTable : Command
createCustomerTable = CreateTable
  "customer"
  [ MkField "id"         SQL_Integer  [PrimaryKey, AutoIncrement]
  , MkField "first_name" SQL_Text     [NotNull]
  , MkField "last_name"  SQL_Text     [NotNull]
  , MkField "email"      SQL_Text     [NotNull]
  ]
  [ Unique "email_unqiue" ["email"] ]

customerTable : String
customerTable = renderCommand createCustomerTable

createPricedOrderLineTable : Command
createPricedOrderLineTable = CreateTable
  "priced_order_line"
  [ MkField "id"            SQL_Integer  [PrimaryKey, AutoIncrement]
  , MkField "order_line_id" SQL_Text     [NotNull]
  , MkField "product_code"  SQL_Text     [NotNull]
  , MkField "quantity"      SQL_Number   [NotNull]
  , MkField "price"         SQL_Number   [NotNull]
  ]
  [ Unique "line_id_unique" ["order_line_id"] ]

pricedOrderLineTable : String
pricedOrderLineTable = renderCommand createPricedOrderLineTable

createOrderLinesTable : Command
createOrderLinesTable = CreateTable
  "order_lines"
  [ MkField "id"          SQL_Integer [PrimaryKey, AutoIncrement]
  , MkField "key"         SQL_Integer [NotNull]
  , MkField "order_line"  SQL_Integer [NotNull]
  ]
  [ ForeignKey "order_line" "priced_order_line" "id"
  , Unique "unique_key_lines" ["key", "order_line"]
  ]

orderLinesTable : String
orderLinesTable = renderCommand createOrderLinesTable

createProductOrderTable : Command
createProductOrderTable = CreateTable
  "priced_order"
  [ MkField "id"                SQL_Integer [PrimaryKey, AutoIncrement]
  , MkField "order_id"          SQL_Text    [NotNull]
  , MkField "customer"          SQL_Integer [NotNull]
  , MkField "shipping_address"  SQL_Integer [NotNull]
  , MkField "billing_address"   SQL_Integer [NotNull]
  , MkField "amount_to_bill"    SQL_Number  [NotNull]
  ]
  [ ForeignKey "customer"         "customer"    "id"
  , ForeignKey "shipping_address" "address"     "id"
  , ForeignKey "billing_address"  "address"     "id"
  , ForeignKey "order_lines"      "order_lines" "id"
  ]

productOrderTable : String
productOrderTable = renderCommand createProductOrderTable

logError : Error -> IO ()
logError err = case !(occured err) of
  Nothing => pure ()
  Just e  => putStrLn !(toString e)

renderMaybe : Show a => Maybe a -> String
renderMaybe Nothing  = "NULL"
renderMaybe (Just x) = show x

export
saveAddress : Database -> AddressDTO -> IO ()
saveAddress db (MkAddressDTO addressLine1 addressLine2 addressLine3 addressLine4 city zipCode) = do
  Database.run db
    ( "INSERT INTO address (line1, line2, line3, line4, city, zip)" ++
      "VALUES (\{show addressLine1},\{renderMaybe addressLine2},\{renderMaybe addressLine3},\{renderMaybe addressLine4},\{show city},\{show zipCode})")
    logError


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
  saveAddress db (MkAddressDTO "add1" Nothing Nothing Nothing "London" "PC")
  Database.close db
