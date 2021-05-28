module OrderTaking.Database.Order

import Data.String
import Service.NodeJS.SQLite
import OrderTaking.DTO.PlaceOrder
import Rango.DataTransfer.SQL.Syntax


createAddressTable : Command
createAddressTable = CreateTable
  "address"
  [ MkField "id"    SQL_Text [PrimaryKey]
  , MkField "line1" SQL_Text [NotNull]
  , MkField "line2" SQL_Text []
  , MkField "line3" SQL_Text []
  , MkField "line4" SQL_Text []
  , MkField "city"  SQL_Text [NotNull]
  , MkField "zip"   SQL_Text [NotNull]
  ]
  []

addressTable : String
addressTable = renderCommand createAddressTable

createCustomerTable : Command
createCustomerTable = CreateTable
  "customer"
  [ MkField "id"         SQL_Text [PrimaryKey]
  , MkField "first_name" SQL_Text [NotNull]
  , MkField "last_name"  SQL_Text [NotNull]
  , MkField "email"      SQL_Text [NotNull]
  ]
  [ Unique "email_unqiue" ["email"] ]

customerTable : String
customerTable = renderCommand createCustomerTable

createPricedOrderLineTable : Command
createPricedOrderLineTable = CreateTable
  "priced_order_line"
  [ MkField "id"            SQL_Text     [PrimaryKey]
  , MkField "product_code"  SQL_Text     [NotNull]
  , MkField "quantity"      SQL_Double   [NotNull]
  , MkField "price"         SQL_Double   [NotNull]
  ]
  []

pricedOrderLineTable : String
pricedOrderLineTable = renderCommand createPricedOrderLineTable

createOrderLinesTable : Command
createOrderLinesTable = CreateTable
  "priced_order_lines"
  [ MkField "ordr"        SQL_Text [NotNull]
  , MkField "order_line"  SQL_Text [NotNull]
  ]
  [ ForeignKey "order_line" "priced_order_line" "id"
  , Unique "unique_key_lines" ["ordr", "order_line"]
  ]

orderLinesTable : String
orderLinesTable = renderCommand createOrderLinesTable

createPricedOrderTable : Command
createPricedOrderTable = CreateTable
  "priced_order"
  [ MkField "id"                SQL_Text    [PrimaryKey]
  , MkField "customer"          SQL_Text    [NotNull]
  , MkField "shipping_address"  SQL_Text    [NotNull]
  , MkField "billing_address"   SQL_Text    [NotNull]
  , MkField "amount_to_bill"    SQL_Double  [NotNull]
  ]
  [ ForeignKey "customer"         "customer"    "id"
  , ForeignKey "shipping_address" "address"     "id"
  , ForeignKey "billing_address"  "address"     "id"
  ]

pricedOrderTable : String
pricedOrderTable = renderCommand createPricedOrderTable

logError : String -> Error -> IO ()
logError sql err = case !(occured err) of
  Nothing => pure ()
  Just e  => do
--    putStrLn sql
    putStrLn !(toString e)

renderMaybe : Show a => Maybe a -> String
renderMaybe Nothing  = "NULL"
renderMaybe (Just x) = show x

export
saveAddress : Database -> AddressDTO -> IO ()
saveAddress db (MkAddressDTO identifier addressLine1 addressLine2 addressLine3 addressLine4 city zipCode) = do
  Database.run db
    ( "INSERT INTO address (id, line1, line2, line3, line4, city, zip)" ++
      "VALUES (\{show identifier},\{show addressLine1},\{renderMaybe addressLine2},\{renderMaybe addressLine3},\{renderMaybe addressLine4},\{show city},\{show zipCode})")
    logError

export
saveCustomer : Database -> CustomerDTO -> IO ()
saveCustomer db (MkCustomerDTO identifier firstName lastName emailAddress) = do
  Database.run db
    ( "INSERT INTO customer (id, first_name, last_name, email)" ++
      "VALUES (\{show identifier},\{show firstName},\{show lastName}, \{show emailAddress})")
    logError

export
savePricedOrderLine : Database -> PricedOrderLineDTO -> IO ()
savePricedOrderLine db (MkPricedOrderLineDTO identifier productCode quantity price) = do
  Database.run db
    ( "INSERT INTO priced_order_line (id,product_code,quantity,price)" ++
      "VALUES (\{show identifier},\{show productCode},\{show quantity},\{show price})" )
    logError

export
saveOrder : Database -> PricedOrderDTO -> IO ()
saveOrder db (MkPricedOrderDTO identifier customer shippingAddress billingAddress orderLines amount) = do
  saveCustomer db customer
  saveAddress db shippingAddress
  saveAddress db billingAddress
  traverse_ (savePricedOrderLine db) orderLines
  Database.run db
    ( "INSERT INTO priced_order (id,customer,shipping_address,billing_address,amount_to_bill)" ++
      "VALUES (\{show identifier},\{show customer.identifier},\{show shippingAddress.identifier},\{show billingAddress.identifier},\{show amount})" )
    logError
  for_ orderLines $ \(MkPricedOrderLineDTO orderIdentifier productCode quantity price) => do
    Database.run db
      ( "INSERT INTO priced_order_lines (ordr, order_line)" ++
        "VALUES (\{show identifier},\{show orderIdentifier})")
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
  Database.run db pricedOrderTable      logError
  saveOrder db
    $ MkPricedOrderDTO
      { identifier = "ID1"
      , customer =
        MkCustomerDTO
        { identifier = "ID2"
        , firstName = "Jack"
        , lastName = "Doe"
        , emailAddress = "jack.doe@gmail.com"
        }
      , shippingAddress =
        MkAddressDTO
        { identifier = "ID3"
        , addressLine1 = "SOME ADDRESS"
        , addressLine2 = Nothing
        , addressLine3 = Nothing
        , addressLine4 = Nothing
        , city = "GREAT"
        , zipCode = "ZP-31683"
        }
      , billingAddress =
        MkAddressDTO
        { identifier = "ID3"
        , addressLine1 = "SOME ADDRESS"
        , addressLine2 = Nothing
        , addressLine3 = Nothing
        , addressLine4 = Nothing
        , city = "GREAT"
        , zipCode = "ZP-31683"
        }
      , orderLines =
        [ MkPricedOrderLineDTO
          { identifier = "ID4"
          , productCode = "W1204"
          , quantity = 1.1
          , price = 100.1
          }
        ]
      , amount = 100.1
      }
  Database.close db
