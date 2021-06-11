module OrderTaking.Database.Order

import public Control.Monad.Either

import Data.String
import OrderTaking.DTO.PlaceOrder
import Rango.DataTransfer.SQL.Syntax
import Control.Monad.Trans

import Service.NodeJS.SQLite
import Service.NodeJS.Promise


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

renderMaybe : Show a => Maybe a -> String
renderMaybe Nothing  = "NULL"
renderMaybe (Just x) = show x

public export
data OrderDBError
  = SaveAddressError String
  | SaveCustomerError String
  | SavePricedOrderLineError String
  | SaveOrderError String

export
Show OrderDBError where
  showPrec d (SaveAddressError          x) = "SaveAddressError: " ++ x 
  showPrec d (SaveCustomerError         x) = "SaveCustomerError: " ++ x
  showPrec d (SavePricedOrderLineError  x) = "SavePricedOrderLineError: " ++ x
  showPrec d (SaveOrderError            x) = "SaveOrderError: " ++ x

export
OrderDB : Type -> Type
OrderDB a = EitherT OrderDBError Promise a

export
runOrderDB : OrderDB a -> Promise (Either OrderDBError a)
runOrderDB = runEitherT

throwIfFail : (String -> OrderDBError) -> Promise (Maybe Error) -> OrderDB ()
throwIfFail mkError p = do
  Nothing <- lift p
    | Just err => throwError $ mkError !(toString err)
  pure ()

export
saveAddress : Database -> AddressDTO -> OrderDB ()
saveAddress db (MkAddressDTO identifier addressLine1 addressLine2 addressLine3 addressLine4 city zipCode) = do
  throwIfFail SaveAddressError $ Database.runP db
    ( "INSERT INTO address (id, line1, line2, line3, line4, city, zip)" ++
      "VALUES (\{show identifier},\{show addressLine1},\{renderMaybe addressLine2},\{renderMaybe addressLine3},\{renderMaybe addressLine4},\{show city},\{show zipCode})")

export
saveCustomer : Database -> CustomerDTO -> OrderDB ()
saveCustomer db (MkCustomerDTO identifier firstName lastName emailAddress) = do
  throwIfFail SaveCustomerError $ Database.runP db
    ( "INSERT INTO customer (id, first_name, last_name, email)" ++
      "VALUES (\{show identifier},\{show firstName},\{show lastName}, \{show emailAddress})")

export
savePricedOrderLine : Database -> PricedOrderLineDTO -> OrderDB ()
savePricedOrderLine db (MkPricedOrderLineDTO identifier productCode quantity price) = do
  throwIfFail SavePricedOrderLineError $ Database.runP db
    ( "INSERT INTO priced_order_line (id,product_code,quantity,price)" ++
      "VALUES (\{show identifier},\{show productCode},\{show quantity},\{show price})" )

export
saveOrder : Database -> PricedOrderDTO -> OrderDB ()
saveOrder db (MkPricedOrderDTO identifier customer shippingAddress billingAddress orderLines amount) = do
  saveCustomer db customer
  saveAddress db shippingAddress
  saveAddress db billingAddress
  traverse_ (savePricedOrderLine db) orderLines
  throwIfFail SaveOrderError $ Database.runP db
    ( "INSERT INTO priced_order (id,customer,shipping_address,billing_address,amount_to_bill)" ++
      "VALUES (\{show identifier},\{show customer.identifier},\{show shippingAddress.identifier},\{show billingAddress.identifier},\{show amount})" )
  for_ orderLines $ \(MkPricedOrderLineDTO orderIdentifier productCode quantity price) => do
    throwIfFail SaveOrderError $ Database.runP db
      ( "INSERT INTO priced_order_lines (ordr, order_line)" ++
        "VALUES (\{show identifier},\{show orderIdentifier})")

export
initDB : IO ()
initDB = do
  sqlite <- SQLite.require
  db <- SQLite.database sqlite "./db/order.db"
  resolve' (\_ => putStrLn "OK.") putStrLn $ do
    ignore $ Database.runP db addressTable
    ignore $ Database.runP db customerTable
    ignore $ Database.runP db pricedOrderLineTable
    ignore $ Database.runP db orderLinesTable
    ignore $ Database.runP db pricedOrderTable
    ignore $ Database.close db
