module BoundedContext.OrderTaking.Workflow.PlaceOrder.Database.Order

import public Control.Monad.Either

import BoundedContext.OrderTaking.Workflow.PlaceOrder.Database.DTO

import Data.String
import Control.Monad.Trans
import Control.Monad.Reader

import Rango.DataTransfer.SQL.Syntax
import Rango.Database.SQLite
import Service.NodeJS.SQLite
import Service.NodeJS.Promise


||| Value like table, meaning that it is highly redundant
addressTable : Table
addressTable = MkTable
  "address"
  [ field "id"    SQL_Text [PrimaryKey]
  , field "line1" SQL_Text [NotNull]
  , field "line2" SQL_Text []
  , field "line3" SQL_Text []
  , field "line4" SQL_Text []
  , field "city"  SQL_Text [NotNull]
  , field "zip"   SQL_Text [NotNull]
  ]
  []
  YesOfCourseValid

customerTable : Table
customerTable = MkTable
  "customer"
  [ field "id"         SQL_Text [PrimaryKey]
  , field "first_name" SQL_Text [NotNull]
  , field "last_name"  SQL_Text [NotNull]
  , field "email"      SQL_Text [NotNull]
  ]
  [ Unique "email_unqiue" ["email"] ]
  YesOfCourseValid

pricedOrderLineTable : Table
pricedOrderLineTable = MkTable
  "priced_order_line"
  [ field "id"            SQL_Text     [PrimaryKey]
  , field "product_code"  SQL_Text     [NotNull]
  , field "quantity"      SQL_Double   [NotNull]
  , field "price"         SQL_Double   [NotNull]
  ]
  []
  YesOfCourseValid

pricedOrderLinesTable : Table
pricedOrderLinesTable = MkTable
  "priced_order_lines"
  [ field "ordr"        SQL_Text [NotNull]
  , field "order_line"  SQL_Text [NotNull]
  ]
  [ ForeignKey "order_line" "priced_order_line" "id"
  , Unique "unique_key_lines" ["ordr", "order_line"]
  ]
  YesOfCourseValid

pricedOrderTable : Table
pricedOrderTable = MkTable
  "priced_order"
  [ field "id"                SQL_Text    [PrimaryKey]
  , field "customer"          SQL_Text    [NotNull]
  , field "shipping_address"  SQL_Text    [NotNull]
  , field "billing_address"   SQL_Text    [NotNull]
  , field "amount_to_bill"    SQL_Double  [NotNull]
  ]
  [ ForeignKey "customer"         "customer"    "id"
  , ForeignKey "shipping_address" "address"     "id"
  , ForeignKey "billing_address"  "address"     "id"
  ]
  YesOfCourseValid

public export
data OrderDBError
  = SaveAddressError String
  | SaveCustomerError String
  | SavePricedOrderLineError String
  | SaveOrderError String
  | InitializeError String

export
Show OrderDBError where
  showPrec d (SaveAddressError          x) = "SaveAddressError: " ++ x 
  showPrec d (SaveCustomerError         x) = "SaveCustomerError: " ++ x
  showPrec d (SavePricedOrderLineError  x) = "SavePricedOrderLineError: " ++ x
  showPrec d (SaveOrderError            x) = "SaveOrderError: " ++ x
  showPrec d (InitializeError           x) = "InitializeError: " ++ x

export
OrderDB : Type -> Type
OrderDB a = EitherT OrderDBError (ReaderT Database Promise) a

export
runOrderDB : Database -> OrderDB a -> Promise (Either OrderDBError a)
runOrderDB db m = runReaderT db (runEitherT m)

throwIfFail : (String -> OrderDBError) -> Promise SomeError -> OrderDB ()
throwIfFail mkError p = do
  NoError <- lift (lift p)
    | HasError err => throwError $ mkError !(toString err)
  pure ()

export
saveAddress : AddressDTO -> OrderDB ()
saveAddress (MkAddressDTO identifier addressLine1 addressLine2 addressLine3 addressLine4 city zipCode) = do
  db <- ask
  throwIfFail SaveAddressError $ Database.run db $ renderCommand $
    Insert addressTable
      [ FieldOf "id"     (SQLText identifier)
      , FieldOf "line1"  (SQLText addressLine1)
      , FieldOf "line2"  (SQLText <$> addressLine2)
      , FieldOf "line3"  (SQLText <$> addressLine3)
      , FieldOf "line4"  (SQLText <$> addressLine4)
      , FieldOf "city"   (SQLText city)
      , FieldOf "zip"    (SQLText zipCode)
      ]

export
saveCustomer : CustomerDTO -> OrderDB ()
saveCustomer (MkCustomerDTO identifier firstName lastName emailAddress) = do
  db <- ask
  throwIfFail SaveCustomerError $ Database.run db $ renderCommand $
    Insert customerTable
      [ FieldOf "id"          (SQLText emailAddress)
      , FieldOf "first_name"  (SQLText firstName)
      , FieldOf "last_name"   (SQLText lastName)
      , FieldOf "email"       (SQLText emailAddress)
      ]

export
savePricedOrderLine : PricedOrderLineDTO -> OrderDB ()
savePricedOrderLine (MkPricedOrderLineDTO identifier productCode quantity price) = do
  db <- ask
  throwIfFail SavePricedOrderLineError $ Database.run db $ renderCommand $
    Insert pricedOrderLineTable
      [ FieldOf "id"            (SQLText identifier)
      , FieldOf "product_code"  (SQLText productCode)
      , FieldOf "quantity"      (SQLDouble quantity)
      , FieldOf "price"         (SQLDouble price)
      ]

export
saveOrder : PricedOrderDTO -> OrderDB ()
saveOrder (MkPricedOrderDTO identifier customer shippingAddress billingAddress orderLines amount) = do
  db <- ask
  saveCustomer customer
  saveAddress shippingAddress
  saveAddress billingAddress
  traverse_ savePricedOrderLine orderLines
  throwIfFail SaveOrderError $ Database.run db $ renderCommand $
    Insert pricedOrderTable
      [ FieldOf "id"               (SQLText identifier)
      , FieldOf "customer"         (SQLText customer.identifier)
      , FieldOf "shipping_address" (SQLText shippingAddress.identifier)
      , FieldOf "billing_address"  (SQLText billingAddress.identifier)
      , FieldOf "amount_to_bill"   (SQLDouble amount)
      ]
  for_ orderLines $ \(MkPricedOrderLineDTO orderIdentifier productCode quantity price) => do
    throwIfFail SaveOrderError $ Database.run db $ renderCommand $
      Insert pricedOrderLinesTable
        [ FieldOf "ordr"        (SQLText identifier)
        , FieldOf "order_line"  (SQLText orderIdentifier)
        ]

export
initDB : IO ()
initDB = do
  sqlite <- SQLite.require
  resolve' (\_ => putStrLn "OK.") putStrLn $ do
    db <- either !(SQLite.database sqlite "./db/order.db")
    ignore $ command db $ CreateTable addressTable
    ignore $ command db $ CreateTable customerTable
    ignore $ command db $ CreateTable pricedOrderLineTable
    ignore $ command db $ CreateTable pricedOrderLinesTable
    ignore $ command db $ CreateTable pricedOrderTable
    ignore $ Database.close db
