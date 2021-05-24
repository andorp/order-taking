module OrderTaking.Domain.Backend

import Data.String
import Control.Monad.Either
import Control.Monad.Reader
import System.Random

import OrderTaking.Domain.PlaceOrder
import OrderTaking.Database.Product

import Service.NodeJS.SQLite
import Service.NodeJS.MD5


record Dependencies where
  constructor MkDependencies
  md5Provider : MD5
  orderDB     : Database
  productDB   : Database

Backend : Type -> Type
Backend a = EitherT PlaceOrderError (ReaderT Dependencies IO) a

mkRunBackend : IO (Backend a -> IO (Either PlaceOrderError a))
mkRunBackend = do
  sqlite <- SQLite.require
  md5 <- MD5.require
  pure $ \script => do
    -- TODO: Bracketing on exception.
    orderDB   <- SQLite.database sqlite "./db/order.db"
    productDB <- SQLite.database sqlite "./db/product.db"
    let conn = MkDependencies md5 orderDB productDB
    x <- runReaderT conn (runEitherT script)
    SQLite.Database.close productDB
    SQLite.Database.close orderDB
    pure x

newOrderId : Backend OrderId
newOrderId = do
  d   <- map (the Double) randomIO
  md5 <- asks md5Provider
  oid <- MD5.create md5 (show d)
  pure $ MkOrderId oid

newOrderLineId : Backend OrderLineId
newOrderLineId = do
  d    <- map (the Double) randomIO
  md5  <- asks md5Provider
  olid <- MD5.create md5 (show d)
  pure $ MkOrderLineId olid

checkProductCodeExists : ProductCode -> Backend Bool
checkProductCodeExists (WidgetProduct (MkWidgetCode x)) = Database.Product.productCodeExists x
checkProductCodeExists (GizmoProduct (MkGizmoCode x))   = Database.Product.productCodeExists x

checkAddressExists : AddressForm -> Backend (Either CheckedAddressValidationError CheckedAddress)
checkAddressExists addressForm = ?cae

getProductPrice : ProductCode -> Backend Price
getProductPrice (WidgetProduct (MkWidgetCode x)) = do
  mp <- Database.Product.productPrice x
  -- TODO: Use Control.App
  pure ?wat2
getProductPrice (GizmoProduct (MkGizmoCode x)) = do
  mp <- Database.Product.productPrice x
  pure ?gpp_4

createOrderAcknowledgementLetter : PricedOrder -> Backend HtmlString
createOrderAcknowledgementLetter pricedOrder = ?coal

sendOrderAcknowledgement : OrderAcknowledgement -> Backend AckSent
sendOrderAcknowledgement orderAcknowledgement = ?soa

export
backend : Model Backend
backend = MkModel
  { throwError                       = throwError
  , catchError                       = tryError
  , newOrderId                       = newOrderId
  , newOrderLineId                   = newOrderLineId
  , checkProductCodeExists           = checkProductCodeExists
  , checkAddressExists               = checkAddressExists
  , getProductPrice                  = getProductPrice
  , createOrderAcknowledgementLetter = createOrderAcknowledgementLetter
  , sendOrderAcknowledgement         = sendOrderAcknowledgement
  }
