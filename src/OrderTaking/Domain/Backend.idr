module OrderTaking.Domain.Backend

import Control.Monad.Trans
import public Control.Monad.Either
import public Control.Monad.Reader

import Data.String
import System.Random

import OrderTaking.Database.Product
import OrderTaking.Database.Order
import OrderTaking.Domain.PlaceOrder
import OrderTaking.Domain.Prelude
import OrderTaking.DTO.PlaceOrder

import Service.NodeJS.SQLite
import Service.NodeJS.MD5
import Service.NodeJS.Date
import Service.NodeJS.Promise


namespace DTO

  orderIdentifier         : OrderId     -> Identifier
  orderLineIdentifier     : OrderLineId -> Identifier
  fromPrice               : Price       -> Double
  toCustomerDTO           : Identifier  -> CustomerInfo    -> CustomerDTO
  toAddressDTO            : Identifier  -> Address         -> AddressDTO
  fromBillingAddress      : Identifier  -> BillingAddress  -> AddressDTO
  fromShippingAddress     : Identifier  -> ShippingAddress -> AddressDTO
  toPricedOrderLineDTO    : Identifier  -> PricedOrderLine -> PricedOrderLineDTO
  export toPricedOrderDTO : PricedOrder -> PricedOrderDTO

  orderIdentifier     (MkOrderId x)     = x
  orderLineIdentifier (MkOrderLineId x) = x

  toPricedOrderDTO p
    = let oid = orderIdentifier p.orderId
      in MkPricedOrderDTO
          { identifier      = oid
          , customer        = toCustomerDTO                  oid p.customerInfo
          , shippingAddress = fromShippingAddress            oid p.shippingAddress
          , billingAddress  = fromBillingAddress             oid p.billingAddress
          , orderLines      = map (toPricedOrderLineDTO oid) p.orderLine
          , amount          = fromPrice                      p.amountToBill
          }

  fromPrice p = value p

  toCustomerDTO i c
    = MkCustomerDTO
      { identifier   = i
      , firstName    = StringN.value c.personalName.firstName
      , lastName     = StringN.value c.personalName.lastName
      , emailAddress = EmailAddress.value c.emailAddress
      }

  toAddressDTO i a
    = MkAddressDTO
      { identifier   = i
      , addressLine1 = StringN.value a.addressLine1
      , addressLine2 = map StringN.value a.addressLine2
      , addressLine3 = map StringN.value a.addressLine3
      , addressLine4 = map StringN.value a.addressLine4
      , city         = StringN.value a.city
      , zipCode      = ZipCode.value a.zipCode
      }

  fromBillingAddress  i (MkBillingAddress  ba) = toAddressDTO (i ++ "-BLN") ba
  fromShippingAddress i (MkShippingAddress sa) = toAddressDTO (i ++ "-SHP") sa

  toPricedOrderLineDTO i po
    = MkPricedOrderLineDTO
      { identifier  = i ++ "-PO-" ++ orderLineIdentifier po.orderLine.orderLineId
      , productCode = productCodeStr po.orderLine.productCode
      , quantity    = OrderQuantity.value po.orderLine.quantity
      , price       = fromPrice po.linePrice
      }


record Dependencies where
  constructor MkDependencies
  md5Provider : MD5
  orderDB     : Database
  productDB   : Database

public export
data Backend : Type -> Type where
  MkBackend : EitherT PlaceOrderError (ReaderT Dependencies Promise) a -> Backend a

backend : Backend a -> EitherT PlaceOrderError (ReaderT Dependencies Promise) a
backend (MkBackend m) = m

export
Functor Backend where
  map f (MkBackend x) = MkBackend (map f x)

export
Applicative Backend where
  pure x = MkBackend (pure x)
  (MkBackend f) <*> (MkBackend x) = MkBackend (f <*> x)

export
Monad Backend where
  (MkBackend m) >>= k = MkBackend (m >>= (backend . k))

export
HasIO Backend where
  liftIO io = MkBackend (liftIO io)

export
MonadReader Dependencies Backend where
  ask = MkBackend ask
  local f (MkBackend m) = MkBackend (local f m)

export
MonadError PlaceOrderError Backend where
  catchError (MkBackend m) f = MkBackend (catchError m (backend . f))
  throwError e = MkBackend (throwError e)

export
liftPromise : Promise a -> Backend a
liftPromise p = MkBackend (lift (lift p))

export
data RunBackend : Type where
  MkRunBackend : ((a : Type) -> Backend a -> Promise (Either PlaceOrderError a)) -> RunBackend

export
runBackend : {a : Type} -> RunBackend -> Backend a -> Promise (Either PlaceOrderError a)
runBackend {a} (MkRunBackend r) = r a

export
mkRunBackend : IO RunBackend
mkRunBackend = do
  sqlite <- SQLite.require
  md5    <- MD5.require
  pure $ MkRunBackend $ \type, script => do
    -- TODO: Bracketing on exception.
    orderDB   <- SQLite.database sqlite "./db/order.db"
    productDB <- SQLite.database sqlite "./db/product.db"
    Nothing <- SQLite.Database.runP orderDB   "begin"
      | Just err => reject !(toString err)
    Nothing <- SQLite.Database.runP productDB "begin"
      | Just err => reject !(toString err)
    let conn = MkDependencies md5 orderDB productDB
    x <- runReaderT conn (runEitherT (backend script))
    case x of
      Left _ => do
        ignore $ SQLite.Database.runP orderDB   "rollback"
        ignore $ SQLite.Database.runP productDB "rollback"
      Right _ => do
        ignore $ SQLite.Database.runP orderDB   "commit"
        ignore $ SQLite.Database.runP productDB "commit"
    SQLite.Database.close productDB
    SQLite.Database.close orderDB
    pure (the (Either PlaceOrderError type) x)

namespace Model2

  newOrderId : Backend OrderId
  newOrderId = do
    n   <- Date.now
    -- d   <- map (the Double) randomIO
    let d = 1.0
    md5 <- asks md5Provider
    oid <- MD5.create md5 (show n ++ show d)
    pure $ MkOrderId oid

  newOrderLineId : Backend OrderLineId
  newOrderLineId = do
    n    <- Date.now
    -- d    <- map (the Double) randomIO
    let d = 1.0
    md5  <- asks md5Provider
    olid <- MD5.create md5 (show n ++ show d)
    pure $ MkOrderLineId olid

  checkProductCodeExists : ProductCode -> Backend Bool
  checkProductCodeExists (WidgetProduct (MkWidgetCode x)) = Database.Product.productCodeExists x
  checkProductCodeExists (GizmoProduct (MkGizmoCode x))   = Database.Product.productCodeExists x

  checkAddressExists : AddressForm -> Backend (Either CheckedAddressValidationError CheckedAddress)
  checkAddressExists addressForm = do
    -- TODO: Address checker
    pure (Right (MkCheckedAddress addressForm))

  getProductPrice : ProductCode -> Backend Price
  getProductPrice (WidgetProduct (MkWidgetCode x)) = do
    Just priceInDB <- Database.Product.productPrice x
      | Nothing => throwError (ProductCodeError (MkProductCodeErr "Price not found in database."))
    let Right price = Price.create priceInDB
        | Left err => throwError (PriceOrderError (MkPricingError err))
    pure price
  getProductPrice (GizmoProduct (MkGizmoCode x)) = do
    Just priceInDB <- Database.Product.productPrice x
      | Nothing => throwError (ProductCodeError (MkProductCodeErr "Price not found in database."))
    let Right price = Price.create priceInDB
        | Left err => throwError (PriceOrderError (MkPricingError err))
    pure price

  placePricedOrder : PricedOrder -> Backend ()
  placePricedOrder pricedOrder = do
    db <- asks orderDB
    Right x <- liftPromise $ runOrderDB $ Order.saveOrder db (toPricedOrderDTO pricedOrder)
      | Left err => throwError $ MkPlaceOrderError $ show err
    pure x

  createOrderAcknowledgementLetter : PricedOrder -> Backend HtmlString
  createOrderAcknowledgementLetter pricedOrder = do
    -- TODO: Render some simple plain text
    pure (MkHtmlString "<HTML></HTML>")

  sendOrderAcknowledgement : OrderAcknowledgement -> Backend AckSent
  sendOrderAcknowledgement orderAcknowledgement = do
    -- TODO: Email sent...
    pure Sent

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
    , placePricedOrder                 = placePricedOrder
    , createOrderAcknowledgementLetter = createOrderAcknowledgementLetter
    , sendOrderAcknowledgement         = sendOrderAcknowledgement
    }
