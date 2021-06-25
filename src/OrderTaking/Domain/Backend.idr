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

namespace FromUpstream

  export
  orderForm : OrderFormDTO -> OrderForm

  orderLineForm    : OrderLineFormDTO -> OrderLineForm
  addressForm      : AddressFormDTO   -> AddressForm
  customerInfoForm : CustomerFormDTO  -> CustomerInfoForm


  orderForm dto = MkOrderForm
    { customerInfo    = customerInfoForm dto.customer
    , shippingAddress = addressForm dto.shippingAddress
    , billingAddress  = addressForm dto.billingAddress
    , orderLines      = map orderLineForm dto.orderLines
    }

  customerInfoForm dto = MkCustomerInfoForm
    { firstName    = dto.firstName
    , lastName     = dto.lastName
    , emailAddress = dto.emailAddress
    }

  nonEmpty : String -> Maybe String
  nonEmpty "" = Nothing
  nonEmpty xs = Just xs

  addressForm dto = MkAddressForm
    { addressLine1 = dto.addressLine1
    , addressLine2 = nonEmpty dto.addressLine2
    , addressLine3 = nonEmpty dto.addressLine3
    , addressLine4 = nonEmpty dto.addressLine4
    , city         = dto.city
    , zipCode      = dto.zipCode
    }

  orderLineForm dto = MkOrderLineForm
    { productCode = dto.productCode
    , quantity    = dto.quantity
    }

namespace ToDownstream

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

namespace WriteModel

  record Customer where
    constructor MkCustomer

namespace ReadModel 

  record Customer where
    constructor MkCustomer

record OrderDB where
  constructor MkOrderDB
  dbConnection        : Type 
  dbError             : Type
  showDBError         : dbError -> String
  initConnection      : Promise (Either dbError dbConnection)
  closeConnection     : dbConnection -> Promise (Maybe dbError)
  beginTransaction    : dbConnection -> Promise (Maybe dbError)
  commitTransaction   : dbConnection -> Promise (Maybe dbError)
  rollbackTransaction : dbConnection -> Promise (Maybe dbError)
  saveOrder           : dbConnection -> PricedOrderDTO -> Promise (Maybe dbError)
  -- saveCustomer : dbConnection -> WriteModel.Cusomter -> Promise (Maybe dbError)
  -- loadCusomter : dbConnection -> Promise (Either dbError ReadModel.Customer)

{-
• If the inner type is an DDD Entity, with its own identity, it should be
stored in a separate table.
• If the inner type is a DDD Value Object, without its own identity, it should
be stored “inline” with the parent data.
-}


export
orderDBSQLite : OrderDB
orderDBSQLite = MkOrderDB
  { dbConnection        = Database
  , dbError             = OrderDBError
  , showDBError         = show
  , initConnection      = do
      sqlite <- SQLite.require
      map (mapFst (InitializeError . show)) $ SQLite.database sqlite "./db/order.db"
  , closeConnection     = \db => Nothing <$ SQLite.Database.close db
  , beginTransaction    = \db => map (map (InitializeError . show) . toMaybe) $ SQLite.Database.run db "begin"
  , commitTransaction   = \db => map (map (InitializeError . show) . toMaybe) $ SQLite.Database.run db "commit"
  , rollbackTransaction = \db => map (map (InitializeError . show) . toMaybe) $ SQLite.Database.run db "rollback"
  , saveOrder           = \db, po => map (either Just (const Nothing)) $ runOrderDB $ Order.saveOrder db po
  }

record ProductDBComp where
  constructor MkProductDBComp
  dbConnection        : Type 
  dbError             : Type
  showDBError         : dbError -> String
  initConnection      : Promise (Either dbError dbConnection)
  closeConnection     : dbConnection -> Promise (Maybe dbError)
  beginTransaction    : dbConnection -> Promise (Maybe dbError)
  commitTransaction   : dbConnection -> Promise (Maybe dbError)
  rollbackTransaction : dbConnection -> Promise (Maybe dbError)

-- TODO: Implement
export
productDBSQlite : ProductDBComp
productDBSQlite = MkProductDBComp
  { dbConnection        = ()
  , dbError             = String
  , showDBError         = id
  , initConnection      = pure (Right ())
  , closeConnection     = \db => pure Nothing
  , beginTransaction    = \db => pure Nothing
  , commitTransaction   = \db => pure Nothing
  , rollbackTransaction = \db => pure Nothing
  }

record Dependencies where
  constructor MkDependencies
  md5Provider   : MD5
  orderDBComp   : OrderDB
  orderDBConn   : orderDBComp.dbConnection
  productDBComp : ProductDBComp
  productDBConn : productDBComp.dbConnection

orderDBDep : Dependencies -> (o : OrderDB ** o.dbConnection)
orderDBDep d = (d.orderDBComp ** d.orderDBConn)

productDBDep : Dependencies -> (p : ProductDBComp ** p.dbConnection)
productDBDep d = (d.productDBComp ** d.productDBConn) 

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
mkRunBackend
  :  (orderDBComp   : OrderDB)
  => (productDBComp : ProductDBComp)
  => IO RunBackend
mkRunBackend = do
  sqlite <- SQLite.require
  md5    <- MD5.require
  pure $ MkRunBackend $ \type, script => do
    -- Open DB conenctions
    orderDBConn
      <- Promise.either
          $ mapFst (OrderDB.showDBError orderDBComp)
          $ !(OrderDB.initConnection orderDBComp)
    productDBConn
      <- Promise.either
          $ mapFst (ProductDBComp.showDBError productDBComp)
          $ !(ProductDBComp.initConnection productDBComp)
    
    -- Initialise transactions
    Nothing <- orderDBComp.beginTransaction orderDBConn
      | Just err => Promise.reject $ orderDBComp.showDBError err
    Nothing <- productDBComp.beginTransaction productDBConn
      | Just err => Promise.reject $ productDBComp.showDBError err

    -- Run the backend computation
    let conn = MkDependencies md5 orderDBComp orderDBConn productDBComp productDBConn
    x <- runReaderT conn (runEitherT (backend script))
    case x of
      Left _ => do
        ignore $ orderDBComp.rollbackTransaction orderDBConn
        ignore $ productDBComp.rollbackTransaction productDBConn
      Right _ => do
        ignore $ orderDBComp.commitTransaction orderDBConn
        ignore $ productDBComp.commitTransaction productDBConn
    
    -- Try to close the connections
    ignore $ productDBComp.closeConnection productDBConn
    ignore $ orderDBComp.closeConnection orderDBConn
    pure (the (Either PlaceOrderError type) x)

generateIdentifier : HasIO io => MD5 -> io String
generateIdentifier md5 = do
  n   <- Date.now
  -- d   <- map (the Double) randomIO
  let d = 1.0
  MD5.create md5 (show n ++ show d)

namespace Model

  newOrderId : Backend OrderId
  newOrderId = MkOrderId <$> (generateIdentifier !(asks md5Provider))

  newOrderLineId : Backend OrderLineId
  newOrderLineId = MkOrderLineId <$> (generateIdentifier !(asks md5Provider))

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
    (orderDB ** orderDBConn) <- asks orderDBDep 
    Nothing <- liftPromise $ orderDB.saveOrder orderDBConn (toPricedOrderDTO pricedOrder)
      | Just err => throwError $ MkPlaceOrderError $ orderDB.showDBError err
    pure ()

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
