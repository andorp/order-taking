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
import Service.NodeJS.Random

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

namespace ToDatabase

  orderIdentifier         : OrderId     -> Identifier
  orderLineIdentifier     : OrderLineId -> Identifier
  fromPrice               : Price       -> Double
  toCustomerDTO           : Identifier  -> CustomerInfo    -> CustomerDTO
  toAddressDTO            : Identifier  -> Address         -> AddressDTO
  fromBillingAddress      : Identifier  -> BillingAddress  -> AddressDTO
  fromShippingAddress     : Identifier  -> ShippingAddress -> AddressDTO
  toPricedOrderLineDTO    : Identifier  -> PricedOrderLine -> PricedOrderLineDTO
  export toPricedOrderDTO : PricedOrder -> PricedOrderDTO

  -- Order

  orderIdentifier     (MkOrderId x)     = x
  orderLineIdentifier (MkOrderLineId x) = x

  toPricedOrderDTO p
    = let oid = orderIdentifier p.orderId
      in MkPricedOrderDTO
          { identifier      = oid
          , customer        = toCustomerDTO                  oid p.customerInfo
          , shippingAddress = fromShippingAddress            oid p.shippingAddress
          , billingAddress  = fromBillingAddress             oid p.billingAddress
          , orderLines      = map (toPricedOrderLineDTO oid) p.orderLines
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
      , productCode = value po.orderLine.productCode
      , quantity    = OrderQuantity.value po.orderLine.quantity
      , price       = fromPrice po.price
      }

  -- Product

  export toProductCodeDTO : ProductCode -> ProductCodeDTO
  export toProductDTO : Product -> ProductDTO

  toProductCodeDTO (WidgetProduct (MkWidgetCode x)) = MkProductCodeDTO x
  toProductCodeDTO (GizmoProduct (MkGizmoCode x))   = MkProductCodeDTO x

  toProductDTO (MkProduct productCode price description)
    = MkProductDTO
      { productCode = toProductCodeDTO productCode
      , price       = Price.value price
      , description = StringN.value description
      }

namespace ToDownstream

  export toPlaceOrderEventDTO   : PlacedOrderEvent -> PlaceOrderEventDTO
  export toPlacedOrderErrorDTO  : PlaceOrderError -> PlaceOrderErrorDTO

  toPricedOrderDsDTO            : PricedOrder -> PricedOrderDsDTO
  toPricedOrderLineDsDTO        : PricedOrderLine -> PricedOrderLineDsDTO
  toAddressDsDTO                : Address -> AddressDsDTO
  toBillableOrderPlacedDTO      : BillableOrderPlaced -> BillableOrderPlacedDTO
  toAcknowledgementSentDTO      : OrderAcknowledgementSent -> OrderAcknowledgementSentDTO
  toAddressValidationErrorDTO   : AddressValidationError -> AddressValidationErrorDTO
  toNameValidationErrorDTO      : NameValidationError -> NameValidationErrorDTO
  toEmailValidationErrorDTO     : EmailValidationError -> EmailValidationErrorDTO
  toQuantityValidationErrorDTO  : QuantityValidationError -> QuantityValidationErrorDTO
  toValidationErrorDTO          : ValidationError -> ValidationErrorDTO
  toProductCodeErrorDTO         : ProductCodeErr -> ProductCodeErrDTO
  toInvalidOrderDTO             : InvalidOrder -> InvalidOrderDTO
  toPricingErrorDTO             : PricingError -> PricingErrorDTO
  toRemoteServiceErrorDTO       : RemoteServiceError -> RemoteServiceErrorDTO

  toPlaceOrderEventDTO (OrderPlacedEvent         x) = OrderPlacedEvent          (toPricedOrderDsDTO x)
  toPlaceOrderEventDTO (BillableOrderPlacedEvent x) = BillableOrderPlacedEvent  (toBillableOrderPlacedDTO x)
  toPlaceOrderEventDTO (AcknowledgementSentEvent x) = AcknowledgementSentEvent  (toAcknowledgementSentDTO x)
  toPlaceOrderEventDTO (InvalidOrderRegistered   x) = InvalidOrderRegistered    (toInvalidOrderDTO x)

  toPricedOrderDsDTO po = MkPricedOrderDsDTO
    { orderId       = value po.orderId
    , orderLines    = map toPricedOrderLineDsDTO po.orderLines
    , amountToBill  = value po.amountToBill
    }

  toPricedOrderLineDsDTO pol = MkPricedOrderLineDsDTO
    { orderLineId = value pol.orderLine.orderLineId
    , productCode = value pol.orderLine.productCode
    , price       = value pol.price
    }

  toAddressDsDTO a = MkAddressDsDTO
    { addressLine1 = value a.addressLine1
    , addressLine2 = maybe "" value a.addressLine2
    , addressLine3 = maybe "" value a.addressLine3
    , addressLine4 = maybe "" value a.addressLine4
    , city         = value a.city
    , zipCode      = value a.zipCode
    }

  toBillableOrderPlacedDTO bop = MkBillableOrderPlacedDTO
    { orderId         = value bop.orderId
    , billingAddress  = toAddressDsDTO bop.billingAddress.address
    , amountToBill    = value bop.amountToBill
    }

  toAcknowledgementSentDTO as = MkOrderAcknowledgementSentDTO
    { orderId      = value as.orderId
    , emailAddress = value as.emailAddress
    }

  toCheckedAddressValidationErrorDTO : CheckedAddressValidationError -> CheckedAddressValidationErrorDTO
  toCheckedAddressValidationErrorDTO (InvalidFormat   x) = InvalidFormat x
  toCheckedAddressValidationErrorDTO (AddressNotFound x) = AddressNotFound x

  toAddressValidationErrorDTO (MkAddressLineError     x) = MkAddressLineError x
  toAddressValidationErrorDTO (MkAddressOptLineError  x) = MkAddressOptLineError x
  toAddressValidationErrorDTO (MkAddressCityError     x) = MkAddressCityError x
  toAddressValidationErrorDTO (MkAddressZipCodeError  x) = MkAddressZipCodeError x
  toAddressValidationErrorDTO (CheckedAddressError    x) = CheckedAddressError (toCheckedAddressValidationErrorDTO x)

  toNameValidationErrorDTO vne = MkNameValidationErrorDTO
    { field = vne.field
    , value = vne.value
    }
  
  toEmailValidationErrorDTO (MkEmailValidationError message)
    = MkEmailValidationErrorDTO message

  toQuantityValidationErrorDTO (MkQuantityValidationError condition message)
    = MkQuantityValidationErrorDTO condition message

  toValidationErrorDTO (AddressValidation   x) = AddressValidation  (toAddressValidationErrorDTO x)
  toValidationErrorDTO (NameValidation      x) = NameValidation     (toNameValidationErrorDTO x)
  toValidationErrorDTO (EmailValidation     x) = EmailValidation    (toEmailValidationErrorDTO x)
  toValidationErrorDTO (QuantityValidation  x) = QuantityValidation (toQuantityValidationErrorDTO x)

  toProductCodeErrorDTO (MkProductCodeErr x) = MkProductCodeErr x

  toInvalidOrderDTO io = MkInvalidOrderDTO
    { validationErrors = map toValidationErrorDTO io.validationErrors
    , productCodeErrors = map toProductCodeErrorDTO io.productCodeErrors
    }

  toPricingErrorDTO (MkPricingError message) = MkPricingError message
  
  toRemoteServiceErrorDTO (MkRemoteServiceError (MkServiceInfo name endpoint) (MkRemoteServiceException message))
    = MkRemoteServiceErrorDTO name message

  toPlacedOrderErrorDTO (MkPlaceOrderError x) = MkPlaceOrderError x
  toPlacedOrderErrorDTO (ValidationErrors xs) = ValidationErrors  $ map toValidationErrorDTO xs
  toPlacedOrderErrorDTO (ProductCodeError x)  = ProductCodeError  $ toProductCodeErrorDTO x
  toPlacedOrderErrorDTO (PriceOrderError x)   = PriceOrderError   $ toPricingErrorDTO x
  toPlacedOrderErrorDTO (RemoteServiceErr x)  = RemoteServiceErr  $ toRemoteServiceErrorDTO x

-- namespace WriteModel

--   record Customer where
--     constructor MkCustomer

-- namespace ReadModel 

--   record Customer where
--     constructor MkCustomer

{-
* If the inner type is an DDD Entity, with its own identity, it should be
stored in a separate table.
* If the inner type is a DDD Value Object, without its own identity, it should
be stored “inline” with the parent data.
-}

record OrderDBComp where
  constructor MkOrderDBComp
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
  productPrice        : dbConnection -> ProductCodeDTO -> Promise (Either dbError Double)
  productExists       : dbConnection -> ProductCodeDTO -> Promise (Either dbError Bool)

record EmailComp where
  constructor MkEmailComp
  emailError  : Type
  showError   : emailError -> String
  serviceInfo : ServiceInfo
  send        : EmailAddress -> HtmlString -> Promise (Maybe emailError)

data CheckAddressResult
  = ValidAddress
  | NotFound String
  | InvalidAddress String

record CheckAddressComp where
  constructor MkCheckAddressComp
  addressError : Type
  showError    : addressError -> String
  serviceInfo  : ServiceInfo
  checkAddress : AddressForm -> Promise (Either addressError CheckAddressResult)

record Dependencies where
  constructor MkDependencies
  md5Provider       : MD5
  orderDBComp       : OrderDBComp
  orderDBConn       : orderDBComp.dbConnection
  productDBComp     : ProductDBComp
  productDBConn     : productDBComp.dbConnection
  emailComp         : EmailComp
  checkAddressComp  : CheckAddressComp

orderDBDep : Dependencies -> (o : OrderDBComp ** o.dbConnection)
orderDBDep d = (d.orderDBComp ** d.orderDBConn)

productDBDep : Dependencies -> (p : ProductDBComp ** p.dbConnection)
productDBDep d = (d.productDBComp ** d.productDBConn) 

||| Backend monad for the PlaceOrder workflow.
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
  :  (orderDBComp       : OrderDBComp)
  => (productDBComp     : ProductDBComp)
  => (emailComp         : EmailComp)
  => (checkAddressComp  : CheckAddressComp)
  => HasIO io
  => io RunBackend
mkRunBackend = do
  sqlite <- SQLite.require
  md5    <- MD5.require
  pure $ MkRunBackend $ \type, script => do
    -- Open DB conenctions
    orderDBConn
      <- Promise.either
          $ mapFst (OrderDBComp.showDBError orderDBComp)
          $ !(OrderDBComp.initConnection orderDBComp)
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
    let dependencies =
          MkDependencies
            { md5Provider       = md5
            , orderDBComp       = orderDBComp
            , orderDBConn       = orderDBConn
            , productDBComp     = productDBComp
            , productDBConn     = productDBConn
            , emailComp         = emailComp
            , checkAddressComp  = checkAddressComp
            }
    x <- runReaderT dependencies (runEitherT (backend script))
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

namespace Model

  generateIdentifier : HasIO io => MD5 -> io String
  generateIdentifier md5 = do
    n <- Date.now
    d <- Random.double
    idx <- MD5.create md5 (show n ++ show d)
    putStrLn idx
    pure idx

  newOrderId : Backend OrderId
  newOrderId = MkOrderId <$> (generateIdentifier !(asks md5Provider))

  newOrderLineId : Backend OrderLineId
  newOrderLineId = MkOrderLineId <$> (generateIdentifier !(asks md5Provider))

  checkProductCodeExists : ProductCode -> Backend Bool
  checkProductCodeExists p = do
    (productDB ** productDBConn) <- asks productDBDep
    Right answer <- liftPromise $ productDB.productExists productDBConn (toProductCodeDTO p)
      | Left err => throwError $ ProductCodeError $ MkProductCodeErr $ productDB.showDBError err
    pure answer
  
  checkAddressExists : AddressForm -> Backend (Either CheckedAddressValidationError CheckedAddress)
  checkAddressExists addressForm = do
    comp <- asks checkAddressComp
    Right res <- liftPromise $ comp.checkAddress addressForm
      | Left err => throwError
                  $ RemoteServiceErr
                  $ MkRemoteServiceError comp.serviceInfo
                  $ MkRemoteServiceException
                  $ comp.showError err
    pure $ case res of
      ValidAddress        => Right (MkCheckedAddress addressForm)
      NotFound msg        => Left (AddressNotFound msg)
      InvalidAddress msg  => Left (InvalidFormat msg)

  getProductPrice : ProductCode -> Backend Price
  getProductPrice p = do
    (productDB ** productDBConn) <- asks productDBDep
    Right priceValue <- liftPromise $ productDB.productPrice productDBConn (toProductCodeDTO p)
      | Left err      => throwError $ ProductCodeError $ MkProductCodeErr $ productDB.showDBError err
    let Right price = Price.create priceValue
        | Left err    => throwError $ ProductCodeError $ MkProductCodeErr err
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
    comp <- asks emailComp
    Nothing <- liftPromise $ comp.send orderAcknowledgement.emailAddress orderAcknowledgement.letter
      | Just err => throwError
                  $ RemoteServiceErr
                  $ MkRemoteServiceError comp.serviceInfo
                  $ MkRemoteServiceException
                  $ comp.showError err
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

namespace Components

  export
  productDBSQlite : ProductDBComp
  productDBSQlite = MkProductDBComp
    { dbConnection        = Database
    , dbError             = ProductDBError
    , showDBError         = show
    , initConnection      = do
        sqlite <- SQLite.require
        map (mapFst (InitializeError . show)) $ SQLite.database sqlite "./db/product.db"
    , closeConnection     = \db => Nothing <$ SQLite.Database.close db
    , beginTransaction    = \db => map (map (InitializeError . show) . toMaybe) $ SQLite.Database.run db "begin"
    , commitTransaction   = \db => map (map (InitializeError . show) . toMaybe) $ SQLite.Database.run db "commit"
    , rollbackTransaction = \db => map (map (InitializeError . show) . toMaybe) $ SQLite.Database.run db "rollback"
    , productPrice        = \db, pc => runProductDB db $ Product.productPrice  pc
    , productExists       = \db, pc => runProductDB db $ Product.productExists pc
    }

  export
  orderDBSQLite : OrderDBComp
  orderDBSQLite = MkOrderDBComp
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
    , saveOrder           = \db, po => map (either Just (const Nothing)) $ runOrderDB db $ Order.saveOrder po
    }

  export
  noEmail : EmailComp
  noEmail = MkEmailComp
    { emailError  = String
    , showError   = id
    , serviceInfo = MkServiceInfo "NoOp Email" (MkUri "localhost")
    , send = \addr, html => do
        putStrLn "\{value addr} : \{value html}"
        pure Nothing
    }

  export
  okCheckAddress : CheckAddressComp
  okCheckAddress = MkCheckAddressComp
    { addressError = String
    , showError    = id
    , serviceInfo  = MkServiceInfo "NoOp Address Check" (MkUri "localhost")
    , checkAddress = \_ => pure $ Right ValidAddress
    }
