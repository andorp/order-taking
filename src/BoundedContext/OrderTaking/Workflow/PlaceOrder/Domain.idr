module BoundedContext.OrderTaking.Workflow.PlaceOrder.Domain

import Rango.BoundedContext.Command

import Data.Between
import Data.Either
import Data.List
import Data.Result
import Data.StringN
import Data.String
import Data.Form

namespace Price

  export
  data Price = MkPrice Double

  export
  create : Double -> Either String Price
  create p = if p < 0.0 then Left "Zero or negative price." else Right (MkPrice p)

  export
  multiply : Price -> Double -> Price
  multiply (MkPrice x) d = MkPrice (x * d)

  export
  (.value) : Price -> Double
  (.value) (MkPrice x) = x

  export
  sumPrices : List Price -> Price
  sumPrices = MkPrice . sum . map (.value)

namespace EmailAddress

  export
  data EmailAddress = MkEmailAddress String

  export
  create : String -> Maybe EmailAddress
  create = Just . MkEmailAddress -- TODO

  export
  (.value) : EmailAddress -> String
  (.value) (MkEmailAddress e) = e

public export
record PersonalName where
  constructor MkPersonalName
  firstName : StringN 50
  lastName  : StringN 50

public export
record CustomerInfo where
  constructor MkCustomerInfo
  personalName : PersonalName
  emailAddress : EmailAddress

namespace ZipCode

  export
  data ZipCode = MkZipCode String

  export
  create : String -> Maybe ZipCode
  create = Just . MkZipCode -- TODO

  export
  (.value) : ZipCode -> String
  (.value) (MkZipCode z) = z

public export
record AddressForm where
  constructor MkAddressForm
  addressLine1 : String
  addressLine2 : Maybe String
  addressLine3 : Maybe String
  addressLine4 : Maybe String
  city         : String
  zipCode      : String

public export
record Address where
  constructor MkAddress
  addressLine1 : StringN 50
  addressLine2 : Maybe (StringN 50)
  addressLine3 : Maybe (StringN 50)
  addressLine4 : Maybe (StringN 50)
  city         : StringN 50
  zipCode      : ZipCode

public export
record ShippingAddress where
  constructor MkShippingAddress
  address : Address

public export
record BillingAddress where
  constructor MkBillingAddress
  address : Address

public export
data WidgetCode = MkWidgetCode String

mkWidgetCode : String -> Maybe WidgetCode
mkWidgetCode = Just . MkWidgetCode -- TODO

public export
data GizmoCode = MkGizmoCode String

mkGizmoCode : String -> Maybe GizmoCode
mkGizmoCode = Just . MkGizmoCode -- TODO

namespace ProductCode

  public export
  data ProductCode
    = WidgetProduct WidgetCode
    | GizmoProduct GizmoCode

  export
  mkProductCode : String -> Maybe ProductCode
  mkProductCode str = map WidgetProduct $ mkWidgetCode str

  export
  (.value) : ProductCode -> String
  (.value) (WidgetProduct (MkWidgetCode x)) = x
  (.value) (GizmoProduct (MkGizmoCode x)) = x

public export
record ProductForm where
  constructor MkProductForm
  productCode : String
  price       : Double
  description : String

public export
record Product where
  constructor MkProduct
  productCode : ProductCode
  price       : Price
  description : StringN 1000

public export
record OrderLineForm where
  constructor MkOrderLineForm
  productCode : String
  quantity    : String

public export
record CustomerInfoForm where
  constructor MkCustomerInfoForm
  firstName    : String
  lastName     : String
  emailAddress : String

public export
record OrderForm where
  constructor MkOrderForm
  customerInfo    : CustomerInfoForm
  shippingAddress : AddressForm
  billingAddress  : AddressForm
  orderLines      : List OrderLineForm

data UnitQuantity : Type where
  MkUnitQuantity
    : (Between Integer 1 1000)
    -> UnitQuantity

data KilogramQuantity : Type where
  MkKilogramQuantity
    :  (Between Double 0.0 100.0)
    -> KilogramQuantity

namespace OrderQuantity

  public export
  data OrderQuantity
    = OrderUnitQuantity     UnitQuantity
    | OrderKilogramQuantity KilogramQuantity

  export
  (.value) : OrderQuantity -> Double
  (.value) (OrderUnitQuantity     (MkUnitQuantity x))     = fromInteger $ x.value
  (.value) (OrderKilogramQuantity (MkKilogramQuantity x)) = the _ x.value -- See https://github.com/idris-lang/Idris2/issues/1760

namespace OrderId

  public export
  data OrderId = MkOrderId String

  export
  (.value) : OrderId -> String
  (.value) (MkOrderId x) = x

namespace OrderLineId

  public export
  data OrderLineId = MkOrderLineId String

  export
  (.value) : OrderLineId -> String
  (.value) (MkOrderLineId x) = x

public export
record OrderLine where
  constructor MkOrderLine
  orderLineId : OrderLineId
  productCode : ProductCode
  quantity    : OrderQuantity

-- Synonym to ValidatedOrder
public export
record Order where
  constructor MkOrder
  orderId         : OrderId
  customerInfo    : CustomerInfo
  shippingAddress : ShippingAddress
  billingAddress  : BillingAddress
  orderLines      : List OrderLine

public export
record PricedOrderLine where
  constructor MkPricedOrderLine
  orderLine : OrderLine
  price     : Price

public export
record PricedOrder where
  constructor MkPricedOrder
  orderId         : OrderId
  customerInfo    : CustomerInfo
  shippingAddress : ShippingAddress
  billingAddress  : BillingAddress
  orderLines      : List PricedOrderLine
  amountToBill    : Price

data AcknowledgementLetter = MkAcknowledgementLetter

record PlacedOrderAcknowledgement where
  constructor MkPlacedOrderAcknowledgement
  pricedOrder           : PricedOrder
  acknowledgementLetter : AcknowledgementLetter

public export
record BillableOrderPlaced where
  constructor MkBillableOrderPlaced
  orderId        : OrderId
  billingAddress : BillingAddress
  amountToBill   : Price

data EnvelopeContents = MkEnvelopeContents (List String)

data QuoteForm = MkQouteForm

data CategorizedMail
  = QuoteMail QuoteForm
  | OrderMail OrderForm

CategorizeInboundMail : Type
CategorizeInboundMail = EnvelopeContents -> CategorizedMail

data ProductCatalog = MkProductCatalog

CalculatePrices : Type
CalculatePrices = ProductCatalog => Order -> PricedOrder

public export
data CheckedAddressValidationError
  = InvalidFormat String
  | AddressNotFound String

Show CheckedAddressValidationError where
  showPrec d (InvalidFormat x) = showCon d "InvalidFormat" $ showArg x
  showPrec d (AddressNotFound x) = showCon d "AddressNotFound" $ showArg x

public export
data AddressValidationError
  = MkAddressLineError String
  | MkAddressOptLineError (Maybe String)
  | MkAddressCityError String
  | MkAddressZipCodeError String
  | CheckedAddressError CheckedAddressValidationError

Show AddressValidationError where
  showPrec d (MkAddressLineError x)     = showCon d "MkAddressLineError" $ showArg x
  showPrec d (MkAddressOptLineError x)  = showCon d "MkAddressOptLineError" $ showArg x
  showPrec d (MkAddressCityError x)     = showCon d "MkAddressCityError" $ showArg x
  showPrec d (MkAddressZipCodeError x)  = showCon d "MkAddressZipCodeError" $ showArg x
  showPrec d (CheckedAddressError x)    = showCon d "CheckedAddressError" $ showArg x

public export
record NameValidationError where
  constructor MkNameValidationError
  field : String
  value : String

Show NameValidationError where
  showPrec d (MkNameValidationError x y) = showCon d "MkNameValidationError" $ concatMap showArg [x, y]

public export
record EmailValidationError where
  constructor MkEmailValidationError
  message : String

Show EmailValidationError where
  showPrec d (MkEmailValidationError x) = showCon d "MkEmailValidationError" $ showArg x

public export
record QuantityValidationError where
  constructor MkQuantityValidationError
  condition : String
  message   : String

Show QuantityValidationError where
  showPrec d (MkQuantityValidationError x y) = showCon d "MkQuantityValidationError" $ concatMap showArg [x, y]

public export
data ValidationError
  = AddressValidation AddressValidationError
  | NameValidation NameValidationError
  | EmailValidation EmailValidationError
  | QuantityValidation QuantityValidationError

Show ValidationError where
  showPrec d (AddressValidation x)  = showCon d "AddressValidation"   $ showArg x
  showPrec d (NameValidation x)     = showCon d "NameValidation"      $ showArg x
  showPrec d (EmailValidation x)    = showCon d "EmailValidation"     $ showArg x
  showPrec d (QuantityValidation x) = showCon d "QuantityValidation"  $ showArg x

public export
data CheckedAddress = MkCheckedAddress AddressForm

namespace HtmlString

  public export
  data HtmlString = MkHtmlString String

  export
  (.value) : HtmlString -> String
  (.value) (MkHtmlString x) = x

public export
record OrderAcknowledgement where
  constructor MkOrderAcknowledgement
  emailAddress : EmailAddress
  letter       : HtmlString

public export
record OrderAcknowledgementSent where
  constructor MkOrderAcknowledgementSent
  orderId      : OrderId
  emailAddress : EmailAddress

public export
data ProductCodeErr = MkProductCodeErr String

Show ProductCodeErr where
  showPrec d (MkProductCodeErr x) = showCon d "MkProductCodeErr" $ showArg x

public export
record InvalidOrder where
  constructor MkInvalidOrder
  order             : OrderForm
  validationErrors  : List ValidationError
  productCodeErrors : List ProductCodeErr

Show InvalidOrder where
  showPrec d (MkInvalidOrder order validationErrors productCodeErrors)
    = showCon d "MkInvalidOrder" $ concat [showArg validationErrors, showArg productCodeErrors]

public export
data PlacedOrderEvent
  = OrderPlacedEvent         PricedOrder
  | BillableOrderPlacedEvent BillableOrderPlaced
  | AcknowledgementSentEvent OrderAcknowledgementSent
  | InvalidOrderRegistered   InvalidOrder

export
Show PlacedOrderEvent where
  show (OrderPlacedEvent         x) = "PlacedOrderEvent"
  show (BillableOrderPlacedEvent x) = "BillableOrderPlacedEvent"
  show (AcknowledgementSentEvent x) = "AcknowledgementSentEvent"
  show (InvalidOrderRegistered   x) = "InvalidOrderRegistered: " ++ show x

createBillingEvent : PricedOrder -> Maybe BillableOrderPlaced
createBillingEvent pricedOrder = do
  if pricedOrder.amountToBill.value > 0
     then Just $ MkBillableOrderPlaced
                  { orderId        = pricedOrder.orderId
                  , billingAddress = pricedOrder.billingAddress
                  , amountToBill   = pricedOrder.amountToBill
                  }
     else Nothing

public export
record PricingError where
  constructor MkPricingError
  message : String

public export
data AckSent = Sent | NotSent

public export
record Uri where
  constructor MkUri
  address : String

public export
record ServiceInfo where
  constructor MkServiceInfo
  name     : String
  endpoint : Uri

public export
record RemoteServiceException where
  constructor MkRemoteServiceException
  message : String

public export
record RemoteServiceError where
  constructor MkRemoteServiceError
  serviceInfo : ServiceInfo
  exception   : RemoteServiceException

public export
data PlaceOrderError
  = MkPlaceOrderError String
  | ValidationErrors (List ValidationError)
  | ProductCodeError ProductCodeErr
  | PriceOrderError PricingError
  | RemoteServiceErr RemoteServiceError

export
Show PlaceOrderError where
  show (MkPlaceOrderError e)  = "MkPlaceOrderError: " ++ e
  show (ValidationErrors xs)  = "ValidationErrors"
  show (ProductCodeError x)   = "ProductCodeError"
  show (PriceOrderError x)    = "PriceOrderError"
  show (RemoteServiceErr x)   = "RemoteServiceErr"

maybeValidationErrors : PlaceOrderError -> Maybe (List ValidationError)
maybeValidationErrors (ValidationErrors es) = Just es
maybeValidationErrors _ = Nothing

maybeProductCodeError : PlaceOrderError -> Maybe ProductCodeErr
maybeProductCodeError (ProductCodeError e) = Just e
maybeProductCodeError _ = Nothing

||| Place Order Monad
|||
||| POM is shorthand for PlaceOrderMonad. This is a syntactical constructs of the
||| different expressions that can be build in the PlaceOrder domain. Because
||| POM has Pure and Bind it inposes a monadic structure, but POM is purely syntactic
||| construction. Categorically it is a commands inlined free monad.
export
data POM : Type -> Type where
  -- Monad interface
  Pure : a -> POM a
  Bind : POM a -> Inf (a -> POM b) -> POM b

  -- Throwing some error
  -- Throwing error needs to help the type-checker with the expected type of the
  -- POM result.
  ThrowError : (a : Type) -> PlaceOrderError -> POM a
  CatchError : {a : Type} -> POM a -> POM (Either PlaceOrderError a)

  -- Order handling
  NewOrderId : POM OrderId
  NewOrderLineId : POM OrderLineId

  -- Validate Order Commands
  CheckProductCodeExists : ProductCode -> POM Bool
  CheckAddressExists     : AddressForm -> POM (Either CheckedAddressValidationError CheckedAddress)

  -- Price Order Commands
  GetProductPrice : ProductCode -> POM Price
  PlacePricedOrder : PricedOrder -> POM ()

  -- Acknowledgement Order Commands
  CreateOrderAcknowledgementLetter : PricedOrder -> POM HtmlString
  SendOrderAcknowledgement : OrderAcknowledgement -> POM AckSent

export
Functor POM where
  map f m = Bind m (Pure . f)

export
Applicative POM where
  pure = Pure
  f <*> x = Bind f (\f' => Bind x (\x' => Pure (f' x')))

export
Monad POM where
  join  m = Bind m id
  m >>= k = Bind m k

||| A model for the POM structure.
|||
||| As POM is purely a syntactical construct, we need to give a semantical model for such
||| a construct. Semantical models can be given for POM in any monad where we can give
||| interpretetations of the POM constructs.
||| Categorically this is a monad morphism between the POM monad an interpretation monad if it.
public export
record Model (m : Type -> Type) where
  constructor MkModel
  throwError
    : {a : Type} -> PlaceOrderError -> m a
  catchError
    : {a : Type} -> m a -> m (Either PlaceOrderError a)
  newOrderId
    : m OrderId
  newOrderLineId
    : m OrderLineId
  checkProductCodeExists
    : ProductCode -> m Bool
  checkAddressExists
    : AddressForm -> m (Either CheckedAddressValidationError CheckedAddress)
  getProductPrice
    : ProductCode -> m Price
  placePricedOrder
    : PricedOrder -> m ()
  createOrderAcknowledgementLetter
    : PricedOrder -> m HtmlString
  sendOrderAcknowledgement
    : OrderAcknowledgement -> m AckSent

||| The function that gives interpretation of a POM expression in the monad 'm' using the
||| given model.
export
interpret : Monad m => Model m -> POM a -> m a
interpret model (Pure x)                             = pure x
interpret model (Bind m k)                           = interpret model m >>= (interpret model . k)
interpret model (ThrowError _ x)                     = model.throwError x
interpret model (CatchError x)                       = model.catchError (interpret model x)
interpret model NewOrderId                           = model.newOrderId
interpret model NewOrderLineId                       = model.newOrderLineId
interpret model (CheckProductCodeExists x)           = model.checkProductCodeExists x
interpret model (CheckAddressExists x)               = model.checkAddressExists x
interpret model (GetProductPrice x)                  = model.getProductPrice x
interpret model (PlacePricedOrder x)                 = model.placePricedOrder x
interpret model (CreateOrderAcknowledgementLetter x) = model.createOrderAcknowledgementLetter x
interpret model (SendOrderAcknowledgement x)         = model.sendOrderAcknowledgement x

checkCustomerInfoForm : CustomerInfoForm -> Form ValidationError CustomerInfo
checkCustomerInfoForm customer =
  MkCustomerInfo
    <$> (MkPersonalName
          <$> field customer.firstName
                    (StringN.create 50)
                    (NameValidation (MkNameValidationError "First name" customer.firstName))
          <*> field customer.lastName
                    (StringN.create 50)
                    (NameValidation (MkNameValidationError "Last name" customer.lastName)))
    <*> field customer.emailAddress
              EmailAddress.create
              (EmailValidation (MkEmailValidationError customer.emailAddress))

createCustomerInfo : CustomerInfoForm -> POM CustomerInfo
createCustomerInfo customer = do
  let Value customerInfo = checkCustomerInfoForm customer
      | Error es => ThrowError CustomerInfo $ ValidationErrors es
  pure customerInfo

checkAddressForm : AddressForm -> Form AddressValidationError Address
checkAddressForm addr =
  MkAddress
    <$> field addr.addressLine1 (StringN.create 50) (MkAddressLineError addr.addressLine1)
    <*> optionalField addr.addressLine2 (StringN.create 50) (MkAddressOptLineError addr.addressLine2)
    <*> optionalField addr.addressLine3 (StringN.create 50) (MkAddressOptLineError addr.addressLine3)
    <*> optionalField addr.addressLine4 (StringN.create 50) (MkAddressOptLineError addr.addressLine4)
    <*> field addr.city (StringN.create 50) (MkAddressCityError addr.city)
    <*> field addr.zipCode ZipCode.create   (MkAddressZipCodeError addr.zipCode)

toAddress : AddressForm -> POM Address
toAddress addressForm = do
  Right (MkCheckedAddress checkedAddressForm) <- CheckAddressExists addressForm
    | Left e => ThrowError Address
                  $ ValidationErrors
                      [AddressValidation $ CheckedAddressError e]
  let Value addr = checkAddressForm checkedAddressForm
      | Error es => ThrowError Address $ ValidationErrors $ map AddressValidation es
  pure addr

toProductCode : String -> POM ProductCode
toProductCode productCodeStr = do
  let Just productCode = mkProductCode productCodeStr
      | _ => ThrowError ProductCode
              $ ProductCodeError
              $ MkProductCodeErr
              $ "Invalid product code " ++ productCodeStr
  True <- CheckProductCodeExists productCode
    | _ => ThrowError ProductCode
            $ ProductCodeError
            $ MkProductCodeErr
            $ "Product doesn't exist " ++ productCodeStr
  pure productCode

toOrderQuantity : ProductCode -> String -> POM OrderQuantity
toOrderQuantity (WidgetProduct wp) quantity = do
  let Just integer = the (Maybe Integer) $ parseInteger quantity
      | _ => ThrowError OrderQuantity
              $ ValidationErrors
                  [ QuantityValidation (MkQuantityValidationError "Integer" quantity) ]
  let Just between = mkBetween integer
      | _ => ThrowError OrderQuantity
              $ ValidationErrors
                  [ QuantityValidation (MkQuantityValidationError "in between" quantity) ]
  pure $ OrderUnitQuantity $ MkUnitQuantity between
toOrderQuantity (GizmoProduct gp) quantity = do
  let Just double = parseDouble quantity
      | _ => ThrowError OrderQuantity
              $ ValidationErrors
                  [ QuantityValidation (MkQuantityValidationError "Double" quantity) ]
  let Just between = mkBetween double
      | _ => ThrowError OrderQuantity
                $ ValidationErrors
                    [ QuantityValidation (MkQuantityValidationError "in between" quantity) ]
  pure $ OrderKilogramQuantity $ MkKilogramQuantity between

toValidatedOrderLine : OrderLineForm -> POM OrderLine
toValidatedOrderLine orderLineForm = do
  orderLineId <- NewOrderLineId
  productCode <- toProductCode orderLineForm.productCode
  quantity    <- toOrderQuantity productCode orderLineForm.quantity
  pure $ MkOrderLine
    { orderLineId = orderLineId
    , productCode = productCode
    , quantity    = quantity
    }

export
validateOrder : OrderForm -> POM (Either InvalidOrder Order)
validateOrder orderForm = do
  orderId      <- NewOrderId
  customerInfo <- createCustomerInfo $ orderForm.customerInfo
  address      <- toAddress $ orderForm.shippingAddress
  orderLines   <- traverse (CatchError . toValidatedOrderLine) orderForm.orderLines
  pure $ case partitionEithers orderLines of
    ([], orderLines)
      => Right $ MkOrder
          { orderId         = orderId
          , customerInfo    = customerInfo
          , shippingAddress = MkShippingAddress address
          , billingAddress  = MkBillingAddress address
          , orderLines      = orderLines
          }
    (errors, orderLines)
      => Left $ MkInvalidOrder
          { order  = orderForm
          , validationErrors = concat $ mapMaybe maybeValidationErrors errors
          , productCodeErrors = mapMaybe maybeProductCodeError errors
          }

-- The Pricing Step

toPricedOrderLine : OrderLine -> POM PricedOrderLine
toPricedOrderLine orderLine = do
  let quantity = orderLine.quantity.value
  priceVal <- GetProductPrice $ orderLine.productCode
  let price = multiply priceVal quantity
  pure $ MkPricedOrderLine
    { orderLine = orderLine
    , price     = price
    }

export
priceOrder : Order -> POM PricedOrder
priceOrder order = do
  pricedOrderLines <- traverse toPricedOrderLine order.orderLines
  let amountToBill = sumPrices $ map price pricedOrderLines
  pure $ MkPricedOrder
    { orderId         = order.orderId
    , customerInfo    = order.customerInfo
    , shippingAddress = order.shippingAddress
    , billingAddress  = order.billingAddress
    , orderLines      = pricedOrderLines
    , amountToBill    = amountToBill
    }

-- The Acknowledge Order Step

export
acknowledgeOrder : PricedOrder -> POM (Maybe OrderAcknowledgementSent)
acknowledgeOrder pricedOrder = do
  letter <- CreateOrderAcknowledgementLetter pricedOrder
  let acknowledgement
        = MkOrderAcknowledgement
        { emailAddress = pricedOrder.customerInfo.emailAddress
        , letter       = letter
        }
  case !(SendOrderAcknowledgement acknowledgement) of
    Sent =>
      pure
        $ Just
        $ MkOrderAcknowledgementSent
            { orderId      = pricedOrder.orderId
            , emailAddress = pricedOrder.customerInfo.emailAddress
            }
    NotSent =>
      pure Nothing

-- Place order step

export
placePricedOrder : PricedOrder -> POM ()
placePricedOrder = PlacePricedOrder

export
createEvents : PricedOrder -> Maybe OrderAcknowledgementSent -> List PlacedOrderEvent
createEvents pricedOrder orderAcknowledgementSent =
  catMaybes
    [ Just $ OrderPlacedEvent pricedOrder
    , map AcknowledgementSentEvent orderAcknowledgementSent
    , map BillableOrderPlacedEvent $ createBillingEvent pricedOrder
    ]
