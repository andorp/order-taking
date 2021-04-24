module OrderTaking.Domain.PlaceOrder

import OrderTaking.Domain.Prelude
import Rango.BoundedContext.Command
import Data.List
import Data.Strings


data Price = MkPrice Double

namespace Price

  export
  multiply : Price -> Double -> Price
  multiply (MkPrice x) d = MkPrice (x * d)

  export
  value : Price -> Double
  value (MkPrice x) = x

  export
  sumPrices : List Price -> Price
  sumPrices = MkPrice . sum . map value

data EmailAddress = MkEmailAddress String

mkEmailAddress : String -> Maybe EmailAddress
mkEmailAddress = ?mkEmailAddress1

record PersonalName where
  constructor MkPersonalName
  firstName : StringN 50
  lastName  : StringN 50

record CustomerInfo where
  constructor MkCustomerInfo
  personalName : PersonalName
  emailAddress : EmailAddress

data ZipCode = MkZipCode String

mkZipCode : String -> Maybe ZipCode
mkZipCode = Just . MkZipCode -- TODO

record AddressForm where
  constructor MkAddressForm
  addressLine1 : String
  addressLine2 : Maybe String
  addressLine3 : Maybe String
  addressLine4 : Maybe String
  city         : String
  zipCode      : String

record Address where
  constructor MkAddress
  addressLine1 : StringN 50
  addressLine2 : Maybe (StringN 50)
  addressLine3 : Maybe (StringN 50)
  addressLine4 : Maybe (StringN 50)
  city         : StringN 50
  zipCode      : ZipCode

data ShippingAddress = MkShippingAddress Address

data BillingAddress = MkBillingAddress Address

data WidgetCode = MkWidgetCode String

mkWidgetCode : String -> Maybe WidgetCode
mkWidgetCode = ?mkWidgetCode1

data GizmoCode = MkGizmoCode String

mkGizmoCode : String -> Maybe GizmoCode
mkGizmoCode = ?mkGizmoCode1

data ProductCode
  = WidgetProduct WidgetCode
  | GizmoProduct GizmoCode

mkProductCode : String -> Maybe ProductCode
mkProductCode = ?mkProductCode1

record OrderLineForm where
  constructor MkOrderLineForm
  productCode : String
  quantity    : String

record CustomerInfoForm where
  constructor MkCustomerInfoForm
  firstName    : String
  lastName     : String
  emailAddress : String

export
record OrderForm where
  constructor MkOrderForm
  customerInfo    : CustomerInfoForm
  shippingAddress : AddressForm
  billingAddress  : String
  orderLines      : List OrderLineForm
  amount          : Double

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
  value : OrderQuantity -> Double
  value (OrderUnitQuantity     (MkUnitQuantity x))     = fromInteger $ value x
  value (OrderKilogramQuantity (MkKilogramQuantity x)) = Between.value x

data OrderId = MkOrderId

data OrderLineId = MkOrderLineId

record OrderLine where
  constructor MkOrderLine
  orderLineId : OrderLineId
  productCode : ProductCode
  quantity    : OrderQuantity

-- Synonym to ValidatedOrder
export
record Order where
  constructor MkOrder
  orderId         : OrderId
  customerInfo    : CustomerInfo
  shippingAddress : ShippingAddress
  billingAddress  : BillingAddress
  orderLines      : List OrderLine

record PricedOrderLine where
  constructor MkPricedOrderLine
  orderLine : OrderLine
  linePrice : Price

export
record PricedOrder where
  constructor MkPricedOrder
  orderId         : OrderId
  customerInfo    : CustomerInfo
  shippingAddress : ShippingAddress
  billingAddress  : BillingAddress
  orderLine       : List PricedOrderLine
  amountToBill    : Price

data AcknowledgementLetter = MkAcknowledgementLetter

record PlacedOrderAcknowledgement where
  constructor MkPlacedOrderAcknowledgement
  pricedOrder           : PricedOrder
  acknowledgementLetter : AcknowledgementLetter

data OrderPlaced = MkOrderPlaced

record BillableOrderPlaced where
  constructor MkBillableOrderPlaced
  orderId        : OrderId
  billingAddress : BillingAddress
  amountToBill   : Price

record PlaceOrderEvents where
  constructor MkPlaceOrderEvents
  acknowledgementSent : AcknowledgementLetter
  orderPlaced         : OrderPlaced
  billableOrderPlaced : BillableOrderPlaced

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

data CheckedAddressValidationError
  = InvalidFormat String
  | AddressNotFound String

data AddressValidationError
  = MkAddressLineError String
  | MkAddressOptLineError (Maybe String)
  | MkAddressCityError String
  | MkAddressZipCodeError String
  | CheckedAddressError CheckedAddressValidationError

data NameValidationError = MkNameValidationError String String

data EmailValidationError = MkEmailValidationError String

data QuantityValidationError = MkQuantityValidationError String String

data ValidationError
  = AddressValidation AddressValidationError
  | NameValidation NameValidationError
  | EmailValidation EmailValidationError
  | QuantityValidation QuantityValidationError

data CheckedAddress = MkCheckedAddress AddressForm

data HtmlString = MkHtmlString String

record OrderAcknowledgement where
  constructor MkOrderAcknowledgement
  emailAddress : EmailAddress
  letter       : HtmlString

record OrderAcknowledgementSent where
  constructor MkOrderAcknowledgementSent
  orderId      : OrderId
  emailAddress : EmailAddress

export
record InvalidOrder where
  constructor MkInvalidOrder
  order  : OrderForm
  errors : List ValidationError

public export
data PlacedOrderEvent
  = OrderPlacedEvent         PricedOrder
  | BillableOrderPlacedEvent BillableOrderPlaced
  | AcknowledgementSentEvent OrderAcknowledgementSent
  | InvalidOrderRegistered   InvalidOrder

createBillingEvent : PricedOrder -> Maybe BillableOrderPlaced
createBillingEvent pricedOrder = do
  if value pricedOrder.amountToBill > 0
     then Just $ MkBillableOrderPlaced
                  { orderId        = pricedOrder.orderId
                  , billingAddress = pricedOrder.billingAddress
                  , amountToBill   = pricedOrder.amountToBill
                  }
     else Nothing

export
createEvents : PricedOrder -> Maybe OrderAcknowledgementSent -> List PlacedOrderEvent
createEvents pricedOrder orderAcknowledgementSent = catMaybes
  [ Just $ OrderPlacedEvent pricedOrder
  , map AcknowledgementSentEvent orderAcknowledgementSent
  , map BillableOrderPlacedEvent $ createBillingEvent pricedOrder
  ]

data PricingError = MkPricingError

data ProductCodeErr = MkProductCodeErr String

data AckSent = Sent | NotSent

public export
data PlaceOrderError
  = MkPlaceOrderError
  | ValidationErrors (List ValidationError)
  | ProductCodeError ProductCodeErr
  | PriceOrderError PricingError

export
data POM : Type -> Type where
  -- Monad interface
  Pure : a -> POM a
  Bind : POM a -> Inf (a -> POM b) -> POM b

  -- Throwing some error
  Throw : (a : Type) -> PlaceOrderError -> POM a
  Catch : {a : Type} -> POM a -> POM (Either PlaceOrderError a)

  -- Order handling
  NewOrderId : POM OrderId
  NewOrderLineId : POM OrderLineId

  -- Validate Order Commands
  CheckProductCodeExists : ProductCode -> POM Bool
  CheckAddressExists     : AddressForm -> POM (Either CheckedAddressValidationError CheckedAddress)

  -- Price Order Commands
  GetProductPrice : ProductCode -> POM Price

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
  (>>=)   = Bind

namespace Command
  public export
  PlaceOrder : Type
  PlaceOrder = Command OrderForm

  public export
  ChangeOrder : Type
  ChangeOrder = Command Order

  public export
  CancelOrder : Type
  CancelOrder = Command Order

  data OrderTakingCommand
    = Place  PlaceOrder
    | Change ChangeOrder
    | Cancel CancelOrder

namespace Field

  public export
  data Field : Type -> Type -> Type where
    Error : List e -> Field e a
    Value : a      -> Field e a

  export
  Functor (Field e) where
    map f (Error es) = Error es
    map f (Value x)  = Value (f x)

  export
  Applicative (Field e) where
    pure x = Value x
    Error es1 <*> Error es2 = Error (es1 ++ es2)
    Error es1 <*> Value x   = Error es1
    Value f   <*> Error es1 = Error es1
    Value f   <*> Value x   = Value (f x)

  export
  field : a -> (a -> Maybe b) -> e -> Field e b
  field v f e = maybe (Error [e]) Value $ f v

  export
  optionalField : Maybe a -> (a -> Maybe b) -> e -> Field e (Maybe b)
  optionalField Nothing f e  = Value Nothing
  optionalField (Just v) f e = Just <$> field v f e

checkCustomerInfoForm : CustomerInfoForm -> Field ValidationError CustomerInfo
checkCustomerInfoForm customer =
  MkCustomerInfo
    <$> (MkPersonalName
          <$> field customer.firstName
                    (mkStringN 50)
                    (NameValidation (MkNameValidationError "First name" customer.firstName))
          <*> field customer.lastName
                    (mkStringN 50)
                    (NameValidation (MkNameValidationError "Last name" customer.lastName)))
    <*> field customer.emailAddress
              mkEmailAddress
              (EmailValidation (MkEmailValidationError customer.emailAddress))

createCustomerInfo : CustomerInfoForm -> POM CustomerInfo
createCustomerInfo customer = do
  let Value customerInfo = checkCustomerInfoForm customer
      | Error es => Throw CustomerInfo $ ValidationErrors es
  pure customerInfo

checkAddressForm : AddressForm -> Field AddressValidationError Address
checkAddressForm addr =
  MkAddress
    <$> field addr.addressLine1 (mkStringN 50) (MkAddressLineError addr.addressLine1)
    <*> optionalField addr.addressLine2 (mkStringN 50) (MkAddressOptLineError addr.addressLine2)
    <*> optionalField addr.addressLine3 (mkStringN 50) (MkAddressOptLineError addr.addressLine3)
    <*> optionalField addr.addressLine4 (mkStringN 50) (MkAddressOptLineError addr.addressLine4)
    <*> field addr.city (mkStringN 50) (MkAddressCityError addr.city)
    <*> field addr.zipCode mkZipCode   (MkAddressZipCodeError addr.zipCode)

toAddress : AddressForm -> POM Address
toAddress addressForm = do
  Right (MkCheckedAddress checkedAddressForm) <- CheckAddressExists addressForm
    | Left e => Throw Address $ ValidationErrors [AddressValidation $ CheckedAddressError e]
  let Value addr = checkAddressForm checkedAddressForm
      | Error es => Throw Address $ ValidationErrors $ map AddressValidation es
  pure addr

toProductCode : String -> POM ProductCode
toProductCode productCodeStr = do
  let Just productCode = mkProductCode productCodeStr
      | _ => Throw ProductCode $ ProductCodeError
                   $ MkProductCodeErr
                   $ "Invalid product code " ++ productCodeStr
  True <- CheckProductCodeExists productCode
    | _ => Throw ProductCode $ ProductCodeError
                 $ MkProductCodeErr
                 $ "Product doesn't exist " ++ productCodeStr
  pure productCode

toOrderQuantity : ProductCode -> String -> POM OrderQuantity
toOrderQuantity (WidgetProduct wp) quantity = do
  let Just integer = the (Maybe Integer) $ parseInteger quantity
      | _ => Throw OrderQuantity $ ValidationErrors [QuantityValidation
                                        (MkQuantityValidationError "Integer" quantity)]
  let Just between = mkBetween integer
      | _ => Throw OrderQuantity $ ValidationErrors [QuantityValidation
                                        (MkQuantityValidationError "in between" quantity)]
  pure $ OrderUnitQuantity $ MkUnitQuantity between
toOrderQuantity (GizmoProduct gp) quantity = do
  let Just double = parseDouble quantity
      | _ => Throw OrderQuantity $ ValidationErrors [QuantityValidation
                                        (MkQuantityValidationError "Double" quantity)]
  let Just between = mkBetween double
      | _ => Throw OrderQuantity $ ValidationErrors [QuantityValidation
                                        (MkQuantityValidationError "in between" quantity)]
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

catchValidationErrors : {a : Type} -> POM a -> POM (Either (List ValidationError) a)
catchValidationErrors m = do
  r <- Catch m
  case r of
    Left (ValidationErrors es) => pure $ Left es
    Left err => Throw (Either (List ValidationError) a) err
    Right x  => pure $ Right x

export
validateOrder : OrderForm -> POM (Either InvalidOrder Order)
validateOrder orderForm = checkInvalidForm $ do
  orderId      <- NewOrderId
  customerInfo <- createCustomerInfo $ orderForm.customerInfo
  address      <- toAddress $ orderForm.shippingAddress
  orderLines   <- traverse toValidatedOrderLine orderForm.orderLines
  pure $ MkOrder
    { orderId         = orderId
    , customerInfo    = customerInfo
    , shippingAddress = MkShippingAddress address
    , billingAddress  = MkBillingAddress address
    , orderLines      = orderLines
    }
  where
    checkInvalidForm : POM Order -> POM (Either InvalidOrder Order)
    checkInvalidForm m = do
      r <- catchValidationErrors m
      pure $ case r of
        Left es => Left $ MkInvalidOrder
                    { order  = orderForm
                    , errors = es
                    }
        Right n => Right n

-- The Pricing Step

toPricedOrderLine : OrderLine -> POM PricedOrderLine
toPricedOrderLine orderLine = do
  let quantity = OrderQuantity.value $ orderLine.quantity
  price <- GetProductPrice $ orderLine.productCode
  let linePrice = multiply price quantity
  pure $ MkPricedOrderLine
    { orderLine = orderLine
    , linePrice = linePrice
    }

export
priceOrder : Order -> POM PricedOrder
priceOrder order = do
  pricedOrderLines <- traverse toPricedOrderLine order.orderLines
  let amountToBill = sumPrices $ map linePrice pricedOrderLines
  pure $ MkPricedOrder
    { orderId         = order.orderId
    , customerInfo    = order.customerInfo
    , shippingAddress = order.shippingAddress
    , billingAddress  = order.billingAddress
    , orderLine       = pricedOrderLines
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

-- Page 142
-- Page 167
-- Page 172
-- Page 178
-- Page 195
