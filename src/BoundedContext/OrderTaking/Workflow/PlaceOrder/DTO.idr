module BoundedContext.OrderTaking.Workflow.PlaceOrder.DTO

import Rango.DataTransfer.JSON.Derive
import Rango.DataTransfer.SQL.Derive

%language ElabReflection
%default total

-- Incoming information
-- From Upstream system

namespace UpstreamDTO

  public export
  record AddressFormDTO where
    constructor MkAddressFormDTO
    addressLine1 : String
    addressLine2 : String
    addressLine3 : String
    addressLine4 : String
    city         : String
    zipCode      : String

  public export
  record OrderLineFormDTO where
    constructor MkOrderLineFormDTO
    productCode : String
    quantity    : String

  public export
  record CustomerFormDTO where
    constructor MkCustomerFormDTO
    firstName    : String
    lastName     : String
    emailAddress : String

  public export
  record OrderFormDTO where
    constructor MkOrderFormDTO
    customer        : CustomerFormDTO
    shippingAddress : AddressFormDTO
    billingAddress  : AddressFormDTO
    orderLines      : List OrderLineFormDTO

  public export
  record ProductFormDTO where
    constructor MkProductFormDTO
    productCode : String
    price       : Double
    description : String

  %runElab deriveJSON defaultOpts `{{AddressFormDTO}}
  %runElab deriveJSON defaultOpts `{{OrderLineFormDTO}}
  %runElab deriveJSON defaultOpts `{{CustomerFormDTO}}
  %runElab deriveJSON defaultOpts `{{OrderFormDTO}}
  %runElab deriveJSON defaultOpts `{{ProductFormDTO}}

-- Outgoing information
-- To Downstream systems

namespace DownstreamDTO

  public export
  record PricedOrderLineDsDTO where
    constructor MkPricedOrderLineDsDTO
    orderLineId : String
    productCode : String
    price       : Double

  public export
  record PricedOrderDsDTO where
    constructor MkPricedOrderDsDTO
    orderId       : String
    orderLines    : List PricedOrderLineDsDTO
    amountToBill  : Double

  --NOTE: Redefining the type doesn't raise any issues. Is this intended?
  --data BillableOrderPlacedDTO : Type

  -- NOTE: Reusing name, creates elaboration issue.
  public export
  record AddressDsDTO where
    constructor MkAddressDsDTO
    addressLine1 : String
    addressLine2 : String
    addressLine3 : String
    addressLine4 : String
    city         : String
    zipCode      : String

  public export
  record BillableOrderPlacedDTO where
    constructor MkBillableOrderPlacedDTO
    orderId        : String
    billingAddress : DownstreamDTO.AddressDsDTO
    amountToBill   : Double

  public export
  record OrderAcknowledgementSentDTO where
    constructor MkOrderAcknowledgementSentDTO
    orderId      : String
    emailAddress : String

  public export
  data CheckedAddressValidationErrorDTO
    = InvalidFormat String
    | AddressNotFound String

  public export
  data AddressValidationErrorDTO
    = MkAddressLineError    String
    | MkAddressOptLineError (Maybe String)
    | MkAddressCityError    String
    | MkAddressZipCodeError String
    | CheckedAddressError   CheckedAddressValidationErrorDTO

  public export
  record NameValidationErrorDTO where
    constructor MkNameValidationErrorDTO
    field : String
    value : String
  
  public export
  record EmailValidationErrorDTO where
    constructor MkEmailValidationErrorDTO
    message : String

  public export
  record QuantityValidationErrorDTO where
    constructor MkQuantityValidationErrorDTO
    condition : String
    message   : String

  public export
  data ValidationErrorDTO
    = AddressValidation   AddressValidationErrorDTO
    | NameValidation      NameValidationErrorDTO
    | EmailValidation     EmailValidationErrorDTO
    | QuantityValidation  QuantityValidationErrorDTO

  public export
  record ProductCodeErrDTO where
    constructor MkProductCodeErr
    message : String

  public export
  record InvalidOrderDTO where
    constructor MkInvalidOrderDTO
    validationErrors  : List ValidationErrorDTO
    productCodeErrors : List ProductCodeErrDTO

  public export
  data PlaceOrderEventDTO
    = OrderPlacedEvent          PricedOrderDsDTO
    | BillableOrderPlacedEvent  BillableOrderPlacedDTO
    | AcknowledgementSentEvent  OrderAcknowledgementSentDTO
    | InvalidOrderRegistered    InvalidOrderDTO

  public export
  record PricingErrorDTO where
    constructor MkPricingError
    message : String

  public export
  record RemoteServiceErrorDTO where
    constructor MkRemoteServiceErrorDTO
    serviceInfo : String
    exception   : String

  public export
  data PlaceOrderErrorDTO
    = MkPlaceOrderError String
    | ValidationErrors (List ValidationErrorDTO)
    | ProductCodeError ProductCodeErrDTO
    | PriceOrderError PricingErrorDTO
    | RemoteServiceErr RemoteServiceErrorDTO

  export
  constructorToJSON : {x : Type} -> (ToJSON x) => String -> x -> JSON
  constructorToJSON tag field = JObject [("tag", JString tag), ("arg1", toJSON field)]

  export
  constructorFromJSON : (FromJSON x) => String -> (x -> y) -> JSON -> Maybe y
  constructorFromJSON tag create (JObject [("tag", JString tag0), ("arg1", field)])
    = if tag == tag0
        then map create $ fromJSON field
        else Nothing
  constructorFromJSON _ _ _ = Nothing

  ToJSON CheckedAddressValidationErrorDTO where
    toJSON (InvalidFormat x)   = constructorToJSON "InvalidFormat"   x
    toJSON (AddressNotFound x) = constructorToJSON "AddressNotFound" x
  
  FromJSON CheckedAddressValidationErrorDTO where
    fromJSON x
        = constructorFromJSON "InvalidFormat"   InvalidFormat x
      <|> constructorFromJSON "AddressNotFound" AddressNotFound x

  ToJSON AddressValidationErrorDTO where
    toJSON (MkAddressLineError    x) = constructorToJSON "MkAddressLineError"    x
    toJSON (MkAddressOptLineError x) = constructorToJSON "MkAddressOptLineError" x
    toJSON (MkAddressCityError    x) = constructorToJSON "MkAddressCityError"    x
    toJSON (MkAddressZipCodeError x) = constructorToJSON "MkAddressZipCodeError" x
    toJSON (CheckedAddressError   x) = constructorToJSON "CheckedAddressError"   x
  
  FromJSON AddressValidationErrorDTO where
    fromJSON x
        = constructorFromJSON "MkAddressLineError" MkAddressLineError x
      <|> constructorFromJSON "MkAddressOptLineError" MkAddressOptLineError x
      <|> constructorFromJSON "MkAddressCityError" MkAddressCityError x
      <|> constructorFromJSON "MkAddressZipCodeError" MkAddressZipCodeError x
      <|> constructorFromJSON "CheckedAddressError" CheckedAddressError x

  %runElab deriveJSON defaultOpts `{{NameValidationErrorDTO}}
  %runElab deriveJSON defaultOpts `{{EmailValidationErrorDTO}}
  %runElab deriveJSON defaultOpts `{{QuantityValidationErrorDTO}}

  ToJSON ValidationErrorDTO where
    toJSON (AddressValidation   x) = constructorToJSON "AddressValidation" x
    toJSON (NameValidation      x) = constructorToJSON "NameValidation" x
    toJSON (EmailValidation     x) = constructorToJSON "EmailValidation" x
    toJSON (QuantityValidation  x) = constructorToJSON "QuantityValidation" x

  FromJSON ValidationErrorDTO where
    fromJSON x
        = constructorFromJSON "AddressValidation" AddressValidation x
      <|> constructorFromJSON "NameValidation" NameValidation x
      <|> constructorFromJSON "EmailValidation" EmailValidation x
      <|> constructorFromJSON "QuantityValidation" QuantityValidation x

  %runElab deriveJSON defaultOpts `{{PricedOrderLineDsDTO}}
  %runElab deriveJSON defaultOpts `{{AddressDsDTO}}
  %runElab deriveJSON defaultOpts `{{BillableOrderPlacedDTO}}
  %runElab deriveJSON defaultOpts `{{OrderAcknowledgementSentDTO}}
  %runElab deriveJSON defaultOpts `{{ProductCodeErrDTO}}
  %runElab deriveJSON defaultOpts `{{InvalidOrderDTO}}
  %runElab deriveJSON defaultOpts `{{PricedOrderDsDTO}}
  %runElab deriveJSON defaultOpts `{{PricingErrorDTO}}
  %runElab deriveJSON defaultOpts `{{RemoteServiceErrorDTO}}

  export
  ToJSON PlaceOrderEventDTO where
    toJSON (OrderPlacedEvent          x) = constructorToJSON "OrderPlacedEvent" x
    toJSON (BillableOrderPlacedEvent  x) = constructorToJSON "BillableOrderPlacedEvent" x
    toJSON (AcknowledgementSentEvent  x) = constructorToJSON "AcknowledgementSentEvent" x
    toJSON (InvalidOrderRegistered    x) = constructorToJSON "InvalidOrderRegistered" x

  export
  FromJSON PlaceOrderEventDTO where
    fromJSON x
        = constructorFromJSON "OrderPlacedEvent" OrderPlacedEvent x
      <|> constructorFromJSON "BillableOrderPlacedEvent" BillableOrderPlacedEvent x
      <|> constructorFromJSON "AcknowledgementSentEvent" AcknowledgementSentEvent x
      <|> constructorFromJSON "InvalidOrderRegistered" InvalidOrderRegistered x

  export
  ToJSON PlaceOrderErrorDTO where
    toJSON (MkPlaceOrderError x)  = constructorToJSON "MkPlaceOrderError" x
    toJSON (ValidationErrors xs)  = constructorToJSON "ValidationErrors" xs
    toJSON (ProductCodeError x)   = constructorToJSON "ProductCodeError" x
    toJSON (PriceOrderError x)    = constructorToJSON "PriceOrderError" x
    toJSON (RemoteServiceErr x)   = constructorToJSON "RemoteServiceErr" x

  export
  FromJSON PlaceOrderErrorDTO where
    fromJSON x
        = constructorFromJSON "MkPlaceOrderError" MkPlaceOrderError x
      <|> constructorFromJSON "ValidationErrors" ValidationErrors x
      <|> constructorFromJSON "ProductCodeError" ProductCodeError x
      <|> constructorFromJSON "PriceOrderError" PriceOrderError x      
      <|> constructorFromJSON "RemoteServiceErr" RemoteServiceErr x
