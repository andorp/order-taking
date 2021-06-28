module OrderTaking.DTO.PlaceOrder

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
-- To Database

namespace DatabaseDTO

  public export
  Identifier : Type
  Identifier = String

  public export
  record AddressDTO where
    constructor MkAddressDTO
    identifier   : Identifier
    addressLine1 : String
    addressLine2 : (Maybe String)
    addressLine3 : (Maybe String)
    addressLine4 : (Maybe String)
    city         : String
    zipCode      : String

  public export
  record OrderLineDTO where
    constructor MkOrderLineDTO
    identifier  : Identifier
    productCode : String
    quantity    : String

  public export
  record PricedOrderLineDTO where
    constructor MkPricedOrderLineDTO
    identifier  : Identifier
    productCode : String
    quantity    : Double
    price       : Double

  public export
  record CustomerDTO where
    constructor MkCustomerDTO
    identifier   : Identifier
    firstName    : String
    lastName     : String
    emailAddress : String

  public export
  record OrderDTO where
    constructor MkOrderDTO
    identifier      : Identifier
    customer        : CustomerDTO
    shippingAddress : AddressDTO
    billingAddress  : AddressDTO
    orderLines      : List OrderLineDTO

  public export
  record PricedOrderDTO where
    constructor MkPricedOrderDTO
    identifier      : Identifier
    customer        : CustomerDTO
    shippingAddress : AddressDTO
    billingAddress  : AddressDTO
    orderLines      : List PricedOrderLineDTO
    amount          : Double

  public export
  record ProductCodeDTO where
    constructor MkProductCodeDTO
    productCode : String

  public export
  record ProductDTO where
    constructor MkProductDTO
    productCode : ProductCodeDTO
    price       : Double
    description : String

  %runElab deriveJSON defaultOpts `{{AddressDTO}}
  %runElab deriveJSON defaultOpts `{{OrderLineDTO}}
  %runElab deriveJSON defaultOpts `{{PricedOrderLineDTO}}
  %runElab deriveJSON defaultOpts `{{CustomerDTO}}
  %runElab deriveJSON defaultOpts `{{OrderDTO}}
  %runElab deriveJSON defaultOpts `{{PricedOrderDTO}}
  %runElab deriveJSON defaultOpts `{{ProductCodeDTO}}
  %runElab deriveJSON defaultOpts `{{ProductDTO}}

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

  toJSONArg1 : {x : Type} -> (ToJSON x) => String -> x -> JSON
  toJSONArg1 tag field = JObject [("tag", JString tag), ("arg1", toJSON field)]

  fromJSONArg1 : (FromJSON x) => String -> (x -> y) -> JSON -> Maybe y
  fromJSONArg1 tag create (JObject [("tag", JString tag0), ("arg1", field)])
    = if tag == tag0
        then map create $ fromJSON field
        else Nothing
  fromJSONArg1 _ _ _ = Nothing

  ToJSON CheckedAddressValidationErrorDTO where
    toJSON (InvalidFormat x)   = toJSONArg1 "InvalidFormat"   x
    toJSON (AddressNotFound x) = toJSONArg1 "AddressNotFound" x
  
  FromJSON CheckedAddressValidationErrorDTO where
    fromJSON x = fromJSONArg1 "InvalidFormat"   InvalidFormat x
    fromJSON x = fromJSONArg1 "AddressNotFound" AddressNotFound x
    fromJSON _ = Nothing

  ToJSON AddressValidationErrorDTO where
    toJSON (MkAddressLineError    x) = toJSONArg1 "MkAddressLineError"    x
    toJSON (MkAddressOptLineError x) = toJSONArg1 "MkAddressOptLineError" x
    toJSON (MkAddressCityError    x) = toJSONArg1 "MkAddressCityError"    x
    toJSON (MkAddressZipCodeError x) = toJSONArg1 "MkAddressZipCodeError" x
    toJSON (CheckedAddressError   x) = toJSONArg1 "CheckedAddressError"   x
  
  FromJSON AddressValidationErrorDTO where
    fromJSON x = fromJSONArg1 "MkAddressLineError" MkAddressLineError x
    fromJSON x = fromJSONArg1 "MkAddressOptLineError" MkAddressOptLineError x
    fromJSON x = fromJSONArg1 "MkAddressCityError" MkAddressCityError x
    fromJSON x = fromJSONArg1 "MkAddressZipCodeError" MkAddressZipCodeError x
    fromJSON x = fromJSONArg1 "CheckedAddressError" CheckedAddressError x
    fromJSON _ = Nothing

  %runElab deriveJSON defaultOpts `{{NameValidationErrorDTO}}
  %runElab deriveJSON defaultOpts `{{EmailValidationErrorDTO}}
  %runElab deriveJSON defaultOpts `{{QuantityValidationErrorDTO}}

  ToJSON ValidationErrorDTO where
    toJSON (AddressValidation   x) = toJSONArg1 "AddressValidation" x
    toJSON (NameValidation      x) = toJSONArg1 "NameValidation" x
    toJSON (EmailValidation     x) = toJSONArg1 "EmailValidation" x
    toJSON (QuantityValidation  x) = toJSONArg1 "QuantityValidation" x

  FromJSON ValidationErrorDTO where
    fromJSON x = fromJSONArg1 "AddressValidation" AddressValidation x
    fromJSON x = fromJSONArg1 "NameValidation" NameValidation x
    fromJSON x = fromJSONArg1 "EmailValidation" EmailValidation x
    fromJSON x = fromJSONArg1 "QuantityValidation" QuantityValidation x
    fromJSON _ = Nothing

  %runElab deriveJSON defaultOpts `{{PricedOrderLineDsDTO}}
  %runElab deriveJSON defaultOpts `{{AddressDsDTO}}
  %runElab deriveJSON defaultOpts `{{BillableOrderPlacedDTO}}
  %runElab deriveJSON defaultOpts `{{OrderAcknowledgementSentDTO}}
  %runElab deriveJSON defaultOpts `{{ProductCodeErrDTO}}
  %runElab deriveJSON defaultOpts `{{InvalidOrderDTO}}
  %runElab deriveJSON defaultOpts `{{PricedOrderDsDTO}}

  export
  ToJSON PlaceOrderEventDTO where
    toJSON (OrderPlacedEvent          x) = toJSONArg1 "OrderPlacedEvent" x
    toJSON (BillableOrderPlacedEvent  x) = toJSONArg1 "BillableOrderPlacedEvent" x
    toJSON (AcknowledgementSentEvent  x) = toJSONArg1 "AcknowledgementSentEvent" x
    toJSON (InvalidOrderRegistered    x) = toJSONArg1 "InvalidOrderRegistered" x

  export
  FromJSON PlaceOrderEventDTO where
    fromJSON x = fromJSONArg1 "OrderPlacedEvent" OrderPlacedEvent x
    fromJSON x = fromJSONArg1 "BillableOrderPlacedEvent" BillableOrderPlacedEvent x
    fromJSON x = fromJSONArg1 "AcknowledgementSentEvent" AcknowledgementSentEvent x
    fromJSON x = fromJSONArg1 "InvalidOrderRegistered" InvalidOrderRegistered x
    fromJSON _ = Nothing
