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

-- public export
-- data PlacedOrderEvent
--   = OrderPlacedEvent         PricedOrder
--   | BillableOrderPlacedEvent BillableOrderPlaced
--   | AcknowledgementSentEvent OrderAcknowledgementSent
--   | InvalidOrderRegistered   InvalidOrder

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

  ToJSON CheckedAddressValidationErrorDTO where
  FromJSON CheckedAddressValidationErrorDTO where
  ToJSON AddressValidationErrorDTO where
  FromJSON AddressValidationErrorDTO where
  ToJSON ValidationErrorDTO where
  FromJSON ValidationErrorDTO where
  ToJSON PlaceOrderEventDTO where
  FromJSON PlaceOrderEventDTO where

  %runElab deriveJSON defaultOpts `{{PricedOrderLineDsDTO}}
  %runElab deriveJSON defaultOpts `{{PricedOrderDsDTO}}
  %runElab deriveJSON defaultOpts `{{AddressDsDTO}}
  %runElab deriveJSON defaultOpts `{{BillableOrderPlacedDTO}}
  %runElab deriveJSON defaultOpts `{{OrderAcknowledgementSentDTO}}
  -- %runElab deriveJSON defaultOpts `{{CheckedAddressValidationErrorDTO}}
  -- %runElab deriveJSON defaultOpts `{{AddressValidationErrorDTO}}
  %runElab deriveJSON defaultOpts `{{NameValidationErrorDTO}}
  %runElab deriveJSON defaultOpts `{{EmailValidationErrorDTO}}
  %runElab deriveJSON defaultOpts `{{QuantityValidationErrorDTO}}
  -- %runElab deriveJSON defaultOpts `{{ValidationErrorDTO}}
  %runElab deriveJSON defaultOpts `{{ProductCodeErrDTO}}
  %runElab deriveJSON defaultOpts `{{InvalidOrderDTO}}
  -- %runElab deriveJSON defaultOpts `{{PlaceOrderEventDTO}}
