module BoundedContext.OrderTaking.Workflow.PlaceOrder.ConvertDTO

import BoundedContext.OrderTaking.Workflow.PlaceOrder.DTO
import BoundedContext.OrderTaking.Workflow.PlaceOrder.Domain

import Data.StringN

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
    { addressLine1 = a.addressLine1.value
    , addressLine2 = maybe "" (.value) a.addressLine2
    , addressLine3 = maybe "" (.value) a.addressLine3
    , addressLine4 = maybe "" (.value) a.addressLine4
    , city         = a.city.value
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
