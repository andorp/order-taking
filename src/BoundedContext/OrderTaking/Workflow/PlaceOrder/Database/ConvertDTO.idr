module BoundedContext.OrderTaking.Workflow.PlaceOrder.Database.ConvertDTO

import BoundedContext.OrderTaking.Workflow.PlaceOrder.Database.DTO
import BoundedContext.OrderTaking.Workflow.PlaceOrder.Domain

import Data.StringN

export toPricedOrderDTO : PricedOrder -> PricedOrderDTO
export toProductCodeDTO : ProductCode -> ProductCodeDTO
export toProductDTO : Product -> ProductDTO


orderIdentifier         : OrderId     -> Identifier
orderLineIdentifier     : OrderLineId -> Identifier
fromPrice               : Price       -> Double
toCustomerDTO           : Identifier  -> CustomerInfo    -> CustomerDTO
toAddressDTO            : Identifier  -> Address         -> AddressDTO
fromBillingAddress      : Identifier  -> BillingAddress  -> AddressDTO
fromShippingAddress     : Identifier  -> ShippingAddress -> AddressDTO
toPricedOrderLineDTO    : Identifier  -> PricedOrderLine -> PricedOrderLineDTO

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

toProductCodeDTO (WidgetProduct (MkWidgetCode x)) = MkProductCodeDTO x
toProductCodeDTO (GizmoProduct (MkGizmoCode x))   = MkProductCodeDTO x

toProductDTO (MkProduct productCode price description)
  = MkProductDTO
    { productCode = toProductCodeDTO productCode
    , price       = Price.value price
    , description = StringN.value description
    }
