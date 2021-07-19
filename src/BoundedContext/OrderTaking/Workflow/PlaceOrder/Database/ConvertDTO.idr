module BoundedContext.OrderTaking.Workflow.PlaceOrder.Database.ConvertDTO

import BoundedContext.OrderTaking.Workflow.PlaceOrder.Database.DTO
import BoundedContext.OrderTaking.Workflow.PlaceOrder.Domain

import Data.StringN

export toPricedOrderDTO : PricedOrder -> PricedOrderDTO
export toProductCodeDTO : ProductCode -> ProductCodeDTO
export toProductDTO : Product -> ProductDTO


orderIdentifier         : OrderId     -> Identifier
orderLineIdentifier     : OrderLineId -> Identifier
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
        , amount          = p.amountToBill.value
        }

toCustomerDTO i c
  = MkCustomerDTO
    { identifier   = i
    , firstName    = c.personalName.firstName.value
    , lastName     = c.personalName.lastName.value
    , emailAddress = c.emailAddress.value
    }

toAddressDTO i a
  = MkAddressDTO
    { identifier   = i
    , addressLine1 = a.addressLine1.value
    , addressLine2 = map (.value) a.addressLine2
    , addressLine3 = map (.value) a.addressLine3
    , addressLine4 = map (.value) a.addressLine4
    , city         = a.city.value
    , zipCode      = a.zipCode.value
    }

fromBillingAddress  i (MkBillingAddress  ba) = toAddressDTO (i ++ "-BLN") ba
fromShippingAddress i (MkShippingAddress sa) = toAddressDTO (i ++ "-SHP") sa

toPricedOrderLineDTO i po
  = MkPricedOrderLineDTO
    { identifier  = i ++ "-PO-" ++ orderLineIdentifier po.orderLine.orderLineId
    , productCode = po.orderLine.productCode.value
    , quantity    = po.orderLine.quantity.value
    , price       = po.price.value
    }

-- Product

toProductCodeDTO (WidgetProduct (MkWidgetCode x)) = MkProductCodeDTO x
toProductCodeDTO (GizmoProduct (MkGizmoCode x))   = MkProductCodeDTO x

toProductDTO (MkProduct productCode price description)
  = MkProductDTO
    { productCode = toProductCodeDTO productCode
    , price       = price.value
    , description = description.value
    }
