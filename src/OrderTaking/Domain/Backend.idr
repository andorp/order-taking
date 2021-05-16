module OrderTaking.Domain.Backend

import Control.Monad.Either
import OrderTaking.Domain.PlaceOrder


Backend : Type -> Type
Backend = EitherT PlaceOrderError IO

newOrderId : Backend OrderId
newOrderId = ?newOrderIdH

newOrderLineId : Backend OrderLineId
newOrderLineId = ?newOrderLineIdH

checkProductCodeExists : ProductCode -> Backend Bool
checkProductCodeExists productCode = ?cpce

checkAddressExists : AddressForm -> Backend (Either CheckedAddressValidationError CheckedAddress)
checkAddressExists addressForm = ?cae

getProductPrice : ProductCode -> Backend Price
getProductPrice productCode = ?gpp

createOrderAcknowledgementLetter : PricedOrder -> Backend HtmlString
createOrderAcknowledgementLetter pricedOrder = ?coal

sendOrderAcknowledgement : OrderAcknowledgement -> Backend AckSent
sendOrderAcknowledgement orderAcknowledgement = ?soa

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
  , createOrderAcknowledgementLetter = createOrderAcknowledgementLetter
  , sendOrderAcknowledgement         = sendOrderAcknowledgement
  }

