module OrderTaking.BoundedContext.PlaceOrder

import Rango.BoundedContext.Workflow
import OrderTaking.Domain.PlaceOrder

%default total

data PlaceOrderData
  = OrderForm
  | ValidatedOrder
  | Order
  | PricedOrder
  | InvalidOrder
  | InvalidOrderQueued
  | Finished

data Chk : PlaceOrderData -> PlaceOrderData -> PlaceOrderData -> Type where
  CheckInvalidOrder : Chk ValidatedOrder InvalidOrder Order

data Cmd : PlaceOrderData -> PlaceOrderData -> Type where
  ValidateOrder     : Cmd OrderForm           ValidatedOrder
  AddInvalidOrder   : Cmd InvalidOrder        InvalidOrderQueued
  PriceOrder        : Cmd Order               PricedOrder
  SendAckToCustomer : Cmd PricedOrder         Finished
  SendInvalidOrder  : Cmd InvalidOrderQueued  Finished

workflow : Workflow Cmd Chk OrderForm Finished
workflow = do
  Do ValidateOrder
  Branch CheckInvalidOrder
    (do Do AddInvalidOrder
        Do SendInvalidOrder)
    (do Do PriceOrder
        Do SendAckToCustomer)

POStateType : PlaceOrderData -> Type
POStateType OrderForm          = PlaceOrder.OrderForm
POStateType ValidatedOrder     = Either PlaceOrder.InvalidOrder PlaceOrder.Order
POStateType Order              = PlaceOrder.Order
POStateType PricedOrder        = PlaceOrder.PricedOrder
POStateType InvalidOrder       = PlaceOrder.InvalidOrder
POStateType InvalidOrderQueued = List PlacedOrderEvent
POStateType Finished           = List PlacedOrderEvent

poRunCmd : Cmd s e -> (POStateType s) -> POM (POStateType e)
poRunCmd ValidateOrder     st = validateOrder st
poRunCmd AddInvalidOrder   st = pure [InvalidOrderRegistered st]
poRunCmd PriceOrder        st = priceOrder st
poRunCmd SendAckToCustomer st = do
  ack <- acknowledgeOrder st
  placePricedOrder st
  pure $ createEvents st ack
poRunCmd SendInvalidOrder  st = pure st

poRunChk : Chk s b1 b2 -> (POStateType s) -> POM (Either (POStateType b1) (POStateType b2))
poRunChk CheckInvalidOrder st = pure st

poInterpreter : Morphism PlaceOrderData POM Cmd Chk
poInterpreter = MkMorphism
  { StateType = POStateType
  , command   = poRunCmd
  , check     = poRunChk
  }

