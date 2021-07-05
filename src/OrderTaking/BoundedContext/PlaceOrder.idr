module OrderTaking.BoundedContext.PlaceOrder

import Rango.BoundedContext.Workflow
import OrderTaking.Domain.PlaceOrder

%default total

export
data PlaceOrderData
  = OrderForm
  | ValidatedOrder
  | Order
  | PricedOrder
  | InvalidOrder
  | InvalidOrderQueued
  | Finished

export
data Check : PlaceOrderData -> PlaceOrderData -> PlaceOrderData -> Type where
  CheckInvalidOrder : Check ValidatedOrder InvalidOrder Order

export
data Transition : PlaceOrderData -> PlaceOrderData -> Type where
  ValidateOrder     : Transition OrderForm           ValidatedOrder
  AddInvalidOrder   : Transition InvalidOrder        InvalidOrderQueued
  PriceOrder        : Transition Order               PricedOrder
  SendAckToCustomer : Transition PricedOrder         Finished
  SendInvalidOrder  : Transition InvalidOrderQueued  Finished

export
workflow : Workflow Transition Check OrderForm Finished
workflow = do
  Do ValidateOrder
  Branch CheckInvalidOrder
    (do Do AddInvalidOrder
        Do SendInvalidOrder)
    (do Do PriceOrder
        Do SendAckToCustomer)

public export
POStateType : PlaceOrderData -> Type
POStateType OrderForm          = PlaceOrder.OrderForm
POStateType ValidatedOrder     = Either PlaceOrder.InvalidOrder PlaceOrder.Order
POStateType Order              = PlaceOrder.Order
POStateType PricedOrder        = PlaceOrder.PricedOrder
POStateType InvalidOrder       = PlaceOrder.InvalidOrder
POStateType InvalidOrderQueued = List PlacedOrderEvent
POStateType Finished           = List PlacedOrderEvent

pomTransition : Transition s e -> (POStateType s) -> POM (POStateType e)
pomTransition ValidateOrder     st = validateOrder st
pomTransition AddInvalidOrder   st = pure [InvalidOrderRegistered st]
pomTransition PriceOrder        st = priceOrder st
pomTransition SendAckToCustomer st = do
  ack <- acknowledgeOrder st
  placePricedOrder st
  pure $ createEvents st ack
pomTransition SendInvalidOrder  st = pure st

pomCheck : Check s b1 b2 -> (POStateType s) -> POM (Either (POStateType b1) (POStateType b2))
pomCheck CheckInvalidOrder st = pure st

public export
withPOMMapping : Morphism PlaceOrderData POM Transition Check
withPOMMapping = MkMorphism
  { StateType = POStateType
  , command   = pomTransition
  , check     = pomCheck
  }
