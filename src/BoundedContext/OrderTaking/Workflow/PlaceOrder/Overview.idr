module BoundedContext.OrderTaking.Workflow.PlaceOrder.Overview

import Rango.BoundedContext.Workflow

%default total

public export
data State
  = OrderForm
  | ValidatedOrder
  | Order
  | PricedOrder
  | InvalidOrder
  | InvalidOrderQueued
  | Finished

public export
data Check : State -> State -> State -> Type where
  CheckInvalidOrder : Check ValidatedOrder InvalidOrder Order

public export
data Transition : State -> State -> Type where
  ValidateOrder     : Transition OrderForm           ValidatedOrder
  AddInvalidOrder   : Transition InvalidOrder        InvalidOrderQueued
  PriceOrder        : Transition Order               PricedOrder
  SendAckToCustomer : Transition PricedOrder         Finished
  SendInvalidOrder  : Transition InvalidOrderQueued  Finished

public export
workflow : Workflow Transition Check OrderForm Finished
workflow = do
  Do ValidateOrder
  Branch CheckInvalidOrder
    (do Do AddInvalidOrder
        Do SendInvalidOrder)
    (do Do PriceOrder
        Do SendAckToCustomer)
