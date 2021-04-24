module OrderTaking.BoundedContext.PlaceOrder

import Rango.BoundedContext.Workflow
import OrderTaking.Domain.OrderTaking

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

poStateType : PlaceOrderData -> Type
poStateType OrderForm          = OrderTaking.OrderForm
poStateType ValidatedOrder     = Either OrderTaking.InvalidOrder OrderTaking.Order
poStateType Order              = OrderTaking.Order
poStateType PricedOrder        = OrderTaking.PricedOrder
poStateType InvalidOrder       = OrderTaking.InvalidOrder
poStateType InvalidOrderQueued = List PlacedOrderEvent
poStateType Finished           = List PlacedOrderEvent

poRunCmd : Cmd s e -> (poStateType s) -> POM (poStateType e)
poRunCmd ValidateOrder     st = OrderTaking.validateOrder st
poRunCmd AddInvalidOrder   st = do
  pure [InvalidOrderRegistered st]
poRunCmd PriceOrder        st = OrderTaking.priceOrder st
poRunCmd SendAckToCustomer st = do
  ack <- OrderTaking.acknowledgeOrder st
  pure $ OrderTaking.createEvents st ack
poRunCmd SendInvalidOrder  st = pure st

poRunChk : Chk s b1 b2 -> (poStateType s) -> POM (Either (poStateType b1) (poStateType b2))
poRunChk CheckInvalidOrder st = pure st

poInterpreter : Interpreter PlaceOrderData POM Cmd Chk
poInterpreter = MkRunner
  { stateType  = poStateType
  , runCommand = poRunCmd
  , runCheck   = poRunChk
  }

