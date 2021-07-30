module BoundedContext.OrderTaking.Workflow.PlaceOrder

import public BoundedContext.OrderTaking.Workflow.PlaceOrder.Overview
import public BoundedContext.OrderTaking.Workflow.PlaceOrder.Domain
import public Rango.BoundedContext.Workflow


public export
StateType : Overview.State -> Type
StateType OrderForm          = Domain.OrderForm
StateType Order              = Either Domain.InvalidOrder Domain.Order
StateType ValidOrder         = Domain.Order
StateType PricedOrder        = Domain.PricedOrder
StateType InvalidOrder       = Domain.InvalidOrder
StateType InvalidOrderQueued = List Domain.PlacedOrderEvent
StateType OrderInfo          = List Domain.PlacedOrderEvent

step : Overview.Step s e -> (StateType s) -> PlaceOrderDSL (StateType e)
step ValidateOrder     st = validateOrder st
step AddInvalidOrder   st = pure [InvalidOrderRegistered st]
step PriceOrder        st = priceOrder st
step SendAckToCustomer st = do
  ack <- acknowledgeOrder st
  placePricedOrder st
  pure $ createEvents st ack
step SendInvalidOrder  st = pure st

check : Check s b1 b2 -> (StateType s) -> PlaceOrderDSL (Either (StateType b1) (StateType b2))
check CheckInvalidOrder st = pure st

public export
PlaceOrderMorphism : Morphism PlaceOrderDSL Overview.State Overview.Step Overview.Check
PlaceOrderMorphism = MkMorphism
  { StateType = StateType
  , step      = step
  , check     = check
  }
