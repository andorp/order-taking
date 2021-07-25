module BoundedContext.OrderTaking.Workflow.PlaceOrder

import public BoundedContext.OrderTaking.Workflow.PlaceOrder.Overview
import public BoundedContext.OrderTaking.Workflow.PlaceOrder.Domain
import public Rango.BoundedContext.Workflow


public export
POStateType : Overview.State -> Type
POStateType OrderForm          = Domain.OrderForm
POStateType Order              = Either Domain.InvalidOrder Domain.Order
POStateType ValidOrder         = Domain.Order
POStateType PricedOrder        = Domain.PricedOrder
POStateType InvalidOrder       = Domain.InvalidOrder
POStateType InvalidOrderQueued = List Domain.PlacedOrderEvent
POStateType OrderInfo          = List Domain.PlacedOrderEvent

pomTransition : Overview.Transition s e -> (POStateType s) -> PlaceOrderDSL (POStateType e)
pomTransition ValidateOrder     st = validateOrder st
pomTransition AddInvalidOrder   st = pure [InvalidOrderRegistered st]
pomTransition PriceOrder        st = priceOrder st
pomTransition SendAckToCustomer st = do
  ack <- acknowledgeOrder st
  placePricedOrder st
  pure $ createEvents st ack
pomTransition SendInvalidOrder  st = pure st

pomCheck : Check s b1 b2 -> (POStateType s) -> PlaceOrderDSL (Either (POStateType b1) (POStateType b2))
pomCheck CheckInvalidOrder st = pure st

public export
PlaceOrderMorphism : Morphism PlaceOrderDSL Overview.State Overview.Transition Overview.Check
PlaceOrderMorphism = MkMorphism
  { StateType = POStateType
  , command   = pomTransition
  , check     = pomCheck
  }
