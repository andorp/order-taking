module BoundedContext.OrderTaking.Event

import BoundedContext.OrderTaking.Workflow.PlaceOrder.Domain

public export
data OrderTaking
  = PlaceOrder (List PlacedOrderEvent)
