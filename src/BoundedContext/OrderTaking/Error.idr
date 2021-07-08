module BoundedContext.OrderTaking.Error

import BoundedContext.OrderTaking.Workflow.PlaceOrder.Domain

public export
data OrderTaking
  = PlaceOrder PlaceOrderError

