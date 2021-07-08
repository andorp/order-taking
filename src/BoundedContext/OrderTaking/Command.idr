module BoundedContext.OrderTaking.Command

import BoundedContext.OrderTaking.Workflow.PlaceOrder.Domain

public export
data OrderTaking
  = PlaceOrder OrderForm

