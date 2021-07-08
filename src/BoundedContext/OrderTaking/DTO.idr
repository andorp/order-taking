module BoundedContext.OrderTaking.DTO

import BoundedContext.OrderTaking.Workflow.PlaceOrder.DTO

public export
data BoundedContextErrorDTO
  = PlaceOrderErrDTO PlaceOrderErrorDTO

public export
data BoundedContextEventDTO
  = PlaceOrderEvDTO (List PlaceOrderEventDTO)

