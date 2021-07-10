module BoundedContext.OrderTaking.ConvertDTO

import BoundedContext.OrderTaking.DTO
import BoundedContext.OrderTaking.Command
import BoundedContext.OrderTaking.Error
import BoundedContext.OrderTaking.Event

import BoundedContext.OrderTaking.Workflow.PlaceOrder.Domain
import BoundedContext.OrderTaking.Workflow.PlaceOrder.DTO
import BoundedContext.OrderTaking.Workflow.PlaceOrder.ConvertDTO

export
toErrorDTO : Error.OrderTaking -> ErrorDTO
toErrorDTO (PlaceOrder x) = PlaceOrderErrorDTO $ toPlacedOrderErrorDTO x

export
toEventDTO : Event.OrderTaking -> EventDTO
toEventDTO (PlaceOrder xs) = PlaceOrderEventDTO $ map toPlaceOrderEventDTO xs

export
fromCommandDTO : CommandDTO -> Command.OrderTaking
fromCommandDTO (PlaceOrderCmdDTO x) = PlaceOrder (orderForm x)
