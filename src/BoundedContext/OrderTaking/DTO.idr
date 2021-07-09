module BoundedContext.OrderTaking.DTO

import BoundedContext.OrderTaking.Workflow.PlaceOrder.DTO

import Rango.DataTransfer.JSON.Derive

%language ElabReflection
%default total


public export
data ErrorDTO
  = PlaceOrderErrorDTO PlaceOrderErrorDTO

public export
data EventDTO
  = PlaceOrderEventDTO (List PlaceOrderEventDTO)

public export
data CommandDTO
  = PlaceOrderCmdDTO OrderFormDTO

export
ToJSON ErrorDTO where
  toJSON (PlaceOrderErrorDTO x) = constructorToJSON "PlaceOrderErrorDTO" x

export
ToJSON EventDTO where
  toJSON (PlaceOrderEventDTO x) = constructorToJSON "PlaceOrderEventDTO" x

export
FromJSON CommandDTO where
  fromJSON = constructorFromJSON "PlaceOrderCmdDTO" PlaceOrderCmdDTO
