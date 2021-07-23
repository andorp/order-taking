module BoundedContext.OrderTaking.Workflow.PlaceOrder.Overview

import Rango.BoundedContext.Workflow

%default total

-- Overview of the Place-ValidOrder workflow:
--
-- The incoming order form gets validated
-- - if valid it is registered
-- - if not it is rejected

-- More precisely this can be represented as the following state transition:

-- 
--                                                 ┌────────────────────────────┐
--                                                 │         OrderForm          │
--                                                 └────────────────────────────┘
--                                                   │
--                                                   │ Validate Order
--                                                   ▼
-- ┌─────────────┐    Create valid                 ┌────────────────────────────┐
-- │ ValidOrder  │   ◀──────────────────────────   │           Order            │
-- └─────────────┘                                 └────────────────────────────┘
--   │                                               │
--   │ Price                                         │ Create invalid
--   ▼                                               ▼
-- ┌─────────────┐                                 ┌────────────────────────────┐
-- │ PricedOrder │                                 │        InvalidOrder        │
-- └─────────────┘                                 └────────────────────────────┘
--   │                                               │
--   │                                               │ Queue
--   │                                               ▼
--   │                                             ┌────────────────────────────┐
--   │                                             │     InvalidOrderQeued      │
--   │                                             └────────────────────────────┘
--   │                                               │
--   │                                               │ Create invalid order info
--   │                                               ▼
--   │               Create priced order info      ┌────────────────────────────┐
--   └─────────────────────────────────────────▶   │         OrderInfo          │
--                                                 └────────────────────────────┘
-- 

-- Nodes represent the information we have, and edges represent the commands
-- that transforms information in this workflow.
-- Commands that have the same starting node are checks/decisions, one possible
-- path is selected during the interpretation of this process.
-- 
-- As in the order taking example, when we have an Order, it can be valid or invalid.

-- Next step is to represent this state transition in Idris as indexed datatypes.

||| States of the Order-Taking 
public export
data State
  = OrderForm
  | Order
  | ValidOrder
  | PricedOrder
  | InvalidOrder
  | InvalidOrderQueued
  | OrderInfo

-- The State is a simple ADT with some constructors.

||| Check of the OrderTaking transition;
|||
||| Decide if an order is valid or invalid.
public export
data Check : State -> State -> State -> Type where
  CheckInvalidOrder : Check Order InvalidOrder ValidOrder

-- The Check is an indexed datatype with three indices, all the three indices are type of State.

||| State transition of the OrderTaking workflow.
public export
data Transition : State -> State -> Type where
  ValidateOrder     : Transition OrderForm           Order
  AddInvalidOrder   : Transition InvalidOrder        InvalidOrderQueued
  PriceOrder        : Transition ValidOrder          PricedOrder
  SendAckToCustomer : Transition PricedOrder         OrderInfo
  SendInvalidOrder  : Transition InvalidOrderQueued  OrderInfo

-- The Transition is another indexed datatype, with two indices of State values.

||| Workflow definiton of the state transition system.
|||
||| See the graph above. This workflow represents an order-taking
||| process, which accepts or rejects an order, if the order
||| is accepted than prices it, if the order is invalid
||| registers it as an invalid order. In both cases it sends
||| information about that state of the order to the customer
public export
workflow : Workflow Transition Check OrderForm OrderInfo
workflow = do
  Do ValidateOrder
  Branch CheckInvalidOrder
    (do Do AddInvalidOrder
        Do SendInvalidOrder)
    (do Do PriceOrder
        Do SendAckToCustomer)

-- In Idris the 'do' notation is tied to the the (>>) and the (>>=) bind operatiors, any type
-- that implements those operators is able to use the 'do' notation. This is a sintactical sugar
-- and helps us to write code which can be read naturally, in sequence and in branching.
-- This approach fits well to the sequential nature of the workflows.

---------------------------------------------------------------------------------------------------

-- Graph Source:
-- 
-- digraph {
-- 	OrderForm 	 	  -> Order    		   	[label="Validate Order"];
--     InvalidOrder 	  -> InvalidOrderQeued 	[label="Queue" ];
--     ValidOrder 		  -> PricedOrder       	[label="Price" ]; 
--     PricedOrder  	  -> OrderInfo          [label="Create priced order info"];
--     InvalidOrderQeued -> OrderInfo          [label="Create invalid order info"];
    
--     Order -> ValidOrder   [label="Create valid"];
--     Order -> InvalidOrder [label="Create invalid"];
-- }
-- 