module BoundedContext.OrderTaking

import public BoundedContext.OrderTaking.Error
import public BoundedContext.OrderTaking.Event
import public BoundedContext.OrderTaking.Command

import BoundedContext.OrderTaking.Workflow.PlaceOrder
import BoundedContext.OrderTaking.Workflow.PlaceOrder.Backend

import Rango.BoundedContext.BoundedContext
import Service.NodeJS.Promise


namespace Overview

  namespace C

    public export
    data Command
      = PlaceOrder

  namespace W

    public export
    data Workflow
      = PlaceOrder

  namespace E

    public export
    data Event
      = PlaceOrder

  public export
  workflowOf : Command -> Workflow
  workflowOf PlaceOrder = PlaceOrder

  public export
  eventOf : Command -> E.Event
  eventOf PlaceOrder = PlaceOrder

namespace Implementation

  public export
  orderTaking : BoundedContext
  orderTaking = MkBoundedContext
    { Command     = Command
    , Workflow    = Workflow
    , Event       = Event
    , workflowOf  = workflowOf
    , eventOf     = eventOf
    }

  WorkflowEntry : W.Workflow -> Type
  WorkflowEntry PlaceOrder = OrderForm

  EventDomainType : E.Event -> Type
  EventDomainType PlaceOrder = List PlacedOrderEvent

  workflowContexts : W.Workflow -> WorkflowEnv
  workflowContexts PlaceOrder = mkWorkflowEnv PlaceOrder.Overview.workflow

  WorkflowMonad : W.Workflow -> (Type -> Type)
  WorkflowMonad PlaceOrder = PlaceOrderDSL

  ErrorDomainType : W.Workflow -> Type
  ErrorDomainType PlaceOrder = PlaceOrderError

  createWorkflowEmbedding
    :  (cmd : Command)
    -> Promise (Embedding (WorkflowMonad (workflowOf cmd)) (ErrorDomainType (workflowOf cmd)) Promise)
  createWorkflowEmbedding PlaceOrder = do
    let orderDBComp       = orderDBSQLite
    let productDBComp     = productDBSQlite
    let emailComp         = noEmail
    let checkAddressComp  = okCheckAddress
    rb <- mkRunBackend
    pure $ MkEmbedding (\type, x => map (the (Either PlaceOrderError type)) (runBackend rb (interpret backend x)))

  workflowMorphism
    :  (cmd : Command)
    -> let w = workflowOf cmd
       in Morphism
            (WorkflowMonad w)
            (WorkflowEnv.state (workflowContexts (w)))
            (WorkflowEnv.Command (workflowContexts w))
            (WorkflowEnv.Branch (workflowContexts w))
  workflowMorphism PlaceOrder = PlaceOrderMorphism
 
  transformWorkflowResult
    :  (cmd : Command)
    -> let m = workflowMorphism cmd
       in m.StateType (WorkflowEnv.end (workflowContexts (workflowOf cmd))) -> Promise (EventDomainType (eventOf cmd))
  transformWorkflowResult PlaceOrder x = pure x

  transformEvent : (cmd : Command) -> EventDomainType (eventOf cmd) -> Promise Event.OrderTaking
  transformEvent PlaceOrder x = pure $ PlaceOrder x

  createCommand : Command.OrderTaking -> Promise (cmd : Command ** WorkflowEntry (workflowOf cmd))
  createCommand (PlaceOrder x) = pure (PlaceOrder ** x)

  transformError
    :  (wf : Workflow)
    -> ErrorDomainType wf -> Promise Error.OrderTaking
  transformError PlaceOrder x = pure (PlaceOrder x)

  createStartState
    :  (cmd : Command)
    -> WorkflowEntry (workflowOf cmd) -> Promise ((workflowMorphism cmd).StateType ((workflowContexts (workflowOf cmd)).start))
  createStartState PlaceOrder x = pure x

  WorkflowMonadInstance : (w : Workflow) -> Monad (WorkflowMonad w)
  WorkflowMonadInstance PlaceOrder = %search

  public export
  orderTakingContext : BoundedContextImplementation Promise
  orderTakingContext = MkBoundedContextImplementation
    { context                 = orderTaking
    , Workflow                = workflowContexts
    , ContextCommand          = Command.OrderTaking
    , WorkflowEntry           = WorkflowEntry
    , ContextEvent            = Event.OrderTaking
    , EventData               = EventDomainType
    , ContextError            = Error.OrderTaking
    , ErrorData               = ErrorDomainType
    , WorkflowMonad           = WorkflowMonad
    , WorkflowMonadInstance   = WorkflowMonadInstance
    , workflowMorphism        = workflowMorphism
    , createWorkflowEmbedding = createWorkflowEmbedding
    , transformWorkflowResult = transformWorkflowResult
    , transformEvent          = transformEvent
    , createCommand           = createCommand
    , createStartState        = createStartState
    , transformError          = transformError
    }
