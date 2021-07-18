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
    { command     = Command
    , workflow    = Workflow
    , event       = Event
    , workflowOf  = workflowOf
    , eventOf     = eventOf
    }

  commandDomainType : C.Command -> Type
  commandDomainType PlaceOrder = OrderForm

  eventDomainType : E.Event -> Type
  eventDomainType PlaceOrder = List PlacedOrderEvent

  workflowContexts : W.Workflow -> WorkflowEnv
  workflowContexts PlaceOrder = mkWorkflowEnv PlaceOrder.Overview.workflow

  workflowMonad : W.Workflow -> (Type -> Type)
  workflowMonad PlaceOrder = POM

  errorDomainType : W.Workflow -> Type
  errorDomainType PlaceOrder = PlaceOrderError

  createWorkflowEmbedding
    :  (cmd : Command)
    -> Promise (Embedding (workflowMonad (workflowOf cmd)) (errorDomainType (workflowOf cmd)) Promise)
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
            (WorkflowEnv.state (workflowContexts (w)))
            (workflowMonad w)
            (WorkflowEnv.command (workflowContexts w))
            (WorkflowEnv.branch (workflowContexts w))
  workflowMorphism PlaceOrder = POMMorphism
 
  createWorkflowEvent
    :  (cmd : Command)
    -> let m = workflowMorphism cmd
       in m.StateType (WorkflowEnv.end (workflowContexts (workflowOf cmd))) -> Promise (eventDomainType (eventOf cmd))
  createWorkflowEvent PlaceOrder x = pure x

  createFinalEvent : (cmd : Command) -> eventDomainType (eventOf cmd) -> Promise Event.OrderTaking
  createFinalEvent PlaceOrder x = pure $ PlaceOrder x

  createCommand : Command.OrderTaking -> Promise (cmd : Command ** commandDomainType cmd)
  createCommand (PlaceOrder x) = pure (PlaceOrder ** x)

  createFinalError : (wf : Workflow) -> errorDomainType wf -> Promise Error.OrderTaking
  createFinalError PlaceOrder x = pure (PlaceOrder x)

  createStartState : (cmd : Command) -> commandDomainType cmd -> Promise ((workflowMorphism cmd).StateType ((workflowContexts (workflowOf cmd)).start))
  createStartState PlaceOrder x = pure x

  workflowMonadInstance : (w : Workflow) -> Monad (workflowMonad w)
  workflowMonadInstance PlaceOrder = %search

  public export
  orderTakingImpl : BoundedContextImplementation Promise
  orderTakingImpl = MkBoundedContextImplementation
    { context                 = orderTaking
    , workflow                = workflowContexts
    , contextCommand          = Command.OrderTaking
    , command                 = commandDomainType
    , contextEvent            = Event.OrderTaking
    , eventData               = eventDomainType
    , contextError            = Error.OrderTaking
    , errorData               = errorDomainType
    , workflowMonad           = workflowMonad
    , workflowMonadInstance   = workflowMonadInstance
    , workflowMorphism        = workflowMorphism
    , createWorkflowEmbedding = createWorkflowEmbedding
    , createWorkflowEvent     = createWorkflowEvent
    , createFinalEvent        = createFinalEvent
    , createCommand           = createCommand
    , createStartState        = createStartState
    , createFinalError        = createFinalError
    }