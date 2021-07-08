module BoundedContext.OrderTaking

import BoundedContext.OrderTaking.Workflow.PlaceOrder
import BoundedContext.OrderTaking.Workflow.PlaceOrder.Backend
import BoundedContext.OrderTaking.Error
import BoundedContext.OrderTaking.Event
import BoundedContext.OrderTaking.Command

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

  boundedContextDefinition : BoundedContextDefinition
  boundedContextDefinition = MkBoundedContextDefinition
    { command     = Command
    , workflow    = Workflow
    , event       = Event
    , workflowOf  = workflowOf
    , eventOf     = eventOf
    }

  commandDomainType : Command -> Type
  commandDomainType PlaceOrder = OrderForm

  eventDomainType : E.Event -> Type
  eventDomainType PlaceOrder = List PlacedOrderEvent

  workflowContexts : Workflow -> WorkflowContext
  workflowContexts PlaceOrder = mkWorkflowContext PlaceOrder.Overview.workflow

  workflowMonads : Workflow -> (Type -> Type)
  workflowMonads PlaceOrder = POM

  errorDomainType : Workflow -> Type
  errorDomainType PlaceOrder = PlaceOrderError

  testMkWfRunner
    :  (cmd : Command)
    -> Promise (Embedding (workflowMonads (workflowOf cmd)) (errorDomainType (workflowOf cmd)) Promise)
  testMkWfRunner PlaceOrder = do
    let orderDBComp       = orderDBSQLite
    let productDBComp     = productDBSQlite
    let emailComp         = noEmail
    let checkAddressComp  = okCheckAddress
    rb <- mkRunBackend
    pure $ MkEmbedding (\type, x => map (the (Either PlaceOrderError type)) (runBackend rb (interpret backend x)))

  testWfMorp
    :  (cmd : Command)
    -> let w = workflowOf cmd
       in Morphism
            (WorkflowContext.state (workflowContexts (w)))
            (workflowMonads w)
            (WorkflowContext.command (workflowContexts w))
            (WorkflowContext.branch (workflowContexts w))
  testWfMorp PlaceOrder = POMMorphism
 
  testCmdEvTy
    :  (cmd : Command)
    -> let m = testWfMorp cmd
       in m.StateType (WorkflowContext.end (workflowContexts (workflowOf cmd))) -> Promise (eventDomainType (eventOf cmd))
  testCmdEvTy PlaceOrder x = pure x

  testEvFinal : (cmd : Command) -> eventDomainType (eventOf cmd) -> Promise Event.OrderTaking
  testEvFinal PlaceOrder x = pure $ PlaceOrder x

  testCmdInit : Command.OrderTaking -> Promise (cmd : Command ** commandDomainType cmd)
  testCmdInit (PlaceOrder x) = pure (PlaceOrder ** x)

  testErrConv : (wf : Workflow) -> errorDomainType wf -> Promise Error.OrderTaking
  testErrConv PlaceOrder x = pure (PlaceOrder x)

  testWfStart : (cmd : Command) -> commandDomainType cmd -> Promise ((testWfMorp cmd).StateType ((workflowContexts (workflowOf cmd)).start))
  testWfStart PlaceOrder x = pure x

  testWfMndI : (w : Workflow) -> Monad (workflowMonads w)
  testWfMndI PlaceOrder = %search

  public export
  testBCM : BoundedContext Promise
  testBCM = MkBoundedContext
    { context               = boundedContextDefinition
    , workflow              = workflowContexts
    , commandData           = Command.OrderTaking
    , command               = commandDomainType
    , event                 = Event.OrderTaking
    , eventData             = eventDomainType
    , error                 = Error.OrderTaking
    , errorData             = errorDomainType
    , workflowMonad         = workflowMonads
    , workflowMonadInstance = testWfMndI
    , workflowMorphism      = testWfMorp
    , workflowEmbedding     = testMkWfRunner
    , createEventData       = testCmdEvTy
    , createEvent           = testEvFinal
    , createCommand         = testCmdInit
    , createWorkflowInput   = testWfStart
    , createError           = testErrConv
    }
