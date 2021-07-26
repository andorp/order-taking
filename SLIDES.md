# Slides

## High level overview

### Idealized view of software projects

```
┌−−−−−−−−−−−−−−−−−−−−−−−−−−−┐
╎  Software Project         ╎
╎                           ╎
╎ ┌───────────────────┐     ╎
╎ │   Requirements    │─┐   ╎
╎ └───────────────────┘ │   ╎
╎   │                   │   ╎
╎   ▼                   │   ╎
╎ ┌───────────────────┐ │   ╎
╎ │ Quality Assurence │ │   ╎
╎ └───────────────────┘ │   ╎
╎   │                   │   ╎
╎   ▼                   │   ╎
╎ ┌───────────────────┐ │   ╎
╎ │  Implementation   │◀┘   ╎
╎ └───────────────────┘     ╎
└−−−−−−−−−−−−−−−−−−−−−−−−−−−┘
```

### Semantic tower of a tipical software project

```
┌−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┐
╎         Semantic Tower         ╎
╎                                ╎
╎ ┌────────────────────────────┐ ╎
╎ │       Design diagram       │ ╎
╎ └────────────────────────────┘ ╎
╎ ┌────────────────────────────┐ ╎
╎ │      Detailed design       │ ╎
╎ └────────────────────────────┘ ╎
╎ ┌────────────────────────────┐ ╎
╎ │ Data / Behavior definition │ ╎
╎ └────────────────────────────┘ ╎
╎ ┌────────────────────────────┐ ╎
╎ │       Implementation       │ ╎
╎ └────────────────────────────┘ ╎
╎ ┌────────────────────────────┐ ╎
╎ │         Libraries          │ ╎
╎ └────────────────────────────┘ ╎
╎ ┌────────────────────────────┐ ╎
╎ │      Tech environment      │ ╎
╎ └────────────────────────────┘ ╎
└−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┘
```

### Realization of a software project

```
┌−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┐
╎             Real Software Project             ╎
╎                                               ╎
╎ ┌───────────────────────────────────────────┐ ╎
╎ │       Requirement docs, JIRA items        │ ╎
╎ └───────────────────────────────────────────┘ ╎
╎ ┌───────────────────────────────────────────┐ ╎
╎ │      Implemented data and behaviour       │ ╎
╎ └───────────────────────────────────────────┘ ╎
╎ ┌───────────────────────────────────────────┐ ╎
╎ │ Some kind tests; manual, unit, end-to-end │ ╎
╎ └───────────────────────────────────────────┘ ╎
└−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┘
```


### Realization of a software project including the semantic tower

```
┌−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┐
╎                Real Software Project               ╎
╎                                                    ╎
╎ ┌────────────────────────────────────────────────┐ ╎
╎ │            Requirement docs, JIRA items        │ ╎
╎ │                                                │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │        Design Diagram      │         │ ╎
╎ │         └────────────────────────────┘         │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │        Detailed Design     │         │ ╎
╎ │         └────────────────────────────┘         │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │ Data / Behavior definition │         │ ╎
╎ │         └────────────────────────────┘         │ ╎
╎ └────────────────────────────────────────────────┘ ╎
╎ ┌────────────────────────────────────────────────┐ ╎
╎ │         Implemented data and behaviour         │ ╎
╎ │                                                │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │       Implementation       │         │ ╎
╎ │         └────────────────────────────┘         │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │          Libraries         │         │ ╎
╎ │         └────────────────────────────┘         │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │       Tech environment     │         │ ╎
╎ │         └────────────────────────────┘         │ ╎
╎ └────────────────────────────────────────────────┘ ╎
╎ ┌────────────────────────────────────────────────┐ ╎
╎ │    Some kind tests; manual, unit, end-to-end   │ ╎
╎ │                                                │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │       Implementation       │         │ ╎
╎ │         └────────────────────────────┘         │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │          Libraries         │         │ ╎
╎ │         └────────────────────────────┘         │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │      Tech environment      │         │ ╎
╎ │         └────────────────────────────┘         │ ╎
╎ └────────────────────────────────────────────────┘ ╎
└−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┘
```

### Change request of any kind

```
┌−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┐
╎          Semantic Tower         ╎
╎                                 ╎
╎ ┌─────────────────────────────┐ ╎
╎ │        Design diagram       │ ╎ ◀─ Change should propagate down
╎ └─────────────────────────────┘ ╎
╎ ┌─────────────────────────────┐ ╎
╎ │       Detailed design       │ ╎
╎ └─────────────────────────────┘ ╎
╎ ┌─────────────────────────────┐ ╎
╎ │  Data / Behavior definition │ ╎ ◀─ Change should propagete up and down
╎ └─────────────────────────────┘ ╎
╎ ┌─────────────────────────────┐ ╎
╎ │        Implementation       │ ╎
╎ └─────────────────────────────┘ ╎
╎ ┌─────────────────────────────┐ ╎
╎ │          Libraries          │ ╎
╎ └─────────────────────────────┘ ╎
╎ ┌─────────────────────────────┐ ╎
╎ │       Tech environment      │ ╎ ◀─ Change could tear down all the project,
╎ └─────────────────────────────┘ ╎    but definetly could destroy everything up
└−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┘    to the behavioral definition.
```

### Change request in realized software project

```
┌−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┐
╎                Real Software Project               ╎
╎                                                    ╎
╎ ┌────────────────────────────────────────────────┐ ╎
╎ │            Requirement docs, JIRA items        │ ╎
╎ │                                                │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │        Design Diagram      │         │ ╎ ◀─ Change here indicates ... what?
╎ │         └────────────────────────────┘         │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │        Detailed Design     │         │ ╎
╎ │         └────────────────────────────┘         │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │ Data / Behavior definition │         │ ╎ ◀─ Change here indicates ... what?
╎ │         └────────────────────────────┘         │ ╎
╎ └────────────────────────────────────────────────┘ ╎
╎ ┌────────────────────────────────────────────────┐ ╎
╎ │         Implemented data and behaviour         │ ╎
╎ │                                                │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │       Implementation       │         │ ╎
╎ │         └────────────────────────────┘         │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │          Libraries         │         │ ╎
╎ │         └────────────────────────────┘         │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │       Tech environment     │         │ ╎ ◀─ Change here indicates ... what?
╎ │         └────────────────────────────┘         │ ╎
╎ └────────────────────────────────────────────────┘ ╎
╎ ┌────────────────────────────────────────────────┐ ╎
╎ │    Some kind tests; manual, unit, end-to-end   │ ╎
╎ │                                                │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │       Implementation       │         │ ╎
╎ │         └────────────────────────────┘         │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │          Libraries         │         │ ╎
╎ │         └────────────────────────────┘         │ ╎
╎ │         ┌────────────────────────────┐         │ ╎
╎ │         │      Tech environment      │         │ ╎
╎ │         └────────────────────────────┘         │ ╎
╎ └────────────────────────────────────────────────┘ ╎
└−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┘
```

### Domain Driven Design

Focuses on the **Data / Behavioral definition**. DDD models the software a collection of
bounded contextes that interact with each other.

```
┌−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┐
╎                    Bounded Context                    ╎
╎                                                       ╎
╎                   ┌───────────────┐                   ╎
╎   ┌────────────── │    Command    │ ──────┐           ╎
╎   │               └───────────────┘       │           ╎
╎   │                 │                     │           ╎
╎   │                 │                     │           ╎
╎   ▼                 ▼                     ▼           ╎
╎ ┌───────────┐     ┌───────────────┐     ┌───────────┐ ╎
╎ │ Workflow  │     │    Workflow   │     │ Workflow  │ ╎
╎ └───────────┘     └───────────────┘     └───────────┘ ╎
╎   │                 │                     │           ╎
╎   │                 │                     │           ╎
╎   │                 ▼                     │           ╎
╎   │               ┌───────────────┐       │           ╎
╎   └─────────────▶ │     Event     │ ◀─────┘           ╎
╎                   └───────────────┘                   ╎
└−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┘
```

### Behaviour described in workflows

For example

```
┌−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┐                                    
╎                                    ┌────────────────────────────┐ ╎
╎                                    │         OrderForm          │ ╎
╎                                    └────────────────────────────┘ ╎
╎                                      │                            ╎
╎                                      │ Validate Order             ╎
╎                                      ▼                            ╎
╎ ┌─────────────┐    Create valid    ┌============================┐ ╎
╎ │ ValidOrder  │   ◀─────────────── I           Order            I ╎
╎ └─────────────┘                    └============================┘ ╎
╎   │                                  │                            ╎
╎   │ Price                            │ Create invalid             ╎
╎   ▼                                  ▼                            ╎
╎ ┌─────────────┐                    ┌────────────────────────────┐ ╎
╎ │ PricedOrder │                    │        InvalidOrder        │ ╎
╎ └─────────────┘                    └────────────────────────────┘ ╎
╎   │                                  │                            ╎
╎   │                                  │ Queue                      ╎
╎   │                                  ▼                            ╎
╎   │                                ┌────────────────────────────┐ ╎
╎   │                                |     InvalidOrderQeued      │ ╎
╎   │                                └────────────────────────────┘ ╎
╎   │                                  │                            ╎
╎   │                                  │ Create invalid order info  ╎
╎   │                                  ▼                            ╎
╎   │  Create priced order info      ┌────────────────────────────┐ ╎
╎   └────────────────────────────▶   │         OrderInfo          │ ╎
╎                                    └────────────────────────────┘ ╎
└−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┘
```

### The missing link of the semantic tower

Layers may depend on each other. With dependent types we can express this connection.
If layer n of the semantic tower is a function of layer n-1 than any change in
one layer, will propagate through the semantic tower.

```
┌−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┐
╎         Semantic Tower         ╎
╎                                ╎
╎ ┌────────────────────────────┐ ╎
╎ │       Design diagram       │ ╎
╎ └────────────────────────────┘ ╎
╎   │                            ╎
╎   ▼                            ╎
╎ ┌────────────────────────────┐ ╎
╎ │      Detailed design       │ ╎
╎ └────────────────────────────┘ ╎
╎   │                            ╎
╎   ▼                            ╎
╎ ┌────────────────────────────┐ ╎
╎ │ Data / Behavior definition │ ╎ ◀─ Change *will* propagete up and down
╎ └────────────────────────────┘ ╎
╎   │                            ╎
╎   ▼                            ╎
╎ ┌────────────────────────────┐ ╎
╎ │       Implementation       │ ╎
╎ └────────────────────────────┘ ╎
╎   │                            ╎
╎   ▼                            ╎
╎ ┌────────────────────────────┐ ╎
╎ │         Libraries          │ ╎
╎ └────────────────────────────┘ ╎
╎   │                            ╎
╎   ▼                            ╎
╎ ┌────────────────────────────┐ ╎
╎ │      Tech environment      │ ╎
╎ └────────────────────────────┘ ╎
└−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┘
```

## Implementation details

### Focus on the most important part: Workflow

Explicit connection between high level definition of the workflow and its implementation.

- [PlaceOrder.Overview](https://github.com/andorp/order-taking/blob/main/src/BoundedContext/OrderTaking/Workflow/PlaceOrder/Overview.idr)
- [Rango.BoundedContext.Workflow DSL](https://github.com/andorp/order-taking/blob/main/src/Rango/BoundedContext/Workflow.idr)

The implementation of the workflow, is a function from the high level description of the workflow to a monadic state transition, via
the `morph` helper function:

```
placeOrder : Workflow Transition Check OrderForm OrderInfo
placeOrder = do
  Do ValidateOrder
  Branch CheckInvalidOrder
    (do Do AddInvalidOrder
        Do SendInvalidOrder)
    (do Do PriceOrder
        Do SendAckToCustomer)
```

```
record Morphism (monad : Type -> Type) state (cmd : state -> state -> Type) (chk : state -> state -> state -> Type)
  where
    constructor MkMorphism
    StateType : state -> Type
    command   : cmd s e -> (StateType s) -> monad (StateType e)
    check     : chk s b1 b2 -> (StateType s) -> monad (Either (StateType b1) (StateType b2))
```

```
morph
  ...
  -> (r : Morphism monad state cmd chk)
  -> Workflow cmd chk start end
  -> (StateType r start) -> monad (StateType r end)
morph r w = ...
```

### Simplest architecture with workflow abstraction included

```
┌−−−−−−−−−−−−−−−−−−−−−−−−−−┐
╎       Architecture       ╎
╎                          ╎
╎ ┌──────────────────────┐ ╎
╎ │ Place Order Workflow │ ╎
╎ └──────────────────────┘ ╎
╎ ┌──────────────────────┐ ╎
╎ │  Place Order Monad   │ ╎
╎ └──────────────────────┘ ╎
╎ ┌──────────────────────┐ ╎
╎ │ NodeJS Promise Monad │ ╎
╎ └──────────────────────┘ ╎
└−−−−−−−−−−−−−−−−−−−−−−−−−−┘
```

### Full architecture

```
┌−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┐
╎           Architecture           ╎
╎                                  ╎
╎ ┌──────────────────────────────┐ ╎
╎ │ Order Taking Bounded Context │ ╎
╎ └──────────────────────────────┘ ╎
╎ ┌──────────────────────────────┐ ╎
╎ │     Place Order Workflow     │ ╎
╎ └──────────────────────────────┘ ╎
╎ ┌──────────────────────────────┐ ╎
╎ │ Place Order DSL (Free Monad) │ ╎
╎ └──────────────────────────────┘ ╎
╎ ┌──────────────────────────────┐ ╎
╎ │  Place Order Backend Monad   │ ╎
╎ └──────────────────────────────┘ ╎
╎ ┌──────────────────────────────┐ ╎
╎ │ Place Order Database Monads  │ ╎
╎ └──────────────────────────────┘ ╎
╎ ┌──────────────────────────────┐ ╎
╎ │     NodeJS Promise Monad     │ ╎
╎ └──────────────────────────────┘ ╎
╎ ┌──────────────────────────────┐ ╎
╎ │        NodeJS Runtime        │ ╎
╎ └──────────────────────────────┘ ╎
└−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┘
```

### Bounded Context implementation

[Source](https://github.com/andorp/order-taking/blob/main/src/Rango/BoundedContext/BoundedContext.idr#L52)

```idris
record BoundedContext where
  constructor MkBoundedContext
  command     : Type
  workflow    : Type
  event       : Type
  workflowOf  : command -> workflow
  eventOf     : command -> event
```

[Source](https://github.com/andorp/order-taking/blob/main/src/Rango/BoundedContext/BoundedContext.idr#L183)

```idris
boundedContext
  :  (Monad m)
  => (bc : BoundedContextImplementation m) -> bc.contextCommand -> m (Either bc.contextError bc.contextEvent)
boundedContext bc contextCommand = do
  (cmd ** cmdData) <- bc.createCommand contextCommand
  workflowRunner <- bc.createWorkflowEmbedding cmd
  let workflowMonadInstance = bc.workflowMonadInstance (bc.context.workflowOf cmd) -- For transformWorkflow
  input  <- bc.createStartState cmd cmdData
  result <- runEmbedding workflowRunner (transformWorkflow (bc.workflow (bc.context.workflowOf cmd)) (bc.workflowMorphism cmd) input)
  case result of
    Left err => map Left (bc.createFinalError (bc.context.workflowOf cmd) err)
    Right wfVal => do
      evVal <- bc.createWorkflowEvent cmd wfVal
      map Right (bc.createFinalEvent cmd evVal)
```

- Bounded Context computation lives in its monad
- Every workflow has its own monad, which must be embeddable to the monad of the bounded context
- Note the dependentt pair and its use `(cmd ** cmdData) <- bc.createCommand contextCommand`

### Full architecture, explained

- [Order Taking Bounded Context](https://github.com/andorp/order-taking/blob/main/src/BoundedContext/OrderTaking.idr#L45)  
  Simple enum like ADTs to name commands, workflows, events
- [Place Order Workflow](https://github.com/andorp/order-taking/blob/main/src/BoundedContext/OrderTaking/Workflow/PlaceOrder/Overview.idr#L95)  
  Indexed datatype to represent workflow as state transition system.
- [Place Order DSL (Free Monad)](https://github.com/andorp/order-taking/blob/main/src/BoundedContext/OrderTaking/Workflow/PlaceOrder/Domain.idr#L455)  
  Detailed information in datatypes, building blocks of behaviour,  
  implementation of workflow steps based on building blocks.
- [Place Order Backend Monad](https://github.com/andorp/order-taking/blob/main/src/BoundedContext/OrderTaking/Workflow/PlaceOrder/Backend.idr#L185)  
  Model of the Place Order DSL, real implementation of the building blocks
- [Place Order Database Monads](https://github.com/andorp/order-taking/blob/main/src/BoundedContext/OrderTaking/Workflow/PlaceOrder/Database/Order.idr#L150)  
  Connect to external sources.
- [NodeJS Promise Monad](https://github.com/andorp/order-taking/blob/main/src/Service/NodeJS/Promise.idr#L6)  
  Make run everything with event driven approach of NodeJS.
- [NodeJS Runtime](https://github.com/andorp/order-taking/tree/main/src/Service/NodeJS)  
  Use third party libraries, such as HTTP, SQLite

### TODO: More slides ...
