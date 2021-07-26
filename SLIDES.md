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

```idris
placeOrder : Workflow Transition Check OrderForm OrderInfo
placeOrder = do
  Do ValidateOrder
  Branch CheckInvalidOrder
    (do Do AddInvalidOrder
        Do SendInvalidOrder)
    (do Do PriceOrder
        Do SendAckToCustomer)
```

```idris
record Morphism (monad : Type -> Type) state (cmd : state -> state -> Type) (chk : state -> state -> state -> Type)
  where
    constructor MkMorphism
    StateType : state -> Type
    command   : cmd s e -> (StateType s) -> monad (StateType e)
    check     : chk s b1 b2 -> (StateType s) -> monad (Either (StateType b1) (StateType b2))
```

```idris
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
╎ │        Runtime Monad         │ ╎ - NodeJS Promise
╎ └──────────────────────────────┘ ╎
╎ ┌──────────────────────────────┐ ╎
╎ │          Tech stack          │ ╎ - NodeJS Runtime
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
  => (bc : BoundedContextImplementation m) -> bc.ContextCommand -> m (Either bc.ContextError bc.ContextEvent)
boundedContext bc contextCommand = do
  (cmd ** cmdData) <- bc.createCommand contextCommand
  workflowRunner <- bc.createWorkflowEmbedding cmd
  let workflowMonadInstance = bc.WorkflowMonadInstance (bc.context.workflowOf cmd) -- For transformWorkflow
  input  <- bc.createStartState cmd cmdData
  result <- runEmbedding workflowRunner (transformWorkflow (bc.Workflow (bc.context.workflowOf cmd)) (bc.workflowMorphism cmd) input)
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

## Dependent types in Action

Tools of dependently typed programming and their use in software.

Examples here are in pseudo Idris; quantities, correct implicit parameters are omitted.

### Extrinsic state transitions (mapping)

```idris
data Workflow
      :  (cmd : state -> state -> Type)
      -> (chk : state -> state -> state -> Type)
      -> state
      -> state
      -> Type
  where
    Do : cmd pre post -> Workflow cmd chk pre post
    Branch : chk pre branch1 branch2
           -> Workflow cmd chk branch1 post
           -> Workflow cmd chk branch2 post
           -> Workflow cmd chk pre post
    (>>)
      :  Workflow cmd chk pre mid
      -> Workflow cmd chk mid post
      -> Workflow cmd chk pre post
```

```idris
record Morphism (monad : Type -> Type) state (cmd : state -> state -> Type) (chk : state -> state -> state -> Type)
  where
    constructor MkMorphism
    StateType : state -> Type
    command   : cmd s e -> (StateType s) -> monad (StateType e)
    check     : chk s b1 b2 -> (StateType s) -> monad (Either (StateType b1) (StateType b2))
```

```idris
morph
  ...
  -> (r : Morphism monad state cmd chk)
  -> Workflow cmd chk start end
  -> (StateType r start) -> monad (StateType r end)
morph r w = ...
```

### Bounded Context Implementation

Encapsulation of types in records

```idris
record BoundedContext where
  constructor MkBoundedContext
  command     : Type
  workflow    : Type
  event       : Type
  workflowOf  : command -> workflow
  eventOf     : command -> event
```

Pieces of the workflow puzzle; a type safe approach

```idris
record BoundedContextImplementation (monad : Type -> Type) where
  constructor MkBoundedContextImplementation
  context
    : BoundedContext
  Workflow
    : context.Workflow -> WorkflowEnv
  ContextCommand
    : Type
  Command
    : context.Command -> Type
  ContextEvent
    : Type
  EventData
    : context.Event -> Type
  ContextError
    : Type
  ErrorData
    : context.Workflow -> Type
  WorkflowMonad
    : context.Workflow -> (Type -> Type)
  WorkflowMonadInstance
    : (w : context.Workflow) -> Monad (WorkflowMonad w)
  workflowMorphism
    : (cmd : context.Command) ->
      let w = context.workflowOf cmd
          d = Workflow w
      in Morphism (WorkflowMonad w) (State d) (WorkflowEnv.Command d) (Branch d)
  createWorkflowEmbedding
    : (cmd : context.Command) ->
      let w = context.workflowOf cmd
      in monad (Embedding (WorkflowMonad w) (ErrorData w) monad)
  createWorkflowEvent
    : (cmd : context.Command) ->
      let m = workflowMorphism cmd
      in m.StateType (WorkflowEnv.end (Workflow (context.workflowOf cmd))) -> monad (EventData (context.eventOf cmd))
  createFinalEvent
    : (cmd : context.Command) -> EventData (context.eventOf cmd) -> monad ContextEvent
  createCommand
    : ContextCommand -> monad (cmd : context.Command ** Command cmd)
  createStartState
    : (cmd : context.Command) -> Command cmd -> 
      let m = workflowMorphism cmd
      in monad (m.StateType (WorkflowEnv.start (Workflow (context.workflowOf cmd))))
  createFinalError
    : (w : context.Workflow) -> (ErrorData w) -> monad ContextError
```

Even encapsulate Monad instances

```idris
  WorkflowMonad
    : context.Workflow -> (Type -> Type)
  WorkflowMonadInstance
    : (w : context.Workflow) -> Monad (WorkflowMonad w)
```

### Free Monadic DSL of a workflow

The Free Monad DSL allows us to easily replace the implementation. Idris is a multi-backend
compiler, with this approach the same solution can have multiple tech environments. Imagine
the same code deployed to NodeJS, Python, JVM, linux with ChezScheme.

Write the implementation in a Domain.

```idris
data PlaceOrderDSL : Type -> Type where
  Pure : a -> PlaceOrderDSL a
  Bind : PlaceOrderDSL a -> Inf (a -> PlaceOrderDSL b) -> PlaceOrderDSL b

  ThrowError : (a : Type) -> PlaceOrderError -> PlaceOrderDSL a
  CatchError : {a : Type} -> PlaceOrderDSL a -> PlaceOrderDSL (Either PlaceOrderError a)

  NewOrderId : PlaceOrderDSL OrderId
  NewOrderLineId : PlaceOrderDSL OrderLineId

  CheckProductCodeExists : ProductCode -> PlaceOrderDSL Bool
  CheckAddressExists     : AddressForm -> PlaceOrderDSL (Either CheckedAddressValidationError CheckedAddress)

  GetProductPrice  : ProductCode -> PlaceOrderDSL Price
  PlacePricedOrder : PricedOrder -> PlaceOrderDSL ()

  CreateOrderAcknowledgementLetter : PricedOrder          -> PlaceOrderDSL HtmlString
  SendOrderAcknowledgement         : OrderAcknowledgement -> PlaceOrderDSL AckSent
```

And model it with many backends...

```idris
record Model (m : Type -> Type) where
  constructor MkModel
  throwError
    : {a : Type} -> PlaceOrderError -> m a
  catchError
    : {a : Type} -> m a -> m (Either PlaceOrderError a)
  newOrderId
    : m OrderId
  newOrderLineId
    : m OrderLineId
  checkProductCodeExists
    : ProductCode -> m Bool
  checkAddressExists
    : AddressForm -> m (Either CheckedAddressValidationError CheckedAddress)
  getProductPrice
    : ProductCode -> m Price
  placePricedOrder
    : PricedOrder -> m ()
  createOrderAcknowledgementLetter
    : PricedOrder -> m HtmlString
  sendOrderAcknowledgement
    : OrderAcknowledgement -> m AckSent
```

```idris
interpret : Monad m => Model m -> PlaceOrderDSL a -> m a
interpret model (Pure x)                             = pure x
interpret model (Bind m k)                           = interpret model m >>= (interpret model . k)
interpret model (ThrowError _ x)                     = model.throwError x
interpret model (CatchError x)                       = model.catchError (interpret model x)
interpret model NewOrderId                           = model.newOrderId
interpret model NewOrderLineId                       = model.newOrderLineId
interpret model (CheckProductCodeExists x)           = model.checkProductCodeExists x
interpret model (CheckAddressExists x)               = model.checkAddressExists x
interpret model (GetProductPrice x)                  = model.getProductPrice x
interpret model (PlacePricedOrder x)                 = model.placePricedOrder x
interpret model (CreateOrderAcknowledgementLetter x) = model.createOrderAcknowledgementLetter x
interpret model (SendOrderAcknowledgement x)         = model.sendOrderAcknowledgement x
```

NOTE: Even datatypes could be abstracted, but here that was not necesary.

### Components

Type-safe non-leaky abstraction of components; everything is closed with dependent records.

```idris
record EmailComp where
  constructor MkEmailComp
  emailError  : Type
  showError   : emailError -> String
  serviceInfo : ServiceInfo
  send        : EmailAddress -> HtmlString -> Promise (Maybe emailError)
```

```idris
record OrderDBComp where
  constructor MkOrderDBComp
  dbConnection        : Type 
  dbError             : Type
  showDBError         : dbError -> String
  initConnection      : Promise (Either dbError dbConnection)
  closeConnection     : dbConnection -> Promise (Maybe dbError)
  beginTransaction    : dbConnection -> Promise (Maybe dbError)
  commitTransaction   : dbConnection -> Promise (Maybe dbError)
  rollbackTransaction : dbConnection -> Promise (Maybe dbError)
  saveOrder           : dbConnection -> PricedOrderDTO -> Promise (Maybe dbError)
```

### Dependency injection with implicit parameters

```idris
record Dependencies where
  constructor MkDependencies
  md5Provider       : MD5
  orderDBComp       : OrderDBComp
  orderDBConn       : orderDBComp.dbConnection
  productDBComp     : ProductDBComp
  productDBConn     : productDBComp.dbConnection
  emailComp         : EmailComp
  checkAddressComp  : CheckAddressComp
```

```idris
mkRunBackend
  :  (orderDBComp       : OrderDBComp)
  => (productDBComp     : ProductDBComp)
  => (emailComp         : EmailComp)
  => (checkAddressComp  : CheckAddressComp)
  => HasIO io
  => io RunBackend
mkRunBackend = ...
```

```idris
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
```

### Type safe SQL commands

```idris
record Table where
  constructor MkTable
  name         : TableName
  fields       : List Field
  constraints  : List Constraint
  0 validTable : ValidTable fields constraints
```

```idris
-- Leaves out autoincrement fields
InsertValues : List Field -> List Type

data Command : Type where
  CreateTable
    :  (table : Table)
    -> Command
  Insert
    :  (table : Table)
    -> (values : HList (InsertValues table.fields))
    -> Command
```

```idris
productTable = MkTable
  "product"
  [ field "id"          SQL_Integer [PrimaryKey, AutoIncrement]
  , field "code"        SQL_Text    [NotNull]
  , field "description" SQL_Text    [NotNull]
  , field "price"       SQL_Double  [NotNull]
  ]
  [ Unique "code_unique" ["code"] ]
  YesOfCourseValid
```

```idris
Insert productTable
  [ FieldOf "code"        (SQLText productCode)
  , FieldOf "description" (SQLText description)
  , FieldOf "price"       (SQLDouble price)
  ]
```

### Type safe SQL queries

```idris
data Query : Type where
  Select
    :  (  fields    : List FieldName)
    -> (  table     : Table)
    -> (1 okFields  : SelectedFieldsDefinedInTable fields table.fields)
    => (  filters   : List (FieldName, String, String))
    -> (0 okFilters : FilteredFieldsDefinedInTable filters table.fields)
```

```idris
Select ["code", "description", "price"] productTable [("code", "=", "'\{productCode}'")]
```

### JSON Schema

```idris
data Presence = Optional | Required

data Schema
  = Null
  | Boolean
  | Number
  | Str
  | Array  (List Schema)
  | Object (String, Presence, Schema)
  | Either Schema Schema
```

```idris
data Field : (String, Presence, Schema) -> Type where
  RequiredField : {f : String} ->         JSON s -> Field (f,Required,s)
  OptionalField : {f : String} -> Maybe (JSON s) -> Field (f,Optional,s)
```

```idris
data JSON : Schema -> Type where
  JNull    :                                        JSON Null
  JBoolean : Bool                                -> JSON Boolean
  JNumber  : Double                              -> JSON Number
  JString  : String                              -> JSON Str
  JArray   : {xs : List Schema}  -> All JSON xs  -> JSON (Array xs)
  JObject  : {xs : FieldList}    -> All Field xs -> JSON (Object xs)
  JLeft    : {l  : Schema}       -> JSON l       -> JSON (Either l r)
  JRight   : {r  : Schema}       -> JSON r       -> JSON (Either l r)
```

### Postfix notation for OO feel

```idris
(.setHeader)     : Response -> String -> String -> IO ()
(.setStatusCode) : Response -> Int              -> IO ()
(.end)           : Response -> String           -> IO ()

rsp.setHeader "Content-Type" "application/json"
rsp.setHeader "Access-Control-Allow-Origin" "*"
rsp.setStatusCode 400
rsp.end "{\"message\":\"Couldn't parse incoming JSON.\"}"
```

### Dynamically typed programming

Idris as strong that it can simulate JavaScript dynamic types, with matching on Types.

```idris
renderFieldValue : {t : Type} -> t -> String
renderFieldValue {t=FieldOfTy n s} (FieldOf _ x) = renderFieldValue x
renderFieldValue {t=Maybe (SQLValue s)} (Just x) = renderFieldValue x
renderFieldValue {t=Maybe (SQLValue s)} Nothing  = "NULL"
renderFieldValue {t=SQLValue s} x = renderSQLValue x
renderFieldValue _ = "(╯°□°)╯︵ ┻━┻" -- This shouldn't happen

renderInsertNames : List Field -> List String

renderCommand (Insert table values)
  = "INSERT INTO \{table.name} (\{withCommas (renderInsertNames table.fields)}) VALUES (\{withCommas (renderInsertValues values)})"
```

Forgot this line:
```idris
renderFieldValue {t=FieldOfTy n s} (FieldOf _ x) = renderFieldValue x
```

Called this:
```idris
Insert productTable
  [ FieldOf "code"        (SQLText productCode)
  , FieldOf "description" (SQLText description)
  , FieldOf "price"       (SQLDouble price)
  ]
```

and got this in the logs:
```
INSERT INTO productTable (...) VALUES ((╯°□°)╯︵ ┻━┻, (╯°□°)╯︵ ┻━┻, (╯°□°)╯︵ ┻━┻)
```

## QED :)
