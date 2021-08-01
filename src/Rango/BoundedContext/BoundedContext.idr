module Rango.BoundedContext.BoundedContext

import public Rango.BoundedContext.Workflow

||| Summary of a Bounded Context
|||
||| A collection of descriptive types, and their connection via
||| simple functions. The Command determines its Workflow
||| and its result.
||| 
||| Later the values of Command, Workflow, and Event will
||| be used for determining the underlying implementational
||| 'Type's as we can use the power of dependent types
||| to calculate Type from values.
--
-- Bounded Context overview in general
--
--                     ┌───────────────┐
--                     │  CommandDTO   │
--                     └───────────────┘
--                       │
--                       │
--                       ▼
-- ┌−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┐
-- ╎                    Bounded Context                    ╎
-- ╎                                                       ╎
-- ╎                   ┌───────────────┐                   ╎
-- ╎   ┌────────────── │    Command    │ ──────┐           ╎
-- ╎   │               └───────────────┘       │           ╎
-- ╎   │                 │                     │           ╎
-- ╎   │                 │                     │           ╎
-- ╎   ▼                 ▼                     ▼           ╎
-- ╎ ┌───────────┐     ┌───────────────┐     ┌───────────┐ ╎
-- ╎ │ Workflow1 │     │    Workflow2  │     │ WorkflowN │ ╎
-- ╎ └───────────┘     └───────────────┘     └───────────┘ ╎
-- ╎   │                 │                     │           ╎
-- ╎   │                 │                     │           ╎
-- ╎   │                 ▼                     │           ╎
-- ╎   │               ┌───────────────┐       │           ╎
-- ╎   └─────────────▶ │     Event     │ ◀─────┘           ╎
-- ╎                   └───────────────┘                   ╎
-- ╎                                                       ╎
-- └−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−┘
--                       │
--                       │
--                       ▼
--                     ┌───────────────┐
--                     │   EventDTO    │
--                     └───────────────┘
--
public export
record BoundedContext where
  constructor MkBoundedContext
  ||| Description of the Command in the Bounded Context
  Command     : Type
  ||| Description of the possible workflows in the Bounded Context
  Workflow    : Type
  ||| Descriptio of the possible events in the Bounded Context
  Event       : Type
  ||| Every Command means one Workflow
  workflowOf  : Command -> Workflow
  ||| Every Command means one Event
  eventOf     : Command -> Event

||| Environment of a Workflow.
|||
||| Existential type for the Workflow; all the
||| necessary information is available that is
||| required for the Workflow. This is a connection
||| between the Workflow abstraction and its use-cases.
public export
record WorkflowEnv where
  constructor MkWorkflowEnv
  state    : Type
  Command  : state -> state -> Type
  Branch   : state -> state -> state -> Type
  start    : state
  end      : state
  Workflow : Workflow Command Branch start end

||| Smart constructor for the Workflow.
|||
||| Capture all the parameters of a Workflow
||| in a Workflow environment.
public export
mkWorkflowEnv
  :  {st : Type}
  -> {cm : st -> st -> Type}
  -> {br : st -> st -> st -> Type}
  -> {s, e : st}
  -> (w : Workflow cm br s e) -> WorkflowEnv
mkWorkflowEnv {st, cm, br, s, e} w = MkWorkflowEnv st cm br s e w

||| Transform a WorkflowEnv with a Workflow morphism.
transformWorkflow
  :  Monad monad
  => (w : WorkflowEnv)
  -> (mr : Morphism monad w.state w.Command w.Branch)
  -> (mr.StateType w.start) -> monad (mr.StateType w.end)
transformWorkflow w mr x = morph mr w.Workflow x

||| Embedding of a workflow monad into the monad of bounded context implementation.
|||
||| The embedding encapsulates a function which transforms one monad to another one.
||| This is a helper abstraction for the Workflow embedding, where workflow monads,
||| needs to be understandable in the monad of the bounded context.
public export
data Embedding : (from : Type -> Type) -> (err : Type) -> (to : Type -> Type) -> Type where
  -- An extra 'a' type parameter is needed here to support the Rank polimorphism
  -- better. This type parameter will be injected with the 'runEmbedding' function.
  -- The 'Either err' is part of the return type signature to encode the fact, that
  -- some kind of error will be expected from the run of the workflow.
  MkEmbedding : ((a : Type) -> from a -> to (Either err a)) -> Embedding from err to

||| Unwraps the embedding
|||
||| unwrapEmbedding use the implicit 'a' type parameters from the context, which
||| is determined by the clients of the 'unwrapEmbedding'.
||| Its intended use is to create the 'f a -> t (Either e a)' function.
unwrapEmbedding : {a : Type} -> Embedding f e t -> (f a -> t (Either e a))
unwrapEmbedding (MkEmbedding f) y = f a y

||| Well-typed formulazation of a Bounded Context.
public export
record BoundedContextImplementation (monad : Type -> Type) where
  constructor MkBoundedContextImplementation

  ||| The high level description of the bounded context.
  context
    : BoundedContext

  ||| Associate workflows with the values of the Workflow description.
  Workflow
    : context.Workflow -> WorkflowEnv
  
  ||| Datatype which represents an incomming command of the Bounded Context
  ContextCommand
    : Type
  
  ||| Association between the different kind of commands and the information
  ||| needed by the Workflow.
  WorkflowEntry
    : context.Workflow -> Type
  
  ||| Datatype which represents an outgoing Event of the Bounded Context
  ||| It should somehow summarize the information from the events created
  ||| by different workflows
  ContextEvent
    : Type
  
  ||| The information needed to represent the Context Event in this
  ||| implementation. Usually a datatype per Event of the context.
  EventData
    : context.Event -> Type
  
  ||| Datatype which represents an outgoing errors of the Bounded Context
  ||| It should somehow summarize the information from the events created
  ||| by different workflows
  ContextError
    : Type

  ||| The information needed to represent the Context Error in this
  ||| implementation. Usually a datatype per error of the context.
  ErrorData
    : context.Workflow -> Type

  ||| For every workflow we can define its own monad, this helps us
  ||| separate of concerns a granulated way. For example
  ||| different workflows, needs to connect to different services
  ||| or different databases. It is enough if the workflow monad
  ||| just encapsulates its very own repsonsibility.
  WorkflowMonad
    : context.Workflow -> (Type -> Type)
  
  ||| Technicality, defining the Monad type for the workflow is not
  ||| enough, we should provide its Monad interface instance. This
  ||| is needed for the embedding part, when we select the Monad
  ||| for the workflow Idris must be aware of the Monad instance
  ||| in context. The easiest way to do this is the grab the instance
  ||| in the record too.
  WorkflowMonadInstance
    : (w : context.Workflow) -> Monad (WorkflowMonad w)
  
  ||| Every command of the bounded context determines its workflow, it is
  ||| enough to associate the workflow with its morphism that implements
  ||| the worklflow in its monad.
  workflowMorphism
    : (cmd : context.Command) ->
      let w = context.workflowOf cmd
          d = Workflow w
      in Morphism (WorkflowMonad w) (state d) (WorkflowEnv.Command d) (Branch d)
  
  ||| Create an embedding from the command. This could instanciate
  ||| new embedding for the workflow every time when a new command
  ||| arrives.
  createWorkflowEmbedding
    : (cmd : context.Command) ->
      let w = context.workflowOf cmd
      in monad (Embedding (WorkflowMonad w) (ErrorData w) monad)
  
  ||| Transforms the result of a workflow into the event of the workflow.
  ||| Computation may happen during the transformation.
  transformWorkflowResult
    : (cmd : context.Command) ->
      let morphism = workflowMorphism cmd
      in morphism.StateType (WorkflowEnv.end (Workflow (context.workflowOf cmd))) -> monad (EventData (context.eventOf cmd))
  
  ||| Transforms the event information that comes from the execution of the workflow
  ||| into the Event of the bounded context.
  ||| Computation may happen during the transformation.
  transformEvent
    : (cmd : context.Command) -> EventData (context.eventOf cmd) -> monad ContextEvent
  
  ||| Creates a command from the command information.
  |||
  ||| The entry point of a bounded context is a command. We get the command
  ||| from the external world, that is the public API of the bounded context,
  ||| but that command needs to parsed, understood in terms of the internal
  ||| commands, and information which is needed by the workflow.
  createCommand
    : ContextCommand -> monad (cmd : context.Command ** WorkflowEntry (context.workflowOf cmd))
  
  ||| Creates a start state of the workflow from command, which
  ||| was extracted from the command data of the bounded context.
  ||| Creation of the start state related information of the workflow
  ||| may happen as part a computation.
  createStartState
    : (cmd : context.Command) ->
      let m = workflowMorphism cmd
      in WorkflowEntry (context.workflowOf cmd) -> monad (m.StateType (WorkflowEnv.start (Workflow (context.workflowOf cmd))))
  
  ||| Transform the error information that comes from the execution of the workflow
  ||| into the Error of the boundedContext.
  ||| Computation may happend during the transformation of this information.
  transformError
    : (w : context.Workflow) -> (ErrorData w) -> monad ContextError

||| Execute a command with the given bounded context.
|||
||| Its responsibility is to select the workflow corresponding the given command,
||| and execute the selected workflow in its monad, which is embeddeable to the
||| main monad of the bounded context handler. The result will be a bounded context
||| error or a bounded context event.
-- 
--   ┌───────────────────────────────────────────────────────────────┐
--   │                                                               │
--   │                                    ┌───────────────────────┐  │
--   │                                    │         Start         │  │
--   │                                    └───────────────────────┘  │
--   │                                      │                        │
--   │ cmd                                  │ context command        │
--   ▼                                      ▼                        │
-- ┌──────────────────┐  workflow entry   ┌────────────────────────────────────────────────────────────────────┐  cmd               ┌─────────────────────────┐
-- │ createStartState │ ◀──────────────── │                                                                    │ ─────────────────▶ │ createWorkflowEmbedding │
-- └──────────────────┘                   │                                                                    │                    └─────────────────────────┘
--   │                                    │                                                                    │                      │
--   │                                    │                           createCommand                            │                      │
--   │                                    │                                                                    │                      │
--   │                                    │                                                                    │                      │
--   │                   ┌─────────────── │                                                                    │ ─┐                   │
--   │                   │                └────────────────────────────────────────────────────────────────────┘  │                   │
--   │                   │                  │                                          │                          │                   │
--   │                   │                  │ cmd                                      │ cmd                      │                   │
--   │                   │                  ▼                                          ▼                          │                   │
--   │                   │                ┌───────────────────────┐                  ┌─────────────────────────┐  │                   │
--   │                   │                │ bc.context.workflowOf │                  │   bc.workflowMorphism   │  │                   │
--   │                   │                └───────────────────────┘                  └─────────────────────────┘  │                   │
--   │ input             │                  │                                          │                          │                   │
--   │                   │                  │ workflow                                 │                          │                   │
--   │                   │                  ▼                                          │                          │                   │
--   │                   │                ┌───────────────────────┐  morphism          │                          │                   │
--   │                   │                │   transformWorkflow   │ ◀──────────────────┘                          │                   │
--   │                   │                └───────────────────────┘                                               │                   │
--   │                   │                  │                                                                     │                   │
--   │                   │                  │ embedded workflow                                                   └───────────────────┼──────────────────────────┐
--   │                   │                  ▼                                                                                         │                          │
--   │                   │                ┌────────────────────────────────────────────────────────────────────┐  workflow runner     │                          │
--   └───────────────────┼──────────────▶ │             unwrapEmbedding: Execute embedded workflow             │ ◀────────────────────┘                          │
--                       │                └────────────────────────────────────────────────────────────────────┘                                                 │
--                       │                  │                                          │                                                                         │
--                       │ cmd              │ error                                    │ event data                                                              │
--                       │                  ▼                                          ▼                                                                         │
--                       │                ┌───────────────────────┐                  ┌─────────────────────────┐  cmd                                            │
--                       └──────────────▶ │    transformError     │                  │ transformWorkflowResult │ ◀───────────────────────────────────────────────┘
--                                        └───────────────────────┘                  └─────────────────────────┘
--                                          │                                          │
--                                          │ context error                            │
--                                          ▼                                          │
--                                        ┌───────────────────────┐  context event     │
--                                        │     contextResult     │ ◀──────────────────┘
--                                        └───────────────────────┘
--                                          │
--                                          │
--                                          ▼
--                                        ┌───────────────────────┐
--                                        │          End          │
--                                        └───────────────────────┘
--
export
execute
  :  (Monad m)
  => (bc : BoundedContextImplementation m) -> bc.ContextCommand -> m (Either bc.ContextError bc.ContextEvent)
execute bc contextCommand = do
  (cmd ** workflowEntry) <- bc.createCommand contextCommand
  workflowRunner <- bc.createWorkflowEmbedding cmd
  let workflowMonadInstance = bc.WorkflowMonadInstance (bc.context.workflowOf cmd) -- For transformWorkflow
  input  <- bc.createStartState cmd workflowEntry
  let embeddedWorkflow = transformWorkflow (bc.Workflow (bc.context.workflowOf cmd)) (bc.workflowMorphism cmd)
  result <- unwrapEmbedding workflowRunner (embeddedWorkflow input)
  case result of
    Left err => map Left (bc.transformError (bc.context.workflowOf cmd) err)
    Right eventData => do
      workflowEvent <- bc.transformWorkflowResult cmd eventData
      map Right (bc.transformEvent cmd workflowEvent)

-- NOTE: Graph definition

{-
digraph {
	Start -> createCommand [ label = "context command" ];
    createCommand -> createWorkflowEmbedding [ label = "cmd" ];
    createCommand -> createStartState [ label = "cmd" ];
    createCommand -> createStartState [ label = "workflow entry" ];
    createCommand -> bc_context_workflowOf [ label = "cmd" ];
    createCommand -> bc_workflowMorphism [ label = "cmd" ];
    bc_context_workflowOf -> transformWorkflow [ label = "workflow" ];
    bc_workflowMorphism -> transformWorkflow [ label = "morphism" ];
    createWorkflowEmbedding -> unwrapEmbedding [ label = "workflow runner" ];
    transformWorkflow -> unwrapEmbedding [ label = "embedded workflow" ];
    createStartState -> unwrapEmbedding [ label = "input" ];
    createCommand   -> transformError [ label = "cmd" ];
    unwrapEmbedding -> transformError [ label = "error" ];
    createCommand   -> transformWorkflowResult [ label = "cmd" ];
    unwrapEmbedding -> transformWorkflowResult [ label = "event data" ];
    transformError -> contextResult [ label = "context error" ];
    transformWorkflowResult -> contextResult [ label = "context event" ];
    contextResult -> End;
    
    unwrapEmbedding [ label = "unwrapEmbedding: Execute embedded workflow" ];
    bc_context_workflowOf [ label = "bc.context.workflowOf" ];
    bc_workflowMorphism [ label = "bc.workflowMorphism" ];   
}
-}
