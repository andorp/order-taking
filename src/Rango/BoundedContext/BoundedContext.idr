module Rango.BoundedContext.BoundedContext

import public Rango.BoundedContext.Workflow

||| Summary of a Bounded Context
|||
||| A collection of descriptive types, and their connection via
||| simple functions. The command determines its workflow
||| and its result.
||| 
||| Later the values of command, workflow, and event will
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
  ||| Description of the command in the Bounded Context
  command     : Type
  ||| Description of the possible workflows in the Bounded Context
  workflow    : Type
  ||| Descriptio of the possible events in the Bounded Context
  event       : Type
  ||| Every command means one workflow
  workflowOf  : command -> workflow
  ||| Every command means one event
  eventOf     : command -> event

||| Environment of a workflow.
|||
||| Existential type for the Workflow; all the
||| necessary information is available that is
||| required for the workflow. This is a connection
||| between the workflow abstraction and its use-cases.
public export
record WorkflowEnv where
  constructor MkWorkflowEnv
  state    : Type
  command  : state -> state -> Type
  branch   : state -> state -> state -> Type
  start    : state
  end      : state
  workflow : Workflow command branch start end

||| Smart constructor for the Workflow.
|||
||| Capture all the parameters of a workflow
||| in a Workflow environment.
public export
mkWorkflowEnv
  :  {st : Type}
  -> {cm : st -> st -> Type}
  -> {br : st -> st -> st -> Type}
  -> {s, e : st}
  -> (w : Workflow cm br s e) -> WorkflowEnv
mkWorkflowEnv {st, cm, br, s, e} w = MkWorkflowEnv st cm br s e w

||| Transform a WorkflowEnv with a workflow morphism.
transformWorkflow
  :  Monad m
  => (w : WorkflowEnv)
  -> (mr : Morphism w.state m w.command w.branch)
  -> (mr.StateType w.start) -> m (mr.StateType w.end)
transformWorkflow w mr x = morph mr w.workflow x

public export
data Embedding : (from : Type -> Type) -> (err : Type) -> (to : Type -> Type) -> Type where
  MkEmbedding : ((a : Type) -> from a -> to (Either err a)) -> Embedding from err to

export
runEmbedding : {a : Type} -> Embedding f e t -> f a -> t (Either e a)
runEmbedding (MkEmbedding f) y = f a y

||| The implementation of the Bounded Context
public export
record BoundedContextImplementation (monad : Type -> Type) where
  constructor MkBoundedContextImplementation

  ||| The high level description of the bounded context.
  context
    : BoundedContext

  ||| Associate workflows with the values of the workflow description.
  workflow
    : context.workflow -> WorkflowEnv
  
  ||| Datatype which represents an incomming command of the Bounded Context
  contextCommand
    : Type
  
  ||| Association between the different kind of commands and the information
  ||| needed by the workflow.
  command
    : context.command -> Type
  
  ||| Datatype which represents an outgoing event of the Bounded Context
  ||| It should somehow summarize the information from the events created
  ||| by different workflows
  contextEvent
    : Type
  
  ||| The information needed to represent the Context Event in this
  ||| implementation. Usually a datatype per event of the context.
  eventData
    : context.event -> Type
  
  ||| Datatype which represents an outgoing errors of the Bounded Context
  ||| It should somehow summarize the information from the events created
  ||| by different workflows
  contextError
    : Type

  ||| The information needed to represent the Context Error in this
  ||| implementation. Usually a datatype per error of the context.
  errorData
    : context.workflow -> Type

  workflowMonad
    : context.workflow -> (Type -> Type)
  workflowMonadInstance
    : (w : context.workflow) -> Monad (workflowMonad w)
  workflowMorphism
    : (cmd : context.command) ->
      let w = context.workflowOf cmd
          d = workflow w
      in Morphism (state d) (workflowMonad w) (WorkflowEnv.command d) (branch d)
  createWorkflowEmbedding
    : (cmd : context.command) ->
      let w = context.workflowOf cmd
      in monad (Embedding (workflowMonad w) (errorData w) monad)
  createWorkflowEvent
    : (cmd : context.command) ->
      let m = workflowMorphism cmd
      in m.StateType (WorkflowEnv.end (workflow (context.workflowOf cmd))) -> monad (eventData (context.eventOf cmd))
  createFinalEvent
    : (cmd : context.command) -> eventData (context.eventOf cmd) -> monad contextEvent
  createCommand
    : contextCommand -> monad (cmd : context.command ** command cmd)
  createStartState
    : (cmd : context.command) -> command cmd -> 
      let m = workflowMorphism cmd
      in monad (m.StateType (WorkflowEnv.start (workflow (context.workflowOf cmd))))
  createFinalError
    : (w : context.workflow) -> (errorData w) -> monad contextError

export
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

-- https://dot-to-ascii.ggerganov.com/ for ASCII art
