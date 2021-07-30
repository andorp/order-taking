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
  State    : Type
  Command  : State -> State -> Type
  Branch   : State -> State -> State -> Type
  start    : State
  end      : State
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
  -> (mr : Morphism monad w.State w.Command w.Branch)
  -> (mr.StateType w.start) -> monad (mr.StateType w.end)
transformWorkflow w mr x = morph mr w.Workflow x

public export
data Embedding : (from : Type -> Type) -> (err : Type) -> (to : Type -> Type) -> Type where
  MkEmbedding : ((a : Type) -> from a -> to (Either err a)) -> Embedding from err to

export
runEmbedding : {a : Type} -> Embedding f e t -> f a -> t (Either e a)
runEmbedding (MkEmbedding f) y = f a y

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
  Command
    : context.Command -> Type
  
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

||| Execute a command with the given bounded context.
|||
||| Its responsibility is to select the workflow corresponding the given command,
||| and execute the selected workflow in its monad, which is embeddeable to the
||| main monad of the bounded context handler. The result will be a bounded context
||| error or a bounded context event.
export
execute
  :  (Monad m)
  => (bc : BoundedContextImplementation m) -> bc.ContextCommand -> m (Either bc.ContextError bc.ContextEvent)
execute bc contextCommand = do
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
