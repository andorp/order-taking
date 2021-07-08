module Rango.BoundedContext.BoundedContext

import public Rango.BoundedContext.Workflow


public export
record WorkflowContext where
  constructor MkWorkflowContext
  state    : Type
  command  : state -> state -> Type
  branch   : state -> state -> state -> Type
  start    : state
  end      : state
  workflow : Workflow command branch start end

public export
mkWorkflowContext
  :  {st : Type}
  -> {cm : st -> st -> Type}
  -> {br : st -> st -> st -> Type}
  -> {s, e : st}
  -> (w : Workflow cm br s e) -> WorkflowContext
mkWorkflowContext {st, cm, br, s, e} w = MkWorkflowContext st cm br s e w

public export
transformWorkflow
  :  Monad m
  => (w : WorkflowContext)
  -> (mr : Morphism w.state m w.command w.branch)
  -> (mr.StateType w.start) -> m (mr.StateType w.end)
transformWorkflow w mr x = morph mr w.workflow x

public export
record BoundedContextDefinition where
  constructor MkBoundedContextDefinition
  command     : Type
  workflow    : Type
  event       : Type
  workflowOf  : command -> workflow
  eventOf     : command -> event

public export
data Embedding : (from : Type -> Type) -> (err : Type) -> (to : Type -> Type) -> Type where
  MkEmbedding : ((a : Type) -> from a -> to (Either err a)) -> Embedding from err to

export
runEmbedding : {a : Type} -> Embedding f e t -> f a -> t (Either e a)
runEmbedding (MkEmbedding f) y = f a y

public export
record BoundedContext (monad : Type -> Type) where
  constructor MkBoundedContext
  context
    : BoundedContextDefinition
  workflow
    : context.workflow -> WorkflowContext
  commandData
    : Type
  command
    : context.command -> Type
  event
    : Type
  eventData
    : context.event -> Type
  error
    : Type
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
      in Morphism (state d) (workflowMonad w) (WorkflowContext.command d) (branch d)
  workflowEmbedding
    : (cmd : context.command) ->
      let w = context.workflowOf cmd
      in monad (Embedding (workflowMonad w) (errorData w) monad)
  createEventData
    : (cmd : context.command) ->
      let m = workflowMorphism cmd
      in m.StateType (WorkflowContext.end (workflow (context.workflowOf cmd))) -> monad (eventData (context.eventOf cmd))
  createEvent
    : (cmd : context.command) -> eventData (context.eventOf cmd) -> monad event
  createCommand
    : commandData -> monad (cmd : context.command ** command cmd)
  createWorkflowInput
    : (cmd : context.command) -> command cmd -> 
      let m = workflowMorphism cmd
      in monad (m.StateType (WorkflowContext.start (workflow (context.workflowOf cmd))))
  createError
    : (w : context.workflow) -> (errorData w) -> monad error

export
boundedContext : (Monad m) => (bc : BoundedContext m) -> bc.commandData -> m (Either bc.error bc.event)
boundedContext bc commandData = do
  (cmd ** cmdData) <- bc.createCommand commandData
  workflowRunner <- bc.workflowEmbedding cmd
  let workflowMonadInstance = bc.workflowMonadInstance (bc.context.workflowOf cmd) -- For transformWorkflow
  input  <- bc.createWorkflowInput cmd cmdData
  result <- runEmbedding workflowRunner (transformWorkflow (bc.workflow (bc.context.workflowOf cmd)) (bc.workflowMorphism cmd) input)
  case result of
    Left err => map Left (bc.createError (bc.context.workflowOf cmd) err)
    Right wfVal => do
      evVal <- bc.createEventData cmd wfVal
      map Right (bc.createEvent cmd evVal)
