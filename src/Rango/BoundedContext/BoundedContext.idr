module Rango.BoundedContext.BoundedContext

import Rango.BoundedContext.Workflow

||| Represents a command and an associated state with the command, where
||| the workflow should start.
data Command : cmd -> state -> Type where
  MkCommand : (c : cmd) -> (start : state) -> Command c start

||| Represents an end state of the workflow and an list of
||| events that the workflow should produce.
data Event : state -> event -> Type where
  MkEvent : (end : state) -> (ev : event) -> Event end ev

||| End to end description of the Workflow. Every workflow
||| starts with a command, has its high level description
||| and it ends emitting some events.
data PublicWorkflow
      :  command
      -> start
      -> end
      -> event
      -> Type
  where
    MkPublicWorkflow
      :  Command command start
      -> Workflow cmd chk start end
      -> Event end event
      -> PublicWorkflow command start end event

||| A bounded context is a collection of workflows.
data BoundedContext : List (command, start, end, event) -> Type where
  Nil  :  BoundedContext []
  (::) :  PublicWorkflow cmd str end ev
       -> BoundedContext cs
       -> BoundedContext ((cmd,str,end,ev) :: cs)

{-

How to turn a BoundedContext description to a NodeJS handler?
We can turn a Workflow into a NodeJS handler, via the two step process of turning a workflow into
a Free monad expression, than execute that free monad expression in the Promise monad.

Every workflow should live in its Free Monad world as simple programs/DSL,
into the Kliesli form of the Monad. (a -> m b).
The command needs to be turned into the information which needs the actual workflow DSL.

c:cmd needs to be turned into an Idris Type
start needs to be turned into an Idris Type, which matches the type defined in Morphism.

There should be an associated monad with the BoundedContext. Every workflow has its Morphism,
and its Free Monad, which is translated to the NodeJS handler monad.

At the end we need to represent the BoundedContext as a computation, for that we have to have
a unified monad, a dispatcher on commands, and a collector of events. When the workflow is executed
it creates a list of event which triggers actions, possible firing up new commands. But in our example
we stay with the NodeJS part.

The bottom approach:
- NodeJS starts
- HTTP server starts
- Backend runner instanciates
- Request comes in
- [A command is created]
- [A workflow is selected]
- The workflow is evalauted in the NodeJS monad
- Notification of events are created
- Response is generated

DTO needs to be generated from the Request and
Response needs to be generated from EventDTOs
-}

public export
record WD where
  constructor MkWD
  state    : Type
  command  : state -> state -> Type
  branch   : state -> state -> state -> Type
  start    : state
  end      : state
  workflow : Workflow command branch start end

public export
mkWD
  :  {st : Type}
  -> {cm : st -> st -> Type}
  -> {br : st -> st -> st -> Type}
  -> {s, e : st}
  -> (w : Workflow cm br s e) -> WD
mkWD {st, cm, br, s, e} w = MkWD st cm br s e w

public export
data BC : cmd -> ev -> Type where
  MkBC : (cmd -> WD) -> BC cmd ev

public export
record MD where
  constructor MkMD
  monad : Type -> Type
  state : Type
  command : state -> state -> Type
  branch : state -> state -> state -> Type
  mrp   : Morphism state monad command branch

public export
applyWD : Functor m => Applicative m => Monad m => (w : WD) -> (mr : Morphism w.state m w.command w.branch) -> (mr.StateType w.start) -> m (mr.StateType w.end)
applyWD w mr x = morph mr w.workflow x

-- public export
-- applyWD1 : (w : WD) -> (mr : MD) -> (mr.mrp.StateType w.start) -> mr.monad (mr.mrp.StateType w.end)
