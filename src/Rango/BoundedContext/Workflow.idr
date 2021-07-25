module Rango.BoundedContext.Workflow

||| High level description of a Workflow of a bounded context.
|||
||| State transition system which describes the state transition
||| system of a workflow, similar as flowcharts

||| The workflow starts in a state, and ends in another one.
||| The workflow describes this state transition using the
||| 'cmd' to annotate a transition from one state to another,
||| 'chk' to annotate a branching condition.
public export
data Workflow
         -- cmd - Annotate transitions from one state to another
      :  (0 cmd : state -> state -> Type)
         -- chk - Annotate checks from one state to a valid and invalid state
      -> (0 chk : state -> state -> state -> Type)
         -- initial state
      -> state
         -- end state
      -> state
      -> Type
  where
    -- Wrap a command in a Do step
    Do
      :  {0 cmd : state -> state -> Type}
      -> cmd pre post
      -> Workflow cmd chk pre post
    -- Wrap a condition is a Branch step
    Branch
      :  {0 chk : state -> state -> state -> Type}
      -> {1 branch1, branch2 : state}
      -> chk pre branch1 branch2
      -> Workflow cmd chk branch1 post
      -> Workflow cmd chk branch2 post
      -> Workflow cmd chk pre post
    -- Use Idris' do notation to sequence steps
    (>>)
      :  {1 mid : state}
      -> Workflow cmd chk pre mid
      -> Workflow cmd chk mid post
      -> Workflow cmd chk pre post

||| How to turn the pieces in the Workflow description into a computation
||| related Idris Types. This helps us to connect the high level description
||| with the actual implementation.
public export
record Morphism
        (0 monad : Type -> Type)
        state
        (0 cmd : state -> state -> Type)
        (0 chk : state -> state -> state -> Type)
  where
    constructor MkMorphism
    -- How to represent information with the associated state.
    StateType
      : state -> Type
    -- How to represent the given command as a monadic state transition,
    -- in form of 'a -> m b'
    command
      : {0 s,e : state} -> cmd s e -> (StateType s) -> monad (StateType e)
    -- How to represent branching as a monadic state transition,
    -- in form of 'a -> m (Either b c)'
    check
      :  {0 s,b1,b2 : state}
      -> chk s b1 b2
      -> (StateType s) -> monad (Either (StateType b1) (StateType b2))

||| How to turn a Workflow description into a state transition system
||| which can is represented as a Kliesli arrow of a Monad; a -> m b
||| This helps us to connect the high level description
||| with the actual implementation.
export
morph
  :  Functor monad => Applicative monad => Monad monad
  => (r : Morphism monad state cmd chk)
  -> Workflow cmd chk start end
  -> (StateType r start) -> monad (StateType r end)
morph r (Do cmd) i = command r cmd i
morph r (m1 >> m2) i = do
  x <- morph r m1 i
  morph r m2 x
morph r (Branch h b1 b2) i = do
  x <- check r h i
  case x of
    Left y  => morph r b1 y
    Right y => morph r b2 y
