module Rango.BoundedContext.Workflow

public export
data Workflow
      :  (0 cmd : state -> state -> Type)
      -> (0 chk : state -> state -> state -> Type)
      -> state
      -> state
      -> Type
  where
    Do
      :  {0 cmd : state -> state -> Type}
      -> cmd pre post
      -> Workflow cmd chk pre post
    Branch
      :  {0 chk : state -> state -> state -> Type}
      -> {1 branch1, branch2 : state}
      -> chk pre branch1 branch2
      -> Workflow cmd chk branch1 post
      -> Workflow cmd chk branch2 post
      -> Workflow cmd chk pre post
    (>>)
      :  {1 mid : state}
      -> Workflow cmd chk pre mid
      -> Workflow cmd chk mid post
      -> Workflow cmd chk pre post

||| How to turn the pieces in the Workflow description into regular
||| Idris Types. This helps us to connect the high level description
||| with the actual implementation.
public export
record Morphism
        state
        (0 m : Type -> Type) -- TODO: Move this to the first parameter
        (0 cmd : state -> state -> Type)
        (0 chk : state -> state -> state -> Type)
  where
    constructor MkMorphism
    StateType
      : state -> Type
    command
      : {0 s,e : state} -> cmd s e -> (StateType s) -> m (StateType e)
    check
      :  {0 s,b1,b2 : state}
      -> chk s b1 b2
      -> (StateType s) -> m (Either (StateType b1) (StateType b2))

||| How to turn a Workflow description into a state transition system
||| which can is represented as a Kliesli arrow of a Monad; a -> m b
||| This helps us to connect the high level description
||| with the actual implementation.
export
morph
  :  Functor m
  => Applicative m
  => Monad m
  => (r : Morphism state m cmd chk)
  -> Workflow cmd chk start end
  -> (StateType r start) -> m (StateType r end)
morph r (Do cmd) i = command r cmd i
morph r (m1 >> m2) i = do
  x <- morph r m1 i
  morph r m2 x
morph r (Branch h b1 b2) i = do
  x <- check r h i
  case x of
    Left y  => morph r b1 y
    Right y => morph r b2 y

