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

public export
record Interpreter
        state
        (0 m : Type -> Type)
        (0 cmd : state -> state -> Type)
        (0 chk : state -> state -> state -> Type)
  where
    constructor MkRunner
    stateType
      : state -> Type
    runCommand
      : {0 s,e : state} -> cmd s e -> (stateType s) -> m (stateType e)
    runCheck
      :  {0 s,b1,b2 : state}
      -> chk s b1 b2
      -> (stateType s) -> m (Either (stateType b1) (stateType b2))

run
  :  Functor m
  => Applicative m
  => Monad m
  => (r : Interpreter state m cmd chk)
  -> Workflow cmd chk start end
  -> (stateType r start)
  -> m (stateType r end)
run r (Do cmd) i = runCommand r cmd i
run r (m1 >> m2) i = do
  x <- run r m1 i
  run r m2 x
run r (Branch h b1 b2) i = do
  x <- runCheck r h i
  case x of
    Left y  => run r b1 y
    Right y => run r b2 y
