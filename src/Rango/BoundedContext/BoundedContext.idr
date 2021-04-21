module Rango.BoundedContext.BoundedContext

import Rango.BoundedContext.Workflow

data Command : cmd -> state -> Type where
  MkCommand : (c : cmd) -> (s : state) -> Command cmd s

data Event : ev -> state -> Type where
  MkEvent : (s : state) -> List e -> Event ev s

data PublicWorkflow
      :  command
      -> event
      -> Type
  where
    MkPublicWorkflow
      :  Command command start
      -> Workflow cmd chk start end
      -> Event event end
      -> PublicWorkflow command event

data BoundedContext : List (command, event) -> Type where
  Nil  :  BoundedContext []
  (::) :  PublicWorkflow c e
       -> BoundedContext cs
       -> BoundedContext ((c,e) :: cs)

