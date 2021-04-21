module Rango.BoundedContext.Description

record Workflow w d e f where
  constructor MkWorkflow
  workflow     : w
  triggeredBy  : e
  primaryInput : d
  dependencies : List d
  outputEvents : List e
  sideEffects  : List f
