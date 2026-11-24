package histos.report

import heddle.endpoint.Schema
import histos.step.{Phase, StepId}
import histos.testrun.{
  ScopedTestFailure,
  SuiteName,
  TestAggregateError,
  TestFailureNode,
  TestNodeFailure,
  TestSelectionError,
}

/** Scoped failures are leaf schemas, not repeated inline derivations in enclosing results. */
private object WireSchemaScoped extends WireSchemaSupport:
  import WireSchemaLeaves.given
  import WireSchemaContract.given
  import WireSchemaStages.given
  import WireSchemaBuild.given

  private given Schema[SuiteName]  = text("SuiteName")
  given Schema[Phase]              = Schema.derived
  given Schema[StepId]             = Schema.derived
  given Schema[TestAggregateError] = Schema.derived
  given Schema[TestSelectionError] = Schema.derived
  given Schema[TestFailureNode]    = Schema.derived
  given Schema[TestNodeFailure]    = Schema.derived
  given Schema[ScopedTestFailure]  = Schema.derived
end WireSchemaScoped
