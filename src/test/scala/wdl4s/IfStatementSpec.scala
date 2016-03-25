package wdl4s

import better.files._
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table

class IfStatementSpec extends WdlTest {
  val ns = loadWdlFile(File("src/test/cases/if_statement/test.wdl"))
  val callD = getCall(ns, "D")
  val callE = getCall(ns, "E")
  val scatter0 = getScatter(ns, 0)

  val lookupVarTable = Table(
    ("node", "variable", "resolution"),
    (callD, "x", Some(scatter0)),
    (callE, "D", Some(callD))
  )

  forAll(lookupVarTable) { (node, variable, resolution) =>
    it should s"resolve variable $variable (relative to ${node.fullyQualifiedName}) -> ${resolution.map(_.fullyQualifiedName).getOrElse("None")}" in {
      node.resolveVariable(variable) shouldEqual resolution
    }
  }
}
