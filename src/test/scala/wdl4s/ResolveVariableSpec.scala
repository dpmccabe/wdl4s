package wdl4s

import better.files._
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table

class ResolveVariableSpec extends WdlTest {
  val ns = loadWdlFile(File("src/test/cases/if_statement/test.wdl"))
  val callD = getCall(ns, "D")
  val callE = getCall(ns, "E")
  val scatter0 = getScatter(ns, 0)

  val lookupVarTable = Table(
    ("node", "variable", "resolution"),
    ("w.D", "x", Some("w.$scatter_0")),
    ("w.E", "D", Some("w.D")),
    ("w.D", "B", Some("w.B")),
    ("w.D", "A", Some("w.A")),
    ("w.D", "arr", Some("w.arr")),
    ("w.A", "arr", Some("w.arr")),
    ("w.A", "i", Some("w.A.i")),
    ("w.D", "i", Some("w.D.i")),
    ("w", "i", Some("w.i")),
    ("w.$if_0", "i", Some("w.i"))
  )

  forAll(lookupVarTable) { (node, variable, resolution) =>
    it should s"resolve variable $variable (relative to $node) -> ${resolution.getOrElse("None")}" in {
      ns.resolve(node).flatMap(_.resolveVariable(variable)) shouldEqual resolution.flatMap(ns.resolve)
    }
  }
}
