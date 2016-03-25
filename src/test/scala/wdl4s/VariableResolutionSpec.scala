package wdl4s

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import org.scalatest.{FlatSpec, Matchers}
import wdl4s.expression.NoFunctions
import wdl4s.types.{WdlArrayType, WdlStringType}
import wdl4s.values.{WdlCallOutputsObject, WdlArray, WdlValue, WdlString}

import better.files._
import java.io.{File => JFile}

class VariableResolutionSpec extends FlatSpec with Matchers with WdlTest {
  val namespace = WdlNamespaceWithWorkflow.load(
    """String global = "global"
      |
      |task taskA {
      |  String i
      |  command {sh script.sh ${i}}
      |  output {String o = read_string(stdout())}
      |}
      |
      |task taskB {
      |  Int i
      |  command {python add_one.py ${i}}
      |  output {Int o = read_int(stdout())}
      |}
      |
      |workflow w {
      |  Int i = 2
      |  String foo = "foo"
      |  Array[String] arr
      |
      |  call taskA as A
      |
      |  if (i == 2) {
      |    Int j = i + 1
      |    Int h = 100
      |
      |    call taskB as add_first {input: i=j}
      |    call taskB as add_second {input: i=i+h}
      |    call taskA as B
      |
      |    scatter (y in arr) {
      |      String k = y + foo
      |
      |      if (y == "foo") {
      |        Int m = i + 200
      |
      |        call taskB as add_third {input: i=m}
      |        call taskA as C {input: i=k+global}
      |      }
      |    }
      |  }
      |
      |  call taskA as D {input: i=C.o[0]}
      |}
    """.stripMargin)

  val taskA = getTask("taskA")
  val taskB = getTask("taskB")

  val callA = getCall("A")
  val callB = getCall("B")
  val callC = getCall("C")
  val callD = getCall("D")
  val callAddFirst = getCall("add_first")
  val callAddSecond = getCall("add_second")
  val callAddThird = getCall("add_third")

  val declWfI = workflow.declarations.find(_.unqualifiedName == "i").get
  val declWfFoo = workflow.declarations.find(_.unqualifiedName == "foo").get
  val declWfArr = workflow.declarations.find(_.unqualifiedName == "arr").get

  val callAIn = callA.declarations.head
  val callBIn = callB.declarations.head
  val callCIn = callC.declarations.head
  val callDIn = callD.declarations.head
  val callAddFirstIn = callAddFirst.declarations.head
  val callAddSecondIn = callAddSecond.declarations.head
  val callAddThirdIn = callAddThird.declarations.head

  val scatter0 = getScatter(0)
  val if0 = getIf(0)
  val if1 = getIf(1)

  val declIf0H = if0.declarations.find(_.unqualifiedName == "h").get
  val declIf0J = if0.declarations.find(_.unqualifiedName == "j").get
  val declIf1M = if1.declarations.find(_.unqualifiedName == "m").get

  val declScatterK = scatter0.declarations.find(_.unqualifiedName == "k").get

  it should "foobar" in {
    val testCases = File("src/test/cases")
    testCases.createDirectories()
    val testCaseDirs = testCases.list.toSeq
    testCaseDirs

    // FQN / FQN with scopes
    // upstream / downstream
    // parent / child
    // scatters
    // ifs
    println(testCases.path)
  }

  /*it should "have 2 tasks" in {
    namespace.tasks shouldEqual Seq(taskA, taskB)
  }

  it should "have 0 imported WdlNamespaces" in {
    namespace.namespaces shouldEqual Seq()
  }

  val fqnTable = Table(
    ("fqn", "fqnWithAllScopes", "scope"),
    ("w", "w", workflow),
    ("taskA", "taskA", taskA),
    ("taskB", "taskB", taskB),
    ("w.i", "w.i", declWfI),
    ("w.foo", "w.foo", declWfFoo),
    ("w.arr", "w.arr", declWfArr),
    ("w.A", "w.A", callA),
    ("w.$if_0", "w.$if_0", if0),
    ("w.j", "w.$if_0.j", declIf0J),
    ("w.h", "w.$if_0.h", declIf0H),
    ("w.add_first", "w.$if_0.add_first", callAddFirst),
    ("w.add_second", "w.$if_0.add_second", callAddSecond),
    ("w.B", "w.$if_0.B", callB),
    ("w.k", "w.$if_0.$scatter_0.k", declScatterK),
    ("w.$if_1", "w.$if_0.$scatter_0.$if_1", if1),
    ("w.m", "w.$if_0.$scatter_0.$if_1.m", declIf1M),
    ("w.add_third", "w.$if_0.$scatter_0.$if_1.add_third", callAddThird),
    ("w.C", "w.$if_0.$scatter_0.$if_1.C", callC),
    ("w.D", "w.D", callD)
  )

  forAll(fqnTable) { (fqn, fqnWithAllScopes, scope) =>
    it should s"resolve FQN: $fqn" in {
      namespace.resolve(fqn) shouldEqual Option(scope)
    }

    it should s"resolve FQN (with scopes): $fqnWithAllScopes" in {
      namespace.resolve(fqnWithAllScopes) shouldEqual Option(scope)
    }

    it should s"generate FQN: $fqn" in {
      scope.fullyQualifiedName shouldEqual fqn
    }

    it should s"generate FQN (with scopes): $fqn" in {
      scope.fullyQualifiedNameWithIndexScopes shouldEqual fqnWithAllScopes
    }
  }

  val stringArray = WdlArray(WdlArrayType(WdlStringType), Seq("alpha", "beta", "gamma").map(WdlString))
  val callCObject = WdlCallOutputsObject(
    callC, Map("o" -> WdlArray(WdlArrayType(WdlStringType), Seq("chi", "psi", "omega").map(WdlString)))
  )

  val commandTable = Table(
    ("node", "inputs", "command"),
    (callA, Map("w.A.i" -> WdlString("test")), "sh script.sh test"),
    (callAddSecond, Map.empty[String, WdlValue], "python add_one.py 102"),
    (callAddFirst, Map.empty[String, WdlValue], "python add_one.py 3"),
    (callAddThird, Map.empty[String, WdlValue], "python add_one.py 202"),
    (callC, Map("w.arr" -> stringArray), "sh script.sh betafooglobal"),
    (callD, Map("w.C" -> callCObject), "sh script.sh chi")
  )

  forAll(commandTable) { (node, inputs, command) =>
    it should s"instantiate a command for ${node.fullyQualifiedName}" in {
      node.instantiateCommandLine(inputs, NoFunctions, shards=Map(scatter0 -> 1))
    }
  }*/

  /*val dependencyTable = Table(
    ("node", "upstream", "downstream"),
    (callA, Set(), Set(if1, if2)),
    (callB, Set(if0), Set()),
    (callC, Set(if1), Set(callE)),
    (callD, Set(scatter0), Set()),
    (callE, Set(callC), Set()),
    (scatter0, Set(if2, declArr), Set(callD)),
    (if0, Set(declI), Set(callB)),
    (if1, Set(callA), Set(callC)),
    (if2, Set(callA), Set(scatter0)),
    (callAIn, Set(), Set()),
    (callBIn, Set(), Set()),
    (callCIn, Set(), Set()),
    (callDIn, Set(), Set()),
    (callEIn, Set(), Set())
  )

  forAll(dependencyTable) { (node, upstream, downstream) =>
    it should s"compute upstream nodes for ${node.fullyQualifiedName}" in {
      node.upstream shouldEqual upstream
    }
    it should s"compute downstream nodes for ${node.fullyQualifiedName}" in {
      node.downstream shouldEqual downstream
    }
  }

  val parentAndChildrenTable = Table(
    ("node", "parent", "children"),
    (callA, Option(workflow), Seq(callAIn)),
    (callB, Option(if0), Seq(callBIn)),
    (callC, Option(if1), Seq(callCIn)),
    (callD, Option(scatter0), Seq(callDIn)),
    (callE, Option(workflow), Seq(callEIn)),
    (workflow, Option(namespace), Seq(declI, declArr, callA, if0, if1, if2, callE)),
    (namespace, None, Seq(taskA, workflow)),
    (declI, Option(workflow), Seq()),
    (declArr, Option(workflow), Seq())
  )

  forAll(parentAndChildrenTable) { (node, parent, children) =>
    it should s"compute children for ${node.fullyQualifiedName}" in {
      node.children shouldEqual children
    }
    it should s"compute parents for ${node.fullyQualifiedName}" in {
      node.parent shouldEqual parent
    }
  }

  val ancestryTable = Table(
    ("node", "ancestry"),
    (namespace, Seq()),
    (workflow, Seq(namespace)),
    (taskA, Seq(namespace)),
    (taskAIn, Seq(taskA, namespace)),
    (callA, Seq(workflow, namespace)),
    (callB, Seq(if0, workflow, namespace)),
    (callC, Seq(if1, workflow, namespace)),
    (callD, Seq(scatter0, if2, workflow, namespace)),
    (callE, Seq(workflow, namespace)),
    (scatter0, Seq(if2, workflow, namespace)),
    (if0, Seq(workflow, namespace)),
    (if1, Seq(workflow, namespace)),
    (if2, Seq(workflow, namespace)),
    (callAIn, Seq(callA, workflow, namespace)),
    (callBIn, Seq(callB, if0, workflow, namespace)),
    (callCIn, Seq(callC, if1, workflow, namespace)),
    (callDIn, Seq(callD, scatter0, if2, workflow, namespace)),
    (callEIn, Seq(callE, workflow, namespace))
  )

  forAll(ancestryTable) { (node, ancestry) =>
    it should s"compute ancestry for ${node.fullyQualifiedName}" in {
      node.ancestry shouldEqual ancestry
    }
  }

  val scatterTable = Table(
    ("node", "item", "collection", "index"),
    (scatter0, "x", "arr", 0)
  )

  forAll(scatterTable) { (node, item, collection, index) =>
    it should s"have '$item' as the iteration variable for scatter block ${node.fullyQualifiedName}" in {
      node.item shouldEqual item
    }
    it should s"have '$collection' as the expression for scatter block ${node.fullyQualifiedName}" in {
      node.collection.toWdlString shouldEqual collection
    }
    it should s"have '$index' as the index scatter block ${node.fullyQualifiedName}" in {
      node.index shouldEqual index
    }
  }

  val ifTable = Table(
    ("node", "expression", "index"),
    (if0, "i == 2", 0),
    (if1, "A.o == \"foo\"", 1),
    (if2, "A.o == \"bar\"", 2)
  )

  forAll(ifTable) { (node, expression, index) =>
    it should s"have a correct condition expression for if block ${node.fullyQualifiedName}" in {
      node.condition.toWdlString shouldEqual expression
    }
    it should s"have a correct index for if block ${node.fullyQualifiedName}" in {
      node.index shouldEqual index
    }
  }

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

  it should "Have five 'Call' objects" in {
    namespace.workflow.calls shouldEqual Set(callA, callB, callC, callD, callE)
  }

  it should "Have one 'Scatter' objects" in {
    namespace.workflow.scatters shouldEqual Set(scatter0)
  }*/
}
