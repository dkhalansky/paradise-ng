class Repl extends ReplSuite("repl") {

    test("Macro annotations do not crash") {
        val printout = repl("""
          |import scala.meta._
          |import localhost.lib._
          |class main(x: Int) extends ParadiseNgAnnotation {
          |    override def apply(defn: Stat): Stat = { defn }
          |}
          |@main(42) class C
        """.stripMargin.trim)
        assert(printout.contains("defined class main"))
        assert(printout.contains("defined class C"))
    }

    test("Macro annotations expand") {
        val printout = repl("""
          |import scala.meta._
          |import localhost.lib._
          |class main(x: Int) extends ParadiseNgAnnotation {
          |    override def apply(defn: Stat): Stat = { q"def foo = $x" }
          |}
          |@main(42) def foo = 5
          |foo + 1024
        """.stripMargin.trim)
        assert(printout.contains((1024 + 42).toString))
    }

    test("Annotations modify the set of definitions") {
        val printout = repl("""
          |import scala.meta._
          |import localhost.lib._
          |class main(x: Int, s: String) extends ParadiseNgAnnotation {
          |    override def apply(defn: Stat): Stat = {
          |        val nm = Term.Name(s)
          |        Term.Block(List(q"class D",
          |          q"object $nm { def bar = $x * 133 }",
          |          q"import scala.collection.mutable._"))
          |    }
          |}
          |@main(15, "MyObj") def foo = 6
          |BitSet
          |MyObj.bar
        """.stripMargin.trim)
        assert(printout.contains("defined class D"))
        assert(printout.contains("defined object MyObj"))
        assert(printout.contains("scala.collection.mutable.BitSet.type"))
        assert(printout.contains((15 * 133).toString))
    }

}
