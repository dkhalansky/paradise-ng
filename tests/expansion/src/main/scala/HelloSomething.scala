package test.macros
import scala.meta._
import localhost.lib._

class HelloSomething(val hello: String = "Hi", val world: String = "Earth")
extends ParadiseNgAnnotation {
  def apply(annottee: Stat): Stat = {
      val name = Term.Name(hello + world)
      q"""def $name() = $hello + ", " + $world + "!""""
  }
}

class HelloPrimitives(val int: Int, val dbl: Double, val bt: Byte)
extends ParadiseNgAnnotation {
    def apply(annottee: Stat): Stat = {
        val name = Term.Name("hello" + int.toString)
        q"def $name() = $dbl + ${bt.asInstanceOf[Int]} * $int"
    }
}
