package test.macros
import scala.meta._
import localhost.lib._

class HelloSomething(val hello: String = "Hi", val world: String = "Earth")
extends ParadiseNgAnnotation {
  def apply(annottee: Tree): Tree = {
      val name = Term.Name(hello + world)
      q"""def $name() = $hello + ", " + $world + "!""""
  }
}
