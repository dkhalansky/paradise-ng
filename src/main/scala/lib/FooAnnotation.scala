package localhost.lib
import scala.meta._

class FooAnnotation extends ParadiseNgAnnotation {
  def apply(annottee: Tree): Tree = {
      q"""
        class Foo {
            def bar() = println("bar")
        }
      """
  }
}
