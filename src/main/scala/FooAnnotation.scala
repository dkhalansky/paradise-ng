package test.macros
import scala.meta._
import localhost.lib._

class FooAnnotation extends ParadiseNgAnnotation {
  def apply(annottee: Tree): Tree = {
      q"""
        class Foo {
            def bar() = println("bar")
        }
      """
  }
}
