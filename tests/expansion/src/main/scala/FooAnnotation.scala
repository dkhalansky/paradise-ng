package test.macros
import scala.meta._
import localhost.lib._

class FooAnnotation extends ParadiseNgAnnotation {
  def apply(annottee: Stat): Stat = {
      q"""
        class Foo {
            def bar() = println("bar")
        }
      """
  }
}
