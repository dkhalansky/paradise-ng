package test.macros
import scala.meta._
import com.github.dkhalansky.paradiseng.lib._

class FooAnnotation extends ParadiseNgAnnotation {
  def apply(annottee: Stat): Stat = {
      q"""
        class Foo {
            def bar() = 15 + 16
        }
      """
  }
}
