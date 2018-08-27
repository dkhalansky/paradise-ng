package macros
import scala.meta._
import com.github.dkhalansky.paradiseng.lib._

class replaceWithFoo(val v: Int) extends ParadiseNgAnnotation {
    override def apply(s: Stat): Stat = q"object foo { def bar = $v }"
}
