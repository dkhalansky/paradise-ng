package test.macros
import scala.meta._
import com.github.dkhalansky.paradiseng.lib._

class ScalametaIdAnnotation extends ParadiseNgAnnotation {
  def apply(annottee: Stat): Stat = annottee
}
