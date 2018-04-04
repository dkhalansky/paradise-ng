package localhost.lib
import scala.meta._

class ScalametaIdAnnotation extends ParadiseNgAnnotation {
  def apply(annottee: Tree): Tree = annottee
}
