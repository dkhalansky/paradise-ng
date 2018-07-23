package localhost.lib

import scala.annotation.StaticAnnotation
import scala.meta._

trait ParadiseNgAnnotation extends StaticAnnotation {
    def apply(annottee: Stat): Stat
}
