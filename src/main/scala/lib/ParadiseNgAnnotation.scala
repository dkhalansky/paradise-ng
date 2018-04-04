package localhost.lib

import scala.annotation.StaticAnnotation
import scala.meta.Tree

trait ParadiseNgAnnotation extends StaticAnnotation {
    def apply(annottee: Tree): Tree
}
