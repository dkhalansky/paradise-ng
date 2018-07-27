package localhost.plugin.meta
import scala.meta._
import scala.meta.transversers.Traverser
import scala.reflect.ClassTag

object Transversers {
    implicit class XtensionPostOrderTraverser[T <: Tree](val tree: T)
    extends AnyVal {
        def traversePostOrder(fn: PartialFunction[Tree, Unit]) {
            val liftedFn = fn.lift
            (new Traverser {
                override def apply(tree: Tree) {
                    super.apply(tree)
                    liftedFn(tree)
                }
            })(tree)
        }

        def findDfs[U <: Tree: ClassTag](pred: U => Boolean): Option[U] = {
            traversePostOrder { case t: U if pred(t) => return Some(t) }
            None
        }

        def findPos[U <: Tree: ClassTag](pos: Position): Option[U] = {
            val f = (u: U) => return Some(u);
            (new Traverser {
                override def apply(tree: Tree) {
                    val p = tree.pos
                    if (p.start <= pos.start && p.end >= pos.end) {
                        super.apply(tree)
                        tree match {
                            case u: U => f(u)
                            case _ =>
                        }
                    }
                }
            })(tree)
            None
        }
    }
}
