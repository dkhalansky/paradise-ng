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
    }
}
