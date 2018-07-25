package localhost.plugin
import scala.meta._

import java.io.InputStream
import java.nio.charset.StandardCharsets
import scala.meta.internal.tokens.TokenStreamPosition
import scala.meta.internal.trees.Origin
import scala.meta.transversers._
import scala.reflect.ClassTag

object ScalametaTreeStorage {

    private case class Storage[T: ClassTag](payload: T, pos: Position)
    extends InputStream {
        override def read(): Int = -1
    }

    private def originField(tree: Tree): java.lang.reflect.Field = {
        val origin = tree.getClass()
            .getDeclaredFields()
            .find(_.getName == "privateOrigin")
            .get
        origin.setAccessible(true)
        origin
    }

    implicit class XtensionTreeStorage[T <: Tree](val tree: T) extends AnyVal {
        def storePayload[U: ClassTag](payload: U): T = {
            originField(tree).set(tree,
                Origin.Parsed(
                    Input.Stream(new Storage[U](payload, getPosition()),
                        StandardCharsets.UTF_8),
                    dialects.Scala212,
                    TokenStreamPosition(-1, -1)
                )
            )
            tree
        }

        def getPayload[U: ClassTag](): Option[U] = {
            originField(tree).get(tree) match {
                case Origin.Parsed(Input.Stream(Storage(p: U, _), _), _, _)
                    => Some(p)
                case _ => None
            }
        }

        def getPosition(): Position = {
            originField(tree).get(tree) match {
                case Origin.Parsed(Input.Stream(Storage(_, p), _), _, _) => p
                case _ => tree.pos
            }
        }
    }

}

object PostOrderTraversal {
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

/* Given a tree, apply a series of transformations to it. */
class ScalametaTransformer(var tree: Tree) {
    import ScalametaTreeStorage._
    import PostOrderTraversal._

    private def contains(p: Position, i: Int) : Boolean = {
        p.start <= i && p.end > i
    }

    private def getAt(position: Int) : Stat = {
        tree.findDfs[Stat](t => contains(t.getPosition(), position)).get
    }

    private def replaceChild(
        parent: Tree,
        child: Stat,
        newchildren: List[Stat]
    ) : Tree = {
        def newStats(old: List[Stat]) : List[Stat] = old.flatMap {
            case v if v.equals(child) => newchildren
            case v => List(v)
        }
        child.parent.get match {
            case o @ Template(_, _, _, stats) => o.copy(stats = newStats(stats))
            case o @ Source(stats)            => o.copy(stats = newStats(stats))
            case o @ Term.Block(stats)        => o.copy(stats = newStats(stats))
            case o @ Pkg(_, stats)            => o.copy(stats = newStats(stats))
        }
    }

    def modify(position: Int, companionPos: Option[Int], fn: Stat => Stat) {
        val tree = getAt(position)
        val companion = companionPos.map(getAt)
        val arg = companion.map(c => Term.Block(List(tree, c))).getOrElse(tree)
        val expanded = fn((arg.transform {
            case s => s.getPayload[Tree]().getOrElse(s)
        }).asInstanceOf[Stat])
        expanded match {
            case Term.Block(lst @ List(t, c)) => {
                tree storePayload t
                companion match {
                    case None => {
                        val parent = tree.parent.get
                        parent storePayload replaceChild(parent, tree, lst)
                    }
                    case Some(p) => p storePayload c
                }
            }
            case t => {
                tree storePayload t
                companion match {
                    case None =>
                    case Some(p) => {
                        val parent = p.parent.get
                        parent storePayload replaceChild(parent, p, List())
                    }
                }
            }
        }
    }

    def get(): Tree = {
        tree.transform {
            case s => s.getPayload[Tree]().getOrElse(s)
        }
    }
}
