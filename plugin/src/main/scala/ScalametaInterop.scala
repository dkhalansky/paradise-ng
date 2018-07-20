package localhost.plugin
import scala.meta._

import java.io.InputStream
import java.nio.charset.StandardCharsets
import scala.meta.internal.tokens.TokenStreamPosition
import scala.meta.internal.trees.Origin
import scala.meta.transversers._

object ScalametaTreeStorage {

    import scala.reflect.ClassTag

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
        tree.traversePostOrder {
            case df: Stat if contains(df.getPosition(), position) => return df
        }
        return q"{}"
    }

    private def setAt(position: Int, newtree: Tree) {
        var done = false
        tree.traversePostOrder {
            case df : Stat if contains(df.getPosition(), position) && !done => {
                df storePayload newtree
                done = true
                return
            }
        }
    }

    def modify(position: Int, companionPos: Option[Int], fn: Tree => Tree) {
        val tree = getAt(position)
        val arg = companionPos match {
            case None => tree
            case Some(p) => Term.Block(List(tree, getAt(p)))
        }
        val expanded = fn(arg.transform {
            case s => s.getPayload[Tree]().getOrElse(s)
        })
        expanded match {
            case Term.Block(List(tree, companion)) => {
                setAt(position, tree)
                companionPos match {
                    case None => // an error
                    case Some(p) => setAt(p, companion)
                }
            }
            case tree => {
                setAt(position, tree)
                companionPos match {
                    case None =>
                    case Some(p) => setAt(p, q"{}")
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

object ScalametaParser {
    /* Given a file path, create the tree parsed from the source code that
       is in the file. */
    def fromFile(path: String): Tree = {
        val source = scala.io.Source.fromFile(path)
        val text = try source.mkString finally source.close()
        create(text)
    }

    /* Parse the string into a scalameta tree. */
    def create(str: String): Tree = {
        // TODO: error handling
        str.parse[Source].get
    }

    /* Parse a char array into a scalameta tree. */
    def create(str: Array[Char]): Tree = {
        str.parse[Source].get
    }
}
