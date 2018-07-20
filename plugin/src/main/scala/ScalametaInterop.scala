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

/* Given a tree, apply a series of transformations to it. */
class ScalametaTransformer(
    val tree: Tree,
    val extractor: ScalametaSourceExtractor
) {
    import ScalametaTreeStorage._
    private var storage = tree

    private def getAt(position: Int) : Stat = {
        val pos = extractor.findAtPos(position).pos
        storage.collect {
            case df : Stat if df.getPosition() == pos => df
        } match { case List(t) => t }
    }

    private def setAt(position: Int, newtree: Tree) {
        val pos = extractor.findAtPos(position).pos
        storage.traverse {
            case df if df.getPosition() == pos => {
                df storePayload newtree
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
        storage.transform {
            case s => s.getPayload[Tree]().getOrElse(s)
        }
    }
}

/* Given a tree in the scalameta format, the extractor can, given a
   position, find the innermost subtree that contains the position.

   This class exists as a workaround for the problem that the Scala compiler
   specifies not range positions for trees but only the starting positions.
   If the compiler provided both start and end positions, it would be easy to
   just get the corresponding source code from the source file and then parse
   it with scalameta.

   `scalac` provides an `-Yrangepos` flag which forces the compiler to provide
   range positions, but it isn't really advisable to use it since it breaks
   many plugins due to the fact that it agressively checks that after the typer
   phase the positions of the trees are sensible, that is, that children of a
   parent lie inside the parent and don't overlap. Many plugins that perform
   transformations don't test with this flag on, and without it, no such checks
   are performed. */
class ScalametaSourceExtractor(val tree: Tree) {
    /* Find the innermost subtree that contains the specified position.
       If the position is out of bounds, `null` is returned, so it is
       advised not to do that.

       In case one wants to find a subtree corresponding to some function
       declaration, the best strategy would be to provide a position of a
       keyword that specifies the type of the declaration, like this:

           @annotation1 @annotation2 class A { def foo() { } }
                                     ^

       If one pointed somewhere to the left, the innermost subtree would
       contain an annotation identifier. If one pointed at an `A`, then
       the `A` identifier would be the innermost subtree. If one pointed
       at the code block, the code block's subtree would be chosen over the
       declaration subtree. */
    def findAtPos(pos: Int): Tree = {
        var seen: Tree = null;
        tree.traverse {
            case s if s.pos.start <= pos && s.pos.end > pos => seen = s
        }
        seen
    }

    /* Find the innermost subtree that contains the specified position,
       returning not the subtree, but the order of the subtree in the traversal
       of the original tree that is performed by the `transform` method. */
    def posAtTransformOrder(pos: Int): Int = {
        var order = 0
        var seen  = 0
        tree.transform {
            case s => {
                order += 1
                if (s.pos.start <= pos && s.pos.end > pos) {
                    seen = order
                }
                s
            }
        }
        seen
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
