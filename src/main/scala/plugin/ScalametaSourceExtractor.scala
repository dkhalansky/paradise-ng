package localhost.plugin
import scala.meta._

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
}

object ScalametaSourceExtractor {
    /* Given a file path, create an extractor with the tree parsed from the
       source code that is in the file. */
    def fromFile(path: String): ScalametaSourceExtractor = {
        val source = scala.io.Source.fromFile(path)
        val text = try source.mkString finally source.close()
        fromString(text)
    }

    /* Parse the string into a scalameta tree and create the corresponding
       extractor. */
    def fromString(str: String): ScalametaSourceExtractor = {
        // TODO: error handling
        val metatree = str.parse[Source].get
        new ScalametaSourceExtractor(metatree)
    }
}
