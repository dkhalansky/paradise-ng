package localhost.plugin

trait Annotations extends Companions { self: ParadiseNgComponent =>

    import global._

    /* Given an AnnotationInfo (received by typechecking a tree and looking at
       the list of annotations for a symbol), determine if it belongs to
       ParadiseNg. */
    private def isOurTypedAnnotation(an: AnnotationInfo) : Boolean = {
        import localhost.lib._
        an.symbol isNonBottomSubClass symbolOf[ParadiseNgAnnotation]
    }

    /* Find the subtrees that are annotated using ParadiseNg's annotations,
       alongside with the relevant annotation definitions and the depth on
       which the tree was encountered. */
    private def ourAnnottees(tree: Tree) = {
        import scala.collection.mutable.ArrayBuffer
        var buffer = new ArrayBuffer[(MemberDef, List[AnnotationInfo], Int)]
        var depth = 0
        object annoteesTraverser extends Traverser {
            override def traverse(tree: Tree) {
                tree match {
                    case md: MemberDef if tree.symbol != null => {
                        val true_annots = tree.symbol.annotations.filter(
                            isOurTypedAnnotation)
                        if (!true_annots.isEmpty) {
                            buffer += ((md, true_annots, depth))
                        }
                    }
                    case _ =>
                }
                depth += 1
                super.traverse(tree)
                depth -= 1
            }
        }
        annoteesTraverser.traverse(tree)
        buffer.to[List]
    }

    def annottees(tree: Tree) : List[(MemberDef, List[AnnotationInfo])]  = {
        ourAnnottees(tree)
            /* Increase the depth if we're dealing with a companion object.
               This is done so that a companion object is always processed
               before the accompanied definition is.

                   @D
                   class A // depth = 1; recDepth = 1

                   @C
                   object A // depth = 1; recDepth = 2
                   { // depth = 2: blocks are separate trees
                       @B
                       class E // depth = 3; recDepth = 3
                   }
            */
            .map({ case (md, ans, depth) =>
                val recDepth = depth + md.isInstanceOf[ModuleDef].compare(false)
                (recDepth, md, ans)
            })
            /* sort in order of decreasing depth so we process children before
               their parents. */
            .sortWith { _._1 > _._1 }
            /* We no longer need to know the depth, everything is ordered
               already. */
            .map({ case (_, md, ans) => (md, ans) })
    }

}
