package com.github.dkhalansky.paradiseng.plugin

/*  This trait is devoted to finding the annotations that have the corresponding
    macro functions. */
trait Annotations extends Companions { self: ParadiseNgComponent =>

    import global._
    private type AnInfos = List[(AnnotationInfo, Int)]

    /* Given an AnnotationInfo (received by typechecking a tree and looking at
       the list of annotations for a symbol), determine if it belongs to
       ParadiseNg. */
    private def isOurTypedAnnotation(an: AnnotationInfo) : Boolean = {
        import com.github.dkhalansky.paradiseng.lib._
        an.symbol isNonBottomSubClass symbolOf[TreeTransformation]
    }

    /* Find the subtrees that are annotated using ParadiseNg's annotations,
       alongside with the relevant annotation definitions, their indices, and
       the number of annotated parents. */
    private def ourAnnottees(tree: Tree) = {
        import scala.collection.mutable.ArrayBuffer
        var buffer = new ArrayBuffer[(MemberDef, AnInfos, Int)]
        object annoteesTraverser extends Traverser {
            var depth = 0
            override def traverse(tree: Tree) {
                tree match {
                    case md: MemberDef if tree.symbol != null => {
                        val true_annots = tree.symbol.annotations.
                            zipWithIndex.filter(a => isOurTypedAnnotation(a._1))
                        if (!true_annots.isEmpty) {
                            buffer += ((md, true_annots, depth))
                            depth += 1
                            super.traverse(tree)
                            depth -= 1
                        } else {
                            super.traverse(tree)
                        }
                    }
                    case _ => super.traverse(tree)
                }
            }
        }
        annoteesTraverser.traverse(tree)
        buffer.to[List]
    }

    /*  Given a typed tree, return a list of member definitions that are
        annotated with macro annotations, the list of such annotations with
        their indices, and the number of annotated parents of a definition.

        This method returns the member definitions in the order in which
        they should be subjected to expansion. That is, the innermost trees
        are expanded first, and of the trees with an equal depth, module
        definitions are expanded first since companion object should be
        processed before the accompanied definition. */
    def annottees(tree: Tree): List[(MemberDef, AnInfos, Int)] = {
        ourAnnottees(tree)
            /* Increase the depth if we're dealing with a companion object.
               This is done so that a companion object is always processed
               before the accompanied definition is. */
            .map({
                case (md: ModuleDef, ans, depth) => (md, ans, 2*depth+1)
                case (md,            ans, depth) => (md, ans, 2*depth)
            })
            /* sort in order of decreasing depth so we process children before
               their parents. */
            .sortWith { _._3 > _._3 }
            /* Restore the original depth. */
            .map({ case (md, ans, depth) => (md, ans, depth/2) })
    }

}
