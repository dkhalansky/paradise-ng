package localhost.plugin
import localhost.lib.ParadiseNgAnnotation

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.collection.mutable.ListBuffer

class ParadiseNg(val global: Global) extends Plugin {
    import global._
    val name = "paradise-ng"
    val description = "Manipulate ASTs"
    val components = new ParadiseNgComponent(this, global) :: Nil
}

class ParadiseNgComponent(plugin: Plugin, val global: Global)
extends PluginComponent {
    import global._

    val phaseName = "paradise-ng"
    val runsAfter = "parser" :: Nil

    def newPhase(_prev: Phase) : StdPhase = new ParadiseNgPhase(_prev)

    private class ParadiseNgPhase(prev: Phase) extends StdPhase(prev) {
        def apply(unit: CompilationUnit) : Unit = {
            val isMyAnnot = (v : AnnotationInfo) =>
                v.atp <:< typeOf[ParadiseNgAnnotation]
            val treesWithAnnotations = findAnnotated(isMyAnnot, unit.body)
            for ((t, an) <- treesWithAnnotations) {
                println(t)
                for (a <- an) {
                    println(a)
                }
                println("-------------")
            }
        }
    }

    def findAnnotated(predicate : AnnotationInfo => Boolean, tree : Tree) :
    List[(Tree, List[AnnotationInfo])] = {
        def annotationsOf(tree: Tree) : List[AnnotationInfo] = tree match {
            case t: MemberDef => t.symbol.annotations
            case Typed(_, tt) if tt.tpe != null => tt.tpe.annotations
            case _ => Nil
        }

        var trees = new ListBuffer[(Tree, List[AnnotationInfo])]()

        object traverser extends Traverser {
            override def traverse(tree: Tree): Unit = {
                annotationsOf(tree).filter(predicate) match {
                    case Nil => super.traverse(tree)
                    case lst => {
                        trees += new Tuple2(tree, lst)
                        super.traverse(tree)
                    }
                }
            }
        }

        traverser.traverse(tree)

        trees.toList
    }
}
