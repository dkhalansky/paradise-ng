package localhost.plugin
import localhost.lib._

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.reporters.StoreReporter
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

import scala.meta._

class ParadiseNg(val global: Global) extends Plugin {
    import global._
    val name = "paradise-ng"
    val description = "Manipulate ASTs"
    val components = new ParadiseNgComponent(this, global) :: Nil
}

class ParadiseNgComponent(plugin: Plugin, val global: Global)
extends PluginComponent
with Errors
with Companions
with SymbolSources
with AnnotationFunctions
{
    import global._

    override val phaseName  = "paradise-ng"
    override val runsAfter  = "parser" :: Nil
    override val runsBefore = "namer"  :: Nil
    def newPhase(prev: Phase) = {
        object phase extends StdPhase(prev) {
            def apply(unit: CompilationUnit): Unit = {
                try {
                    val annottees = withTyped(unit) { typed =>
                        attachSourcesToSymbols(typed)
                        ourAnnottees(typed).map { case (md, ans, depth) =>
                            val companion = getCompanionTree(md.symbol)
                            val start : Stat => Stat = m => m
                            var fn = (start /: ans) {
                                (f, an) => getAnnotationFunction(an) compose f
                            }
                            /* Increase the depth if we're dealing with a
                               companion object. This is done so that a
                               companion object is always processed before
                               the accompanied definition is.

                                   @D
                                   class A // depth = 1; recDepth = 1

                                   @C
                                   object A // depth = 1; recDepth = 2
                                   { // depth = 2: blocks are separate trees
                                       @B
                                       class E // depth = 3; recDepth = 3
                                   }
                            */
                            val recDepth = depth +
                                md.isInstanceOf[ModuleDef].compare(false)
                            (recDepth, md, companion, fn)
                        }
                    }
                    if (annottees.length == 0)
                        return

                    /* sort in order of decreasing depth so we process
                       children before their parents. */
                    val sorted = annottees.sortWith { _._1 > _._1 }

                    val tr = {
                        val metatree = unit.source.content.parse[Source].get
                        new meta.ParadiseNgTransformer(metatree)
                    }

                    for ((_, md, comp, fn) <- sorted) {
                        tr.modify(md.pos.start-1,
                            comp.map(t => t.pos.start-1), fn)
                    }

                    unit.body = newUnitParser(tr.get().toString()).parse()
                } catch {
                    // Use this exception for emergency exits
                    case ParadiseNgException(pos, msg) =>
                        reporter.error(pos, msg)
                }
            }
        }
        phase
    }

    /* Given an AnnotationInfo (received by typechecking a tree and looking at
       the list of annotations for a symbol), determine if it belongs to
       ParadiseNg. */
    def isOurTypedAnnotation(an: AnnotationInfo) : Boolean = {
        an.symbol isNonBottomSubClass symbolOf[ParadiseNgAnnotation]
    }

    /* Find the subtrees that are annotated using ParadiseNg's annotations,
       alongside with the relevant annotation definitions and the depth on
       which the tree was encountered. */
    def ourAnnottees(tree: Tree) = {
        import scala.collection.mutable.ArrayBuffer
        var buffer = new ArrayBuffer[(MemberDef, List[AnnotationInfo], Int)]
        var depth = 0
        object annoteesTraverser extends Traverser {
            override def traverse(tree: Tree) {
                tree match {
                    case md: MemberDef if tree.symbol != null => {
                        val true_annots = tree.symbol.annotations.filter(
                            isOurTypedAnnotation)
                        if (true_annots.length > 0) {
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
    } : List[(MemberDef, List[AnnotationInfo], Int)]

    /* Assign types to the tree as comprehensively as possible. */
    def withTyped[T](unit: CompilationUnit)(fn: Tree => T): T = {
        val packageIdent = TermName("'paradise-ng")
        val tree = PackageDef(Ident(packageIdent),
            List(unit.body.duplicate))
        val context = analyzer.rootContext(unit).
            make(tree, scope = newScope)
        val namer = analyzer.newNamer(context)

        val reporter = global.reporter
        global.reporter = new StoreReporter()
        val typed = try {
            val ct = namer.enterSym(tree)
            val typer = analyzer.newTyper(ct)
            typer.typed(tree)
        } finally {
            global.reporter = reporter
        }

        val result = fn(typed)

        // erasing the fake package from the root class
        val decls = rootMirror.RootClass.info.decls
        val existing = decls.lookup(packageIdent)
        decls unlink existing

        result
    }

}
