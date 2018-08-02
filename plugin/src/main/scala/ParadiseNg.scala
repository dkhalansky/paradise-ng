package localhost.plugin

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
with SymbolSources
with Annotations
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
                    val ants = withTyped(unit) { typed =>
                        attachSourcesToSymbols(typed)
                        annottees(typed).map { case (md, ans) =>
                            val companion = getCompanionTree(md.symbol)
                                .map(t => t.pos.start-1)
                            val start : Stat => Stat = m => m
                            var fn = (start /: ans) {
                                (f, a) => getAnnotationFunction(a._1) compose f
                            }
                            (md.pos.start-1, companion, fn)
                        }
                    }
                    if (ants.isEmpty)
                        return

                    val tr = {
                        val metatree = unit.source.content.parse[Source].get
                        new meta.ParadiseNgTransformer(metatree)
                    }

                    for ((md, comp, fn) <- ants) {
                        tr.modify(md, comp, fn)
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
