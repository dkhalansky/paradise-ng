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
with ReplIntegration
with TypeParamsDesugar
{
    import global._

    override val phaseName  = "paradise-ng"
    override val runsAfter  = "parser" :: Nil
    override val runsBefore = "namer"  :: Nil
    def newPhase(prev: Phase) = {
        object phase extends StdPhase(prev) {
            def apply(unit: CompilationUnit): Unit = {
                val afterHiding = HideTypeParameters(unit.body)
                try {
                    val ants = withTyped(unit, afterHiding) { typed =>
                        attachSourcesToSymbols(typed)
                        annottees(typed).map { case (md, ans, depth) =>
                            val companion = getCompanionTree(md.symbol)
                                .map(t => t.pos)
                            val nans = ans map { case (an, ix) =>
                                (getAnnotationFunction(an), ix)
                            }
                            (md.pos, companion, nans, depth)
                        }
                    }
                    if (ants.isEmpty)
                        return

                    val tr = {
                        val metatree = unit.source.content.parse[Source].get
                        new meta.ParadiseNgTransformer(metatree)
                    }

                    for ((md, comp, ans, depth) <- ants) {
                        tr.modify(md.start-1, comp.map(_.start-1), new
                            localhost.lib.AnnotationCombination(ans))
                    }

                    unit.body = newUnitParser(tr.get().toString()).parse()
                    updateReplHandlers(unit.body)
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
    def withTyped[T](unit: CompilationUnit, body: Tree)(fn: Tree => T): T = {
        val packageIdent = TermName("'paradise-ng")
        val tree = PackageDef(Ident(packageIdent),
            List(body.duplicate))
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
