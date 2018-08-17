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
                    var encounteredErrors = false
                    val ants = withTyped(unit, afterHiding) { typed =>
                        attachSourcesToSymbols(typed)
                        annottees(typed).map { case (md, ans, depth) =>
                            val companion = getCompanionTree(md.symbol)
                                .map(t => t.pos)
                            val nans = ans map { case (an, ix) =>
                                (try getAnnotationFunction(an) catch {
                                    case ParadiseNgException(pos, msg) => {
                                        encounteredErrors = true
                                        reporter.error(pos, msg)
                                        null
                                    }
                                }, ix)
                            }
                            (md.pos, companion, nans, depth)
                        }
                    }
                    if (encounteredErrors || ants.isEmpty)
                        return

                    val tr = {
                        val metatree = unit.source.content.parse[Source].get
                        new meta.ParadiseNgTransformer(metatree)
                    }

                    var body = unit.body
                    for ((md, comp, ans, depth) <- ants) {
                        val fn = new localhost.lib.AnnotationCombination(ans)
                        val getStats = (v: List[AnyRef]) => v.map { s =>
                            newUnitParser(s.toString()).parseStats()(0) }
                        comp match {
                            case None => {
                                val nt = tr.modify(fn, md.start-1)
                                if (depth == 0) {
                                    body = replaceTree(body, md, getStats(nt))
                                }
                            }
                            case Some(c) => {
                                val (nt, cnt) = tr.modify(
                                    fn, md.start-1, c.start-1)
                                if (depth == 0) {
                                    body = replaceTree(body, md, getStats(nt))
                                    body = replaceTree(body, c, getStats(cnt))
                                }
                            }
                        }
                    }

                    unit.body = body
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

    def replaceTree(body: Tree, pos: Position, newTrees: List[Tree]): Tree = {
        newTrees map { t => t.setPos(pos) }
        def newStats(ss: List[Tree]): List[Tree] = ss.flatMap { s =>
            if (s.pos == pos) { newTrees } else { List(s) }
        }
        object transformer extends Transformer {
            var found = false
            override def transform(tree: Tree): Tree = {
                if (!found && tree.children.exists(_.pos == pos)) {
                    found = true
                    tree match {
                        case o@PackageDef(_, s)  => o.copy(stats = newStats(s))
                        case o@Block(s, _)       => o.copy(stats = newStats(s))
                        case o@Template(_, _, s) => o.copy(body  = newStats(s))
                    }
                } else {
                    super.transform(tree)
                }
            }
        }
        transformer.transform(body)
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
