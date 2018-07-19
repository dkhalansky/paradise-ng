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
extends PluginComponent {
    import global._

    override val phaseName  = "paradise-ng"
    override val runsAfter  = "parser" :: Nil
    override val runsBefore = "namer"  :: Nil
    def newPhase(prev: Phase) = {
        object phase extends StdPhase(prev) {
            def apply(unit: CompilationUnit): Unit = {
                val typed = getPreliminarilyTyped(unit)
                var didApplyAnnotations = false
                lazy val tr = {
                    val metatree = ScalametaParser.create(
                        unit.source.content)
                    val anotherMetatree = ScalametaParser.create(
                        unit.source.content)
                    new ScalametaTransformer(metatree,
                        new ScalametaSourceExtractor(anotherMetatree))
                }
                for ((md, ans) <- ourAnnottees(typed).reverse) {
                    val start : scala.meta.Tree => scala.meta.Tree = m => m
                    var fn = (start /: ans) {
                        (f, an) => getAnnotationFunction(an) compose f
                    }
                    tr.modify(md.pos.start-1, fn)
                    didApplyAnnotations = true
                }
                if (didApplyAnnotations) {
                    unit.body = newUnitParser(tr.get().toString()).parse()
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
       alongside with the relevant annotation definitions. */
    def ourAnnottees(tree: Tree): List[(MemberDef, List[AnnotationInfo])] = {
        import scala.collection.mutable.ArrayBuffer
        var buffer = new ArrayBuffer[(MemberDef, List[AnnotationInfo])]
        object annoteesTraverser extends Traverser {
            override def traverse(tree: Tree) {
                tree match {
                    case md: MemberDef if tree.symbol != null => {
                        val true_annots = tree.symbol.annotations.filter(
                            isOurTypedAnnotation)
                        if (true_annots.length > 0) {
                            buffer += ((md, true_annots))
                        }
                    }
                    case _ =>
                }
                super.traverse(tree)
            }
        }
        annoteesTraverser.traverse(tree)
        buffer.to[List]
    }

    /* Assign types to the tree as comprehensively as possible. */
    def getPreliminarilyTyped(unit: CompilationUnit): Tree = {
        val reporter = global.reporter
        global.reporter = new StoreReporter()
        var result = unit.body
        try {
            val packageIdent = TermName("'paradise-ng")
            val tree = PackageDef(Ident(packageIdent),
                List(unit.body.duplicate))
            val context = analyzer.rootContext(unit).
                make(tree, scope = newScope).
                makeSilent(false)
            val namer = analyzer.newNamer(context)
            val ct = namer.enterSym(tree)
            val typer = analyzer.newTyper(ct)
            result = typer.typed(tree);

            // erasing the fake package from the root class
            val decls = rootMirror.RootClass.info.decls
            val existing = decls.lookup(packageIdent)
            decls unlink existing
        } finally {
            global.reporter = reporter
        }
        result
    }

    case class SymbolSourceAttachment(source: Tree)

    def attachSourcesToSymbols(tree: Tree) {
        (new Traverser {
            override def traverse(tree: Tree) {
                tree match {
                    case md : MemberDef
                    if md.symbol != null && md.symbol != NoSymbol => {
                        md.symbol.updateAttachment(SymbolSourceAttachment(md))
                    }
                    case _ =>
                }
                super.traverse(tree)
            }
        }).traverse(tree)
    }

    def getSource(symbol: Symbol): Option[Tree] = {
        symbol.attachments.get[SymbolSourceAttachment].map(att => att.source)
    }

    /* Given a symbol, determine the string to feed to the classloader to
       acquire the corresponding class. */
    def getClassNameBySymbol(symbol: Symbol) = {
        def loop(sym: Symbol): String = sym match {
            case sym if sym.isTopLevel =>
                val suffix = if (sym.isModule || sym.isModuleClass) "$" else ""
                sym.fullName + suffix
            case sym =>
                val separator = if (sym.owner.isModuleClass) "" else "$"
                loop(sym.owner) + separator + sym.javaSimpleName.toString
        }
        loop(symbol)
    }

    // Loads the annotation functions.
    lazy val retrieveFunction = new ParadiseNgFunctionRetriever(
        Reflect.findMacroClassLoader(global.classPath.asURLs,
            global.settings.outputDirs.getSingleOutput))

    /* From annotation defition acquire the function to be applied to the
       annotated scalameta tree. */
    def getAnnotationFunction(annotation: AnnotationInfo):
    scala.meta.Tree => scala.meta.Tree = {
        lazy val withSources = attachSourcesToSymbols(annotation.original)
        val args = annotation.args.map(xs => (xs match {
            case Literal(Constant(v)) => v
            case s : Ident if s.symbol != null && s.symbol != NoSymbol => {
                withSources
                getSource(s.symbol) match {
                    case Some(ValDef(_, _, _, Literal(Constant(v)))) => v
                    case v => { println(showRaw(v)); 15 }
                }
            }
        }).asInstanceOf[AnyRef])
        val cls_name = getClassNameBySymbol(annotation.tpe.typeSymbol)
        retrieveFunction(cls_name, args)
    }

}
