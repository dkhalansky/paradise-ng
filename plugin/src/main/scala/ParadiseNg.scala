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
                    new ScalametaTransformer(metatree)
                }
                for ((md, ans) <- ourAnnottees(typed).reverse) {
                    for (an <- ans) {
                        tr.modify(md.pos.start-1, getAnnotationFunction(an))
                    }
                    didApplyAnnotations = true
                }
                if (didApplyAnnotations) {
                    unit.body = newUnitParser(tr.storage.toString()).parse()
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

    /* Run an action only on those subtrees that are annotated using
       ParadiseNg's annotations. */
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

    def getAnnotationFunction(annotation: AnnotationInfo)(tree: scala.meta.Tree) = {
        import scala.language.reflectiveCalls
        val args = annotation.args.map(xs => xs match {
            case Literal(Constant(v)) => v.asInstanceOf[AnyRef]
        })
        val arg_types = args.map(xs => xs.getClass())
        val cls_name = annotation.tpe.typeSymbol.fullName
        val classloader = {
            import scala.reflect.internal.util.ScalaClassLoader
            val classpath = global.classPath.asURLs
            ScalaClassLoader.fromURLs(classpath,
                tree.getClass().getClassLoader())
        }
        val cls = Class.forName(cls_name, true, classloader)
        val inst = cls.getConstructor(arg_types.toArray: _*)
            .newInstance(args.toArray: _*)
            .asInstanceOf[{
                def apply(annottee: scala.meta.Tree): scala.meta.Tree
            }]
        inst.apply(tree)
    } : scala.meta.Tree

}
