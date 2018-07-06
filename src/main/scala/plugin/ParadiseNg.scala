package localhost.plugin
import localhost.lib._

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.Parsing
import nsc.typechecker.Namers
import nsc.typechecker.Typers
import nsc.reporters.StoreReporter
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.transform.Transform
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.OffsetPosition
import scala.reflect.internal

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
                val metatree = ScalametaParser.fromFile(
                    unit.body.pos.source.path)
                val tr = new ScalametaTransformer(metatree)
                runOnOurAnnotees(typed) {
                    (md, an) => {
                        tr.modify(md.pos.start-1, getAnnotationFunction(an))
                    }
                }
                unit.body = newUnitParser(tr.storage.toString()).parse()
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
    def runOnOurAnnotees(tree: Tree)(f: (MemberDef, AnnotationInfo) => Unit) {
        object annoteesTraverser extends Traverser {
            override def traverse(tree: Tree) {
                super.traverse(tree)
                tree match {
                    case md: MemberDef if tree.symbol != null => {
                        for (an <- tree.symbol.annotations.reverse)
                        if (isOurTypedAnnotation(an)) {
                            f(md, an)
                        }
                    }
                    case _ =>
                }
            }
        }
        annoteesTraverser.traverse(tree)
    }

    /* This class is a workaround for Namer always messing up the root context.

       The original Namer, it turns out, modifies two scopes: the scope that is
       provided to it--and the root scope. The root scope is modified when the
       namer encounters a package declaration.

       Therefore, it isn't possible to run the original namer on an arbitrary
       file and still be able to keep the global state intact, no matter how one
       configures the namer. */
    class LocalNamer(context: analyzer.Context) extends analyzer.Namer(context) {
        // Mostly a copy-paste from the original Namer.
        // scala/src/compiler/scala/tools/nsc/typechecker/Namers.scala
        override def createPackageSymbol(pos: Position, pid: RefTree): Symbol = {
            val pkgOwner = pid match {
              case Ident(_)                 => if (owner.isEmptyPackageClass) rootMirror.RootClass else owner
              case Select(qual: RefTree, _) => createPackageSymbol(pos, qual).moduleClass
            }
            val existing = pkgOwner.info.decls.lookup(pid.name)

            if (existing.hasPackageFlag && pkgOwner == existing.owner)
              existing
            else {
              val pkg          = pkgOwner.newPackage(pid.name.toTermName, pos)
              val pkgClass     = pkg.moduleClass
              val pkgClassInfo = new PackageClassInfoType(newPackageScope(pkgClass), pkgClass)

              pkgClass setInfo pkgClassInfo
              pkg setInfo pkgClass.tpe
              pkg /* The difference from the original Namer is here: we don't
                     store the package symbol in the root context but simply
                     return the newly-created package symbol. We do it just so
                     the types check out.
                  */
            }
        }
    }

    /* Assign types to the tree as comprehensively as possible. */
    def getPreliminarilyTyped(unit: CompilationUnit): Tree = {
        val reporter = global.reporter
        global.reporter = new StoreReporter()
        var result = unit.body
        try {
            val tree = unit.body.duplicate
            val context = analyzer.rootContext(unit).
                make(tree, scope = newScope).
                makeSilent(false)
            val namer = new LocalNamer(context)
            val ct = namer.enterSym(tree)
            val typer = analyzer.newTyper(ct)
            result = typer.typed(tree)
        } finally {
            global.reporter = reporter
        }
        result
    }

    def getAnnotationFunction(annotation: AnnotationInfo)(tree: scala.meta.Tree) = {
        import scala.language.reflectiveCalls
        val cls_name = annotation.tpe.typeSymbol.fullName
        val classloader = {
            import scala.reflect.internal.util.ScalaClassLoader
            val classpath = global.classPath.asURLs
            ScalaClassLoader.fromURLs(classpath,
                tree.getClass().getClassLoader())
        }
        val cls = Class.forName(cls_name, true, classloader)
        val inst = cls.newInstance.asInstanceOf[{
            def apply(annottee: scala.meta.Tree): scala.meta.Tree
        }]
        inst.apply(tree)
    } : scala.meta.Tree

}
