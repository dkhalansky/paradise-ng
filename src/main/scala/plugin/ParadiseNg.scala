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
        val cls_name = annotation.tpe.typeSymbol.fullName
        val classloader = {
            val m_findMacroClassLoader =
                analyzer.getClass.getMethods().find(_.getName == "findMacroClassLoader").get
            m_findMacroClassLoader.setAccessible(true)
            m_findMacroClassLoader.invoke(analyzer).asInstanceOf[ClassLoader]
        }
        // val cls = Class.forName(cls_name, true, classloader)
        val cls = Class.forName(cls_name)
        val inst = cls.newInstance.asInstanceOf[{
            def apply(annottee: scala.meta.Tree): scala.meta.Tree
        }]
        inst.apply(tree)
    } : scala.meta.Tree

    class FooTransformer(path: String) extends Transformer {
        val metaTree = new ScalametaSourceExtractor(
            ScalametaParser.fromFile(path))

        override def transform(tree: Tree): Tree = {
            val ntree = super.transform(tree)
            ourAnnotations(ntree) match {
                case Nil => ntree
                case lst => (ntree /: lst)(
                    (t, a) => applyAnnotation(t, a))
            }
        }

        /* Given a tree and an annotation, apply the transforms. */
        def applyAnnotation(t : Tree, annotation : Tree) : Tree = {
            val metatree = metaTree.findAtPos(t.pos.start)
            val cls = Class.forName(annotation.tpe.typeSymbol.fullName).
                newInstance.asInstanceOf[{
                    def apply(annottee: scala.meta.Tree): scala.meta.Tree
                }]
            val newtree = cls.apply(metatree)
            val newtext = newtree.toString()
            buildReplacementTree(t.pos, newtext, getMods(t).get)
        }
    }

    /* Build a tree that can go in the position specified in its
       argument and represents the provided source code. */
    def buildReplacementTree(pos: Position, code: String, mods: Modifiers): Tree = {
        adaptTree(pos, setMods(newUnitParser(code).parseStats()(0), mods))
    }

    /* Adapt the tree to inserting it into the larger tree on the defined
       position. */
    def adaptTree(pos: Position, tree: Tree): Tree = {
        val newPos = new OffsetPosition(pos.source, pos.start)
        object positionSetter extends Transformer {
            override def transform(tree: Tree): Tree = {
                tree.pos = newPos
                super.transform(tree)
            }
        }
        /* If we just returned `tree`, then compiler would fail when using
           the flag -Yrangepos. The reason is, the flag has a side effect:
           besides providing the detaled information on the positions of
           trees, it also forces the compiler to recheck that the tree
           ranges are consistent. The new code isn't and in general can't
           be aligned to the old range, so we set the positions of all the
           trees that we insert to a set value inside the allowed range. */
        positionSetter.transform(tree)
    }

    /* Given a position, return a string to which this position points */
    def readCodeBlock(pos: Position) : String = {
        val path = pos.source.path
        val source = scala.io.Source.fromFile(path)
        val text = try source.mkString finally source.close()
        text.substring(pos.start, pos.end)
    }

    /* Given a tree, returns the annotations of the tree that are recognised by
       the plugin. */
    def ourAnnotations(t: Tree) : List[Tree] = {
        getMods(t) match {
            case None => Nil
            case Some(mods) => mods.annotations.map(annotationType).flatten
        }
    }

    /* Determine the type of the annotation in case it belongs to this plugin.
       If it does, return the typed tree. If it doesn't, return `None`. */
    def annotationType(annotation: Tree): Option[Tree] = {
        val annotPackage = rootMirror.getPackage(
            TermName("localhost.lib"))
        val annotPackageImport = gen.mkWildcardImport(annotPackage)
        val context = analyzer.NoContext.
            make(EmptyTree, rootMirror.RootClass, newScope).
            makeSilent(false, annotPackageImport)
        val typer = analyzer.newTyper(context)
        val typed = typer.typed(annotation.duplicate)
        if (!typed.tpe.isError && typed.tpe <:< typeOf[ParadiseNgAnnotation]) {
            val meth = typed.tpe.member(TermName("apply"))
            if (meth.isMethod) {
                Some(typed)
            } else {
                None
            }
        } else {
            None
        }
    }

    /* Utility function to get the modifiers of a tree if they exist. */
    def getMods(tree: Tree): Option[Modifiers] = {
        tree match {
            case ClassDef(mods, _, _, _)     => Some(mods)
            case DefDef(mods, _, _, _, _, _) => Some(mods)
            case ModuleDef(mods, _, _)       => Some(mods)
            case TypeDef(mods, _, _, _)      => Some(mods)
            case ValDef(mods, _, _, _)       => Some(mods)
            case _                           => None
        }
    }

    /* Utility function to replace the existing modifiers with new ones. */
    def setMods(tree: Tree, mods: Modifiers) : Tree = {
        tree match {
            case ClassDef (_, a, b, c)       => ClassDef (mods, a, b, c)
            case DefDef   (_, a, b, c, d, e) => DefDef   (mods, a, b, c, d, e)
            case ModuleDef(_, a, b)          => ModuleDef(mods, a, b)
            case TypeDef  (_, a, b, c)       => TypeDef  (mods, a, b, c)
            case ValDef   (_, a, b, c)       => ValDef   (mods, a, b, c)
            case t                           => t
        }
    }
}
