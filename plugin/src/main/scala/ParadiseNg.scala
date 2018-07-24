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
                attachSourcesToSymbols(typed)
                var didApplyAnnotations = false
                lazy val tr = {
                    val metatree = ScalametaParser.create(
                        unit.source.content)
                    new ScalametaTransformer(metatree)
                }
                for ((md, ans) <- ourAnnottees(typed).reverse) {
                    val companionPos = getCompanionTree(md)
                        .map(t => t.pos.start-1)
                    val start : scala.meta.Stat => scala.meta.Stat = m => m
                    var fn = (start /: ans) {
                        (f, an) => getAnnotationFunction(an) compose f
                    }
                    tr.modify(md.pos.start-1, companionPos, fn)
                    didApplyAnnotations = true
                }
                if (didApplyAnnotations) {
                    unit.body = newUnitParser(tr.get().toString()).parse()
                }
            }
        }
        phase
    }

    /* Get the tree corresponding to the companion object of the member,
       if any. */
    def getCompanionTree(original: MemberDef) : Option[ModuleDef] = {
        // If the compiler itself can point us to a companion, we trust it.
        getSource(original.symbol.companion) match {
            case Some(m) => return Some(m.asInstanceOf[ModuleDef])
            case _ =>
        }

        /* If the compiler didn't find the companion object, it can mean one
           of two things: either there really is no companion object or
           the compiler is mistaken.

           According to the definition of and comments for
           `def companionSymbolOf` from `Namers.scala` in scalac sources, the
           compiler can be trusted iff the member alongside its companion
           object is *not* defined in a code block. Class and package
           definitions are not considered code blocks, they are called
           "templates". So, for

               {
                   class A
                   object A
               }

           and

               def foo() {
                   class A
                   object A
               }

           lookup fails, while it succeeds for, say,

               class B {
                   class A
                   object A
               }

           So, if the compiler has told us that it knows nothing about a
           companion object, we trust it if the member was not defined in a
           code block. */
        val owner = original.symbol.owner
        if (!owner.isTerm && owner.hasCompleteInfo) {
            return None
        }

        /* We take the source code of the parent and try and traverse its tree
           looking for something that looks like our companion object to us. */

        val parent = getSource(owner) match {
            case Some(p) => p
            case _ => return None
        }

        var result = None.asInstanceOf[Option[ModuleDef]]

        object traverser extends Traverser {
            override def traverse(tree: Tree) {
                tree match {
                    case m : ModuleDef if m.symbol != null &&
                        m.symbol.name == original.symbol.name.companionName &&
                        m.symbol.isCoDefinedWith(original.symbol) => {
                            result = Some(m)
                        }
                    case _ => super.traverse(tree)
                }
            }
        }
        traverser.traverse(parent)

        result
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
                if (tree.symbol != null && tree.symbol != NoSymbol && (
                    tree.isInstanceOf[MemberDef] || tree.symbol.isLocalDummy
                )) {
                    tree.symbol.updateAttachment(SymbolSourceAttachment(tree))
                }
                super.traverse(tree)
            }
        }).traverse(tree)
    }

    def getSource(symbol: Symbol): Option[Tree] = {
        if (symbol == null || symbol == NoSymbol) {
            None
        } else if (symbol.isModuleClass) {
            getSource(symbol.sourceModule)
        } else {
            symbol.attachments
              .get[SymbolSourceAttachment]
              .map(att => att.source)
        }
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

    def resolveConstant(tree: Tree, maxDepth: Int = 50): Option[AnyRef] = {
        if (maxDepth <= 0) {
            None
        } else tree match {
            case Literal(Constant(v)) => Some(v.asInstanceOf[AnyRef])
            case s if s.symbol != null && s.symbol != NoSymbol => {
                getSource(s.symbol) match {
                    case Some(ValDef(_, _, _, v)) => {
                        resolveConstant(v, maxDepth - 1)
                    }
                    case _ => None
                }
            }
            case _ => None
        }
    }

    /* From annotation defition acquire the function to be applied to the
       annotated scalameta tree. */
    def getAnnotationFunction(annotation: AnnotationInfo):
    scala.meta.Stat => scala.meta.Stat = {
        val withSources = attachSourcesToSymbols(annotation.original)
        val args = annotation.args.map(xs => resolveConstant(xs).get)
        val cls_name = getClassNameBySymbol(annotation.tpe.typeSymbol)
        retrieveFunction(cls_name, args)
    }

}
