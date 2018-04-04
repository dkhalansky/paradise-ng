package localhost.plugin
import localhost.lib._

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.Parsing
import nsc.reporters.StoreReporter
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.transform.Transform
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.OffsetPosition

import scala.meta._

class ParadiseNg(val global: Global) extends Plugin {
    import global._
    val name = "paradise-ng"
    val description = "Manipulate ASTs"
    val components = new ParadiseNgComponent(this, global) :: Nil
}

class ParadiseNgComponent(plugin: Plugin, val global: Global)
extends PluginComponent with Transform {
    import global._

    override val phaseName  = "paradise-ng"
    override val runsAfter  = "parser" :: Nil
    override val runsBefore = "namer"  :: Nil

    override def newTransformer(unit: CompilationUnit): Transformer = {
        object fooTransformer extends Transformer {
            override def transform(tree: Tree): Tree = {
                val ntree = super.transform(tree)
                ourAnnotations(ntree) match {
                    case Nil => ntree
                    case lst => (ntree /: lst)(
                        (t, a) => applyAnnotation(t, a))
                }
            }
        }
        fooTransformer
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

    /* Given a tree and the annotation that we recognise, apply the transforms
       inherent to the annotation. */
    def applyAnnotation(t : Tree, annotation : Tree) : Tree = {
        val source = readCodeBlock(t.pos)
        val metatree = source.parse[Stat].get
        val cls = Class.forName(annotation.tpe.typeSymbol.fullName).
            newInstance.asInstanceOf[{
                def apply(annottee: scala.meta.Tree): scala.meta.Tree
            }]
        val newtree = cls.apply(metatree)
        val newtext = newtree.toString()
        buildReplacementTree(t.pos, newtext, getMods(t).get)
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
