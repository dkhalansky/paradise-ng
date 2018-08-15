package localhost.lib

import scala.annotation.StaticAnnotation
import scala.meta._

trait TreeTransformation {
    def pluginInterop(tree: Stat, companion: Option[Stat]):
        (List[(Stat, Option[Stat])], List[Stat])
}

trait ParadiseNgAnnotation extends StaticAnnotation with TreeTransformation {
    override def pluginInterop(tree: Stat, companion: Option[Stat]) = {
        val arg = companion match {
            case None => tree
            case Some(comp) => q"$tree; $comp"
        }
        apply(arg) match {
            case Term.Block(Seq(tree, comp)) => (List((tree, Some(comp))), Nil)
            case Term.Block(Seq(tree)) => (List((tree, None)), Nil)
            case Term.Block(lst) => {
                def shouldProcess(t: Tree) =
                    t.isInstanceOf[Decl] || t.isInstanceOf[Defn]
                val keep = lst filter shouldProcess map { t => (t, None) }
                val leave = lst filter (!shouldProcess(_))
                (keep, leave)
            }
            case tree => (List((tree, None)), Nil)
        }
    }: (List[(Stat, Option[Stat])], List[Stat])

    def apply(annottee: Stat): Stat
}

class AnnotationCombination(ans: List[(TreeTransformation, Int)])
extends TreeTransformation {
    override def pluginInterop(tree: Stat, comp: Option[Stat]) = {
        import localhost.lib.internal.meta.Modifiers._
        val badIndices = ans map { m => m._2 }
        val removeAnnots = (mods: List[Mod]) =>
            mods.zipWithIndex
                .filter { m => !badIndices.contains(m._2) }
                .map { m => m._1 }
        val initial = (s: Stat, c: Option[Stat]) => { s match {
            case m: Defn => (List((m transformMods removeAnnots, c)), Nil)
            case m: Decl => (List((m transformMods removeAnnots, c)), Nil)
        }}: (List[(Stat, Option[Stat])], List[Stat])
        val fn = (initial /: ans) { (f, a) => (tree, comp) =>
        f(tree, comp) match {
            case (ltv, lst) => {
                val inr = Nil.asInstanceOf[List[(Stat, Option[Stat])]]
                ltv.map(t => a._1.pluginInterop(t._1, t._2))
                   .foldRight((inr, lst))((tp, ac) =>
                       (tp._1 ++ ac._1, tp._2 ++ ac._2))
            }
        }}
        fn(tree, comp)
    }
}
