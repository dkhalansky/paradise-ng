package com.github.dkhalansky.paradiseng.lib

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
    import com.github.dkhalansky.paradiseng.lib.internal.meta.Modifiers._

    private def splitAtMultiple[T](vals: List[T], ixs: List[Int]) = {
        val r = ((vals, List.empty[List[T]], -1) /: ixs) { (ac, ix) =>
            (ac._1.drop(ix-ac._3), ac._1.take(ix-ac._3-1) :: ac._2, ix)
        }
        (r._1 :: r._2).reverse
    } : List[List[T]]

    override def pluginInterop(tree: Stat, companion: Option[Stat]) = {
        val (annotsWithMods, initialTree) = tree match {
            case t: Defn => {
                val mods = splitAtMultiple(t.mods, ans map (_._2))
                (ans map (_._1) zip mods, t withMods mods.last)
            }
            case t: Decl => {
                val mods = splitAtMultiple(t.mods, ans map (_._2))
                (ans map (_._1) zip mods, t withMods mods.last)
            }
        }

        val ac = (List((initialTree, companion)), List.empty[Stat])
        (annotsWithMods :\ ac) { (anWithMods, trees) => {
            (trees, anWithMods) match {
                case ((toExpand, notToExpand), (an, mods)) => {
                    val ac1 = List.empty[(Stat, Option[Stat])]
                    toExpand
                        .map(t => an.pluginInterop(t._1, t._2))
                        .foldRight((ac1, notToExpand)) { (next, ac) =>
                            val next1WithMods = if (mods.isEmpty) {
                                next._1
                            } else next._1 map { v => v._1 match {
                                case t: Defn =>
                                    (t.transformMods(x => mods ++ x), v._2)
                                case t: Decl =>
                                    (t.transformMods(x => mods ++ x), v._2)
                            }}
                            (next1WithMods ++ ac._1, next._2 ++ ac._2)
                        }
                }
            }
        }}
    }: (List[(Stat, Option[Stat])], List[Stat])
}
