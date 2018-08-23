package com.github.dkhalansky.paradiseng.lib

import scala.annotation.StaticAnnotation
import scala.meta._

/** Defines operations on scalameta trees. */
trait TreeTransformation {
    /** Get the results of the transformation.
    *
    * This method is the entry point for the compiler plugin for cases
    * where the tree to be transformed is a statement.
    *
    * @param tree The statement to be transformed.
    *
    * @param companion The companion of `tree`, if any.
    *
    * @return A pair of a list of trees and their alleged companions for
    *     downstream transformations and a list of new children of the
    *     initial tree's parents that aren't subject to the downstream
    *     transformations.
    */
    def pluginInterop(tree: Stat, companion: Option[Stat]):
        (List[(Stat, Option[Stat])], List[Stat])
}

/** Wraps `TreeTransformation` to provide more familiar access for existing
* users of `paradise`.
*/
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

    /** Apply the transformations.
    *
    * This method provides a simplified interface for interacting with the
    * compiler plugin.
    *
    * @param annottee If the annotated tree doesn't have a companion object,
    *     this parameter will only contain the said tree. If, on the other
    *     hand, it has a companion, this parameter will be a block of the
    *     form `q"{ $tree; $companion }"` where `$tree` is the annotated tree
    *     and `$companion` is its companion object.
    *
    * @return If a statement that is not a block is returned, the returned tree
    *     replaces the old one; if it had a companion object, it is erased from
    *     existence. If a block is returned and it consists of two trees, the
    *     second one will be considered to be the companion of the first one and
    *     will replace the existing companion object or create a new one.
    *     Otherwise, each tree from the returned block is considered an
    *     independent statement: no tree is thought to be the companion of
    *     another tree.
    */
    def apply(annottee: Stat): Stat
}

/** Combine multiple tree-transforming annotations.
*
* This class gets instantiated by the compiler plugin to combine multiple
* annotations acting on the same statement into a single transforming
* function.
*
* As defined here, annotations are combined inner-to-outer, in accordance to
* this mental model (here `y` are macro annotations):
*
* `@x1 @x2 (@y1 (@x3 @x4 (@y2 (@x5 class B))))`
*
* So, first `@x5 class B` is expanded with `@y2`, then to each tree of the
* result that is eligible for further transformations `@x3 @x4` are
* prepended, then each resulting tree is independently expanded with `@y1`,
* and then to each resulting tree eligible for transformations `@x1 @x2` are
* prepended.
*
* @param ans A list of tree transformations associated with annotations and
*     the indices of the annotations. The annotations are enumerated starting
*     from zero, up to down. The list should be sorted by the second field.
*/
class AnnotationCombination(ans: List[(TreeTransformation, Int)])
extends TreeTransformation {
    import com.github.dkhalansky.paradiseng.lib.internal.meta.Modifiers._

    /** Splits the `vals` list at indices from `ixs`. */
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
