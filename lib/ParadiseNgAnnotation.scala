package localhost.lib

import scala.annotation.StaticAnnotation
import scala.meta._

trait ParadiseNgAnnotation extends StaticAnnotation {
    def pluginInterop(tree: Stat, companion: Option[Stat]) = {
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
