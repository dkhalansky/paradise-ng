package localhost.plugin.meta
import scala.meta._
import localhost.lib._

/* Given a tree, apply a series of transformations to it. */
class ParadiseNgTransformer(var tree: Tree) {
    import TreeStorage._
    import Transversers._

    private def contains(p: Position, i: Int) : Boolean = {
        p.start <= i && p.end > i
    }

    private def getAt(position: Int) : Stat = {
        val f = (u: Stat) => return u;
        (new Traverser {
            override def apply(tree: Tree) {
                if (contains(tree.getPosition(), position)) {
                    super.apply(tree.getPayload[Tree]().getOrElse(tree))
                    tree match {
                        case u: Stat => f(u)
                        case _ =>
                    }
                }
            }
        })(tree)
        None.get
    }

    private def replaceChild(parent: Tree, child: Stat, newchildren: List[Stat])
    = {
        def newStats(old: List[Stat]) : List[Stat] = old.flatMap {
            case v if v.equals(child) => newchildren
            case v => List(v)
        }
        val newParent = parent.getPayload[Tree]().getOrElse(parent) match {
            case o @ Template(a, b, c, stats) => o.copy(stats = newStats(stats))
            case o @ Source(stats)            => o.copy(stats = newStats(stats))
            case o @ Term.Block(stats)        => o.copy(stats = newStats(stats))
            case o @ Pkg(a, stats)            => o.copy(stats = newStats(stats))
        }
        parent storePayload newParent
    }

    private def eval(tree: Tree) = tree.transform {
        case s => s.getPayload[Tree]().getOrElse(s)
    }

    def modify(position: Int, companionPos: Option[Int],
        ans: List[(ParadiseNgAnnotation, Int)])
    {
        val fn = {
            import Modifiers._
            val badIndices = ans map { m => m._2 }
            val removeAnnots = (mods: List[Mod]) =>
                mods.zipWithIndex
                    .filter { m => !badIndices.contains(m._2) }
                    .map { m => m._1 }
            val initial = (s: Stat, c: Option[Stat]) => { s match {
                case m: Defn => (List((m transformMods removeAnnots, c)), Nil)
                case m: Decl => (List((m transformMods removeAnnots, c)), Nil)
            }}: (List[(Stat, Option[Stat])], List[Stat])
            (initial /: ans) { (f, a) => (tree, comp) => f(tree, comp) match {
                case (ltv, lst) => {
                    val inr = Nil.asInstanceOf[List[(Stat, Option[Stat])]]
                    ltv.map(t => a._1.pluginInterop(t._1, t._2))
                       .foldRight((inr, lst))((tp, ac) =>
                           (tp._1 ++ ac._1, tp._2 ++ ac._2))
                }
            }}
        }
        val nonShadowParent = {
            val nonShadowTree = this.tree.findPos[Stat](
                Position.Range(null, position, position+1),
                t => t.getPosition).get
            nonShadowTree.parent.get
        }
        val tree = getAt(position)
        val companion = companionPos.map(getAt)
        val expanded = fn(eval(tree).asInstanceOf[Stat],
            companion.map(c => eval(c).asInstanceOf[Stat]))
        val newTrees = (expanded._1.map(_._1)
            ++ expanded._1.flatMap(_._2)
            ++ expanded._2)
        companion map { c => c storePayload (q"{}") }
        newTrees match {
            case List(t) => tree storePayload t
            case lst => replaceChild(nonShadowParent, tree, lst)
        }
    }

    def get(): Tree = eval(tree)
}
