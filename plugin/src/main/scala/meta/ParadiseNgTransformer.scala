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

    def modify(position: Int, companionPos: Option[Int], fn: TreeTransformation)
    {
        val nonShadowParent = {
            val nonShadowTree = this.tree.findPos[Stat](
                Position.Range(null, position, position+1),
                t => t.getPosition).get
            nonShadowTree.parent.get
        }
        val tree = getAt(position)
        val companion = companionPos.map(getAt)
        val expanded = fn.pluginInterop(eval(tree).asInstanceOf[Stat],
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
