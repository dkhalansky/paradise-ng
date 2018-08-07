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
            val removeAnnots = (m: Defn) =>m.transformMods { mods =>
                mods.zipWithIndex
                    .filter { m => !badIndices.contains(m._2) }
                    .map { m => m._1 }
            }
            val initial = (s: Stat) => (s match {
                case m: Defn => removeAnnots(m)
                case Term.Block(Seq(t1: Defn, t2)) =>
                    Term.Block(List(removeAnnots(t1), t2))
            }): Stat
            (initial /: ans) { (f, a) => (a._1(_)) compose f }
        }
        val nonShadowParent = {
            val nonShadowTree = this.tree.findPos[Stat](
                Position.Range(null, position, position+1),
                t => t.getPosition).get
            nonShadowTree.parent.get
        }
        val tree = getAt(position)
        val companion = companionPos.map(getAt)
        val arg = companion.map(c => Term.Block(List(tree, c))).getOrElse(tree)
        val expanded = fn((arg.transform {
            case s => s.getPayload[Tree]().getOrElse(s)
        }).asInstanceOf[Stat])
        val removeCompanion = () => {
            companion map (c => replaceChild(nonShadowParent, c, List()))
        }
        expanded match {
            case Term.Block(lst @ List(t, c)) => {
                companion match {
                    case None => replaceChild(nonShadowParent, tree, lst)
                    case Some(p) => {
                        tree storePayload t
                        p storePayload c
                    }
                }
            }
            case Term.Block(lst) => {
                replaceChild(nonShadowParent, tree, lst)
                removeCompanion()
            }
            case t => {
                tree storePayload t
                removeCompanion()
            }
        }
    }

    def get(): Tree = eval(tree)
}
