package localhost.plugin.meta
import scala.meta._

/* Given a tree, apply a series of transformations to it. */
class ParadiseNgTransformer(var tree: Tree) {
    import TreeStorage._
    import Transversers._

    private def contains(p: Position, i: Int) : Boolean = {
        p.start <= i && p.end > i
    }

    private def getAt(position: Int) : Stat = {
        tree.findPos[Stat](Position.Range(null, position, position+1),
            t => t.getPosition()).get
    }

    private def replaceChild(
        child: Stat,
        newchildren: List[Stat]
    ) : Tree = {
        def newStats(old: List[Stat]) : List[Stat] = old.flatMap {
            case v if v.equals(child) => newchildren
            case v => List(v)
        }
        val parent = child.parent.get
        parent.getPayload[Tree]().getOrElse(parent) match {
            case o @ Template(_, _, _, stats) => o.copy(stats = newStats(stats))
            case o @ Source(stats)            => o.copy(stats = newStats(stats))
            case o @ Term.Block(stats)        => o.copy(stats = newStats(stats))
            case o @ Pkg(_, stats)            => o.copy(stats = newStats(stats))
        }
    }

    def modify(position: Int, companionPos: Option[Int], fn: Stat => Stat) {
        val tree = getAt(position)
        val companion = companionPos.map(getAt)
        val arg = companion.map(c => Term.Block(List(tree, c))).getOrElse(tree)
        val expanded = fn((arg.transform {
            case s => s.getPayload[Tree]().getOrElse(s)
        }).asInstanceOf[Stat])
        expanded match {
            case Term.Block(lst @ List(t, c)) => {
                tree storePayload t
                companion match {
                    case None => {
                        val parent = tree.parent.get
                        parent storePayload replaceChild(tree, lst)
                    }
                    case Some(p) => p storePayload c
                }
            }
            case t => {
                tree storePayload t
                companion match {
                    case None =>
                    case Some(p) => {
                        val parent = p.parent.get
                        parent storePayload replaceChild(p, List())
                    }
                }
            }
        }
    }

    def get(): Tree = {
        tree.transform {
            case s => s.getPayload[Tree]().getOrElse(s)
        }
    }
}
