package localhost.plugin.meta
import scala.meta._
import localhost.lib._

/* Given a tree, apply a series of transformations to it. */
class ParadiseNgTransformer(var tree: Tree) {
    import TreeStorage._
    import Transversers._

    private type Payload = (List[Stat], List[Stat])

    private def newStats(stats: List[Stat]): List[Stat] = stats flatMap { s =>
        s.getPayload[Payload] match {
            case None => List(s)
            case Some((mn, arb)) => mn ++ arb
        }
    }

    private def eval(tree: Tree): Tree = tree.transform {
        case o @ Template(a, b, c, stats) => o.copy(stats = newStats(stats))
        case o @ Source(stats)            => o.copy(stats = newStats(stats))
        case o @ Term.Block(stats)        => o.copy(stats = newStats(stats))
        case o @ Pkg(a, stats)            => o.copy(stats = newStats(stats))
    }

    private def getAt(position: Int, t: Tree = tree): Stat = {
        t.findPos[Stat](Position.Range(null, position, position+1),
            _.getPosition()).get
    }

    def modify(fn: TreeTransformation, position: Int): List[Stat] = {
        val tree = getAt(position)
        val result = fn.pluginInterop(eval(tree).asInstanceOf[Stat], None)
        val newTrees = (result._1.map(_._1) ++ result._1.flatMap(_._2),
            result._2)
        tree storePayload newTrees
        newTrees._1 ++ newTrees._2
    }

    def modify(fn: TreeTransformation, position: Int, companionPos: Int) = {
        val tree = getAt(position)
        val compStat = getAt(companionPos, tree.parent.get)
        val compPayload = compStat.getPayload[Payload]
            .getOrElse((List(compStat), Nil))
        val companion = compPayload match {
            case (List(c), _) => c
        }
        val result = fn.pluginInterop(
            eval(tree).asInstanceOf[Stat], Some(companion))
        val newTrees = (result._1.map(_._1), result._2)
        val companions = (result._1.flatMap(_._2), compPayload._2)
        tree storePayload newTrees
        compStat storePayload companions
        (newTrees._1 ++ newTrees._2, companions._1 ++ companions._2)
    }: (List[Stat], List[Stat])
}
