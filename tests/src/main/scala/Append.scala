package test.macros
import scala.meta._
import localhost.lib._

class AppendAAnnotation extends ParadiseNgAnnotation {
    def apply(annottee: Tree): Tree = {
        AppendHelpers.appendStat(annottee, "letters += 'a'")
    }
}

class AppendBAnnotation extends ParadiseNgAnnotation {
    def apply(annottee: Tree): Tree = {
        AppendHelpers.appendStat(annottee, "letters += 'b'")
    }
}

class AppendCAnnotation extends ParadiseNgAnnotation {
    def apply(annottee: Tree): Tree = {
        AppendHelpers.appendStat(annottee, "letters += 'c'")
    }
}

object AppendHelpers {
  def appendStat(defn: Tree, stat: String) = {
    val parsedStat = stat.parse[Stat].get

    defn match {
      case q"""..$mods def $name[..$tparams](...$paramss): $tpeopt = {
                 ..${ stats: scala.collection.immutable.Seq[Stat] }
               }""" =>
        q"..$mods def $name[..$tparams](...$paramss): $tpeopt = { ..$stats;  $parsedStat}"
      case q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" =>
        q"..$mods def $name[..$tparams](...$paramss): $tpeopt = { $expr; $parsedStat  }"
    }

  }
}
