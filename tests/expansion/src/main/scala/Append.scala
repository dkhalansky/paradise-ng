package test.macros
import scala.meta._
import com.github.dkhalansky.paradiseng.lib._

class AppendAAnnotation extends ParadiseNgAnnotation {
    def apply(annottee: Stat): Stat = {
        AppendHelpers.appendStat(annottee, "letters += 'a'")
    }
}

class AppendBAnnotation extends ParadiseNgAnnotation {
    def apply(annottee: Stat): Stat = {
        AppendHelpers.appendStat(annottee, "letters += 'b'")
    }
}

class AppendCAnnotation extends ParadiseNgAnnotation {
    def apply(annottee: Stat): Stat = {
        AppendHelpers.appendStat(annottee, "letters += 'c'")
    }
}

object AppendHelpers {
  def appendStat(defn: Stat, stat: String) = {
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
