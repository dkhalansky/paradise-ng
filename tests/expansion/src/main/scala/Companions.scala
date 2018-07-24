package test.macros
import scala.meta._
import localhost.lib._

object AppendToObject {
    def append(obj: Defn.Object, stat: Stat): Defn.Object = {
        obj match {
            case q"""
                ..$mods object $tname extends { ..$earlyStats }
                with ..$ctorcalls { $selfParam => ..$stats }
            """ => q"""
                ..$mods object $tname extends { ..$earlyStats }
                with ..$ctorcalls { $selfParam => ..$stats; $stat }
            """
        }
    }
}

class AppendBarToCompanion extends ParadiseNgAnnotation {

    override def apply(annottee: Stat): Stat = {
        val bar = q"def bar() = 42"
        annottee match {
            case Term.Block(Seq(cls: Defn.Class, obj: Defn.Object)) => {
                Term.Block(List(cls, AppendToObject.append(obj, bar)))
            }
            case cls: Defn.Class => {
                val Type.Name(name) = cls.name
                Term.Block(List(cls, q"""object ${Term.Name(name)} { $bar }"""))
            }
        }
    }
}

