package test.macros
import scala.meta._
import com.github.dkhalansky.paradiseng.lib._

object AppendToObject {
    def apply(obj: Defn.Object, stat: Stat): Defn.Object = {
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
                Term.Block(List(cls, AppendToObject(obj, bar)))
            }
            case cls: Defn.Class => {
                val Type.Name(name) = cls.name
                Term.Block(List(cls, q"""object ${Term.Name(name)} { $bar }"""))
            }
        }
    }
}

class AppendFooToObject extends ParadiseNgAnnotation {
    override def apply(annottee: Stat): Stat = {
        val foo = q"def foo() = 43"
        annottee match {
            case c: Defn.Object => AppendToObject(c, foo)
        }
    }
}

class ReplaceCompanion extends ParadiseNgAnnotation {
    override def apply(annottee: Stat): Stat = {
        val foo = q"def foo() = 43"
        annottee match {
            case Term.Block(Seq(m: Defn, c: Defn.Object)) => {
                Term.Block(List(m, q"object ${c.name} { def foo() = 1556 }"))
            }
            case _ => annottee
        }
    }
}

class FailIfCompanionExists extends ParadiseNgAnnotation {
    override def apply(annottee: Stat): Stat = {
        annottee match {
            case c: Term.Block => None.get
            case _ => annottee
        }
    }
}
