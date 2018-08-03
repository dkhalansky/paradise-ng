package localhost.plugin.meta
import scala.meta._

object Modifiers {
    implicit class XtensionModifiers[T <: Defn](val tree: T) extends AnyVal {
        def transformMods(fn: List[Mod] => List[Mod]): T = tree match {
            case t: Defn.Class  => t.copy(mods = fn(t.mods)).asInstanceOf[T]
            case t: Defn.Trait  => t.copy(mods = fn(t.mods)).asInstanceOf[T]
            case t: Defn.Type   => t.copy(mods = fn(t.mods)).asInstanceOf[T]
            case t: Defn.Object => t.copy(mods = fn(t.mods)).asInstanceOf[T]
            case t: Defn.Def    => t.copy(mods = fn(t.mods)).asInstanceOf[T]
            case t: Defn.Val    => t.copy(mods = fn(t.mods)).asInstanceOf[T]
            case t: Defn.Var    => t.copy(mods = fn(t.mods)).asInstanceOf[T]
        }
    }
}
