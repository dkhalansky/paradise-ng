package localhost.plugin.meta
import scala.meta._

object Modifiers {
    implicit class XtensionDefModifiers[T <: Defn](val tree: T) extends AnyVal {
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

    implicit class XtensionDeclModifiers[T <: Decl](val tree: T) extends AnyVal {
        def transformMods(fn: List[Mod] => List[Mod]): T = tree match {
            case t: Decl.Type   => t.copy(mods = fn(t.mods)).asInstanceOf[T]
            case t: Decl.Def    => t.copy(mods = fn(t.mods)).asInstanceOf[T]
            case t: Decl.Val    => t.copy(mods = fn(t.mods)).asInstanceOf[T]
        }
    }
}
