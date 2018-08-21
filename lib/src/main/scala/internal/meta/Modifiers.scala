package com.github.dkhalansky.paradiseng.lib.internal.meta
import scala.meta._

object Modifiers {
    implicit class XtensionDefModifiers[T <: Defn](val tree: T) extends AnyVal {
        def transformMods(fn: List[Mod] => List[Mod]): T =
            tree.withMods(fn(tree.mods))

        def mods: List[Mod] = tree match {
            case t: Defn.Class  => t.mods
            case t: Defn.Trait  => t.mods
            case t: Defn.Type   => t.mods
            case t: Defn.Object => t.mods
            case t: Defn.Def    => t.mods
            case t: Defn.Val    => t.mods
            case t: Defn.Var    => t.mods
        }

        def withMods(mods: List[Mod]) = tree match {
            case t: Defn.Class  => t.copy(mods = mods).asInstanceOf[T]
            case t: Defn.Trait  => t.copy(mods = mods).asInstanceOf[T]
            case t: Defn.Type   => t.copy(mods = mods).asInstanceOf[T]
            case t: Defn.Object => t.copy(mods = mods).asInstanceOf[T]
            case t: Defn.Def    => t.copy(mods = mods).asInstanceOf[T]
            case t: Defn.Val    => t.copy(mods = mods).asInstanceOf[T]
            case t: Defn.Var    => t.copy(mods = mods).asInstanceOf[T]
        }
    }

    implicit class XtensionDeclModifiers[T <: Decl](val tree: T) extends AnyVal {
        def transformMods(fn: List[Mod] => List[Mod]): T =
            tree.withMods(fn(tree.mods))

        def mods: List[Mod] = tree match {
            case t: Decl.Type   => t.mods
            case t: Decl.Def    => t.mods
            case t: Decl.Val    => t.mods
        }

        def withMods(mods: List[Mod]) = tree match {
            case t: Decl.Type   => t.copy(mods = mods).asInstanceOf[T]
            case t: Decl.Def    => t.copy(mods = mods).asInstanceOf[T]
            case t: Decl.Val    => t.copy(mods = mods).asInstanceOf[T]
        }
    }
}
