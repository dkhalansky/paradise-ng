package localhost.plugin

trait Companions { self: ParadiseNgComponent =>
    import global._

    /* Get the tree corresponding to the companion object of the member,
       if any. */
    def getCompanionTree(symbol: Symbol) : Option[ModuleDef] = {
        // If the compiler itself can point us to a companion, we trust it.
        symbol.companion.source match {
            case Some(m) => return Some(m.asInstanceOf[ModuleDef])
            case _ =>
        }

        /* If the compiler didn't find the companion object, it can mean one
           of two things: either there really is no companion object or
           the compiler is mistaken.

           According to the definition of and comments for
           `def companionSymbolOf` from `Namers.scala` in scalac sources, the
           compiler can be trusted iff the member alongside its companion
           object is *not* defined in a code block. Class and package
           definitions are not considered code blocks, they are called
           "templates". So, for

               {
                   class A
                   object A
               }

           and

               def foo() {
                   class A
                   object A
               }

           lookup fails, while it succeeds for, say,

               class B {
                   class A
                   object A
               }

           So, if the compiler has told us that it knows nothing about a
           companion object, we trust it if the member was not defined in a
           code block. */
        val owner = symbol.owner
        if (!owner.isTerm && owner.hasCompleteInfo && !symbol.isType) {
            return None
        }

        // We try to find the real parent tree of the original one.
        var parent = null.asInstanceOf[Tree]
        object parentFinder extends Traverser {
            var ourTreeIsChild = false
            override def traverse(tree: Tree) {
                if (ourTreeIsChild) {
                    return
                } else if (tree.symbol == symbol && tree.isInstanceOf[MemberDef]) {
                    ourTreeIsChild = true;
                } else {
                    super.traverse(tree)
                    if (ourTreeIsChild) {
                        ourTreeIsChild = false
                        parent = tree
                    }
                }
            }
        }
        parentFinder(owner.source.get)

        // We try to find among the children of our parent a companion object.
        parent.children.find(m => m.isInstanceOf[ModuleDef] &&
            m.symbol != null && m.symbol.name == symbol.name.companionName &&
            m.symbol.isCoDefinedWith(symbol)).map(m => m.asInstanceOf[ModuleDef])
    }

}
