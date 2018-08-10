package localhost.plugin

/*  This trait attaches to symbols the source code where they were defined. */
trait SymbolSources { self: ParadiseNgComponent =>
    import global._

    private case class SymbolSourceAttachment(source: Tree)

    /*  Given a typed tree, for each definition attach to the corresponding
        symbol the defining subtree. */
    def attachSourcesToSymbols(tree: Tree) {
        (new Traverser {
            override def traverse(tree: Tree) {
                if (tree.symbol != null && tree.symbol != NoSymbol && (
                    tree.isInstanceOf[MemberDef] || tree.symbol.isLocalDummy
                )) {
                    tree.symbol.updateAttachment(SymbolSourceAttachment(tree))
                }
                super.traverse(tree)
            }
        }).traverse(tree)
    }

    implicit class XtensionSymbolSource(symbol: Symbol) {

        /*  Try acquiring the source code attached to the symbol. */
        def source: Option[Tree] = {
            if (symbol == null || symbol == NoSymbol) {
                None
            } else if (symbol.isModuleClass) {
                /*  Definitions can have two corresponding symbols, with
                    the `sourceModule` being the one that we attached the
                    source code to, and `moduleClass` the one that is
                    referenced as the owner of the child symbols. */
                symbol.sourceModule.source
            } else {
                symbol.attachments
                  .get[SymbolSourceAttachment]
                  .map(att => att.source)
            }
        }

    }
}
