package localhost.plugin

trait SymbolSources { self: ParadiseNgComponent =>
    import global._

    case class SymbolSourceAttachment(source: Tree)

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

        def source: Option[Tree] = {
            if (symbol == null || symbol == NoSymbol) {
                None
            } else if (symbol.isModuleClass) {
                symbol.sourceModule.source
            } else {
                symbol.attachments
                  .get[SymbolSourceAttachment]
                  .map(att => att.source)
            }
        }

    }
}
