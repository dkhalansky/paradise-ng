package localhost.plugin
import scala.meta._

trait TypeParamsDesugar { self: ParadiseNgComponent =>

    import global._

    private val fakeIdent = "fake) "

    private def hideTypeParam(tree: Tree): Tree = {
        val str = fakeIdent + tree.toString
        Apply(Select(Ident(TermName("scala.meta.Type")),
                TermName("scala.meta.Name")),
            List(Literal(Constant(str))))
    }

    def unhideTypeParam(tree: Tree): Option[scala.meta.Type] =
    tree match {
        case Apply(Select(Ident(TermName("scala.meta.Type")),
        TermName("scala.meta.Name")), List(Literal(Constant(str: String))))
        if str.startsWith(fakeIdent) => {
            str.substring(fakeIdent.length).parse[scala.meta.Type].toOption
        }
        case _ => None
    }

    object HideTypeParameters extends Transformer {
        override def transform(tree: Tree): Tree = {
            val ntree = tree match {
                case Apply(Select(New(AppliedTypeTree(nm, targs)), fn), args) =>
                    Apply(Select(New(nm), fn), (targs map hideTypeParam) ++ args)
                case _ => tree
            }
            super.transform(ntree)
        }
        def apply(tree: Tree) = transform(tree)
    }
}
