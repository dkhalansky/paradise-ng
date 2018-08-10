package localhost.plugin
import scala.meta._

/*  This trait deals with type parameters in annotations.

    We want to support a syntax that allows users to provide types to their
    macro annotations. For example, it needs to be possible to use the
    scalameta tree for type `Int` when processing the invokation `@foo[Int]`.

    However, obviously, *all* the approaches that provide this behavior are
    magical. In Paradise, `this` had a special meaning in macro annotations
    and returned the scalameta tree corresponding to the invokation, so one
    could do

        this match {
            q"foo[$tpe]($val)" => ...
        }

    Also, the visibility scopes were magical, so if the macro annotation
    was defined as `class foo[T]`, one could access the tree for `T` by
    just using the variable `T`.

    Our approach is much less magical. It only transforms type parameters on
    macro annotations to normal arguments. For instance, it effectively
    transforms

        @foo[Int, String](3, 4)

    to

        @foo(t"Int", t"String", 3, 4)

    This approach allows to keep the macro annotations themselves completely
    non-magical, while probably providing the same functionality. */
trait TypeParamsDesugar { self: ParadiseNgComponent =>

    import global._

    private val fakeIdent = "fake) "

    private def hideTypeParam(tree: Tree): Tree = {
        val str = fakeIdent + tree.toString
        /*  We hide it so thoroghly because we while nowadays the compiler only
            concerns itself with the number of given arguments on the typing
            phase, we can't rely on this behavior and need to provide a
            plausibly-typed tree. Since the parameter is going to be used in
            places where `scala.meta.Type` is required, we hide the parameter
            in the tree with such a type. */
        Apply(Select(Ident(TermName("scala.meta.Type")),
                TermName("scala.meta.Name")),
            List(Literal(Constant(str))))
    }

    /*  Try to recover the `scala.meta.Type` that may be hidden inside. */
    def unhideTypeParam(tree: Tree): Option[scala.meta.Type] =
    tree match {
        case Apply(Select(Ident(TermName("scala.meta.Type")),
        TermName("scala.meta.Name")), List(Literal(Constant(str: String))))
        if str.startsWith(fakeIdent) => {
            str.substring(fakeIdent.length).parse[scala.meta.Type].toOption
        }
        case _ => None
    }

    /*  Transform a given tree so that the trees that denote creating an
        instance with type arguments are replaced in such a way that the type
        arguments are prepended to normal arguments and have the type of
        `scala.meta.Type` while also storing in themselves enough information
        to reconstruct the original type parameters. */
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
