package localhost.plugin

/*  This trait provides error handling facilities. */
trait Errors { self: ParadiseNgComponent =>
    import global._

    case class ParadiseNgException(pos: Position, msg: String) extends Exception

    def UnresolvedMacroParameterError[T](pos: Position): T = {
        throw ParadiseNgException(pos,
            "couldn't resolve a macro annotation argument; " +
            "only constant values are supported")
    }

    def NoConstructorError[T](an: AnnotationInfo, args: Seq[Any]): T = {
        val name = an.tpe.typeSymbol.name
        val argsStr = args.map(x => x.getClass().toString())
            .mkString(", ")
        throw ParadiseNgException(an.pos,
            s"macro annotation $name can't be instantiated with ($argsStr)")
    }

    def AnnotationFromCurrentRunError[T](an: AnnotationInfo): T = {
        import scala.tools.nsc.interpreter._
        val name = an.tpe.typeSymbol.name
        val advice = if (reporter.isInstanceOf[ReplReporter]) {
            "it must be on a separate line and without :paste enabled"
        } else {
            "it must be in a separate module"
        }
        throw ParadiseNgException(an.pos,
            s"macro annotation $name is defined in the current run; " + advice)
    }
}
