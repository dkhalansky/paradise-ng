package com.github.dkhalansky.paradiseng.plugin

/*  This trait provides error handling facilities. */
trait Errors { self: ParadiseNgComponent =>
    import global._

    case class ParadiseNgException(pos: Position, msg: String) extends Exception

    def ParameterResolutionError[T](pos: Position, e: TreeEvaluationError): T ={
        e match {
            case TreeEvaluationError.UnsupportedTree(t) =>
                throw ParadiseNgException(t.pos,
                    "only constant values and val-definitions are supported in macro annotation parameters")
            case TreeEvaluationError.NoSourceError(t) =>
                throw ParadiseNgException(t.pos,
                    "couldn't find the source for macro annotation parameter")
            case TreeEvaluationError.AnnotatedDependency(t) =>
                throw ParadiseNgException(t.pos,
                    "annotated val-definitions can't be used in macro annotation invocations")
            case TreeEvaluationError.VarValueDependency(t) =>
                throw ParadiseNgException(t.pos,
                    "mutable definitions can't be referenced in macro annotation invocations")
        }
    }

    def TreeTransformationError[T](pos: Position, t: Throwable): T = {
        throw ParadiseNgException(pos,
            s"an error occurred during a macro annotation expansion: $t")
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
