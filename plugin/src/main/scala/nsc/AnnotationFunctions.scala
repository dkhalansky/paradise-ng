package localhost.plugin
import localhost.lib._

trait AnnotationFunctions { self: ParadiseNgComponent =>

    import global._

    /* Given a symbol, determine the string to feed to the classloader to
       acquire the corresponding class. */
    private def getClassNameBySymbol(symbol: Symbol) = {
        def loop(sym: Symbol): String = sym match {
            case sym if sym.isTopLevel =>
                val suffix = if (sym.isModule || sym.isModuleClass) "$" else ""
                sym.fullName + suffix
            case sym =>
                val separator = if (sym.owner.isModuleClass) "" else "$"
                loop(sym.owner) + separator + sym.javaSimpleName.toString
        }
        loop(symbol)
    }

    private lazy val loader = Reflect.findMacroClassLoader(
        global.classPath.asURLs, global.settings.outputDirs.getSingleOutput)

    private def getParameter(tree: Tree, maxDepth: Int = 15): Option[Any] = {
        if (maxDepth <= 0) {
            None
        } else tree match {
            case Literal(Constant(v)) => Some(v)
            case s if s.symbol != null && s.symbol != NoSymbol =>
                s.symbol.source match {
                    case Some(ValDef(_, _, _, v)) =>
                        getParameter(v, maxDepth - 1)
                    case _ => None
                }
            case _ => None
        }
    }

    /* From annotation defition acquire an instance of the annotation class. */
    def getAnnotationFunction(annotation: AnnotationInfo) = {
        if (currentRun.compiles(annotation.tpe.typeSymbol)) {
            AnnotationFromCurrentRunError(annotation)
        }

        val withSources = attachSourcesToSymbols(annotation.original)
        val args = annotation.args.map(xs => {
            unhideTypeParam(xs) match {
                case Some(t) => t
                case None => getParameter(xs) match {
                    case Some(v) => v
                    case None => UnresolvedMacroParameterError(xs.pos)
                }
            }
        })
        val cls_name = getClassNameBySymbol(annotation.tpe.typeSymbol)
        try Reflect.instantiate[ParadiseNgAnnotation](loader, cls_name, args)
        catch {
            case e: java.lang.NoSuchMethodException =>
                NoConstructorError(annotation, args)
        }
    }
}
