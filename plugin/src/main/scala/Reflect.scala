package localhost.plugin
import scala.reflect.io.AbstractFile
import java.net.URL
import scala.reflect.api.Symbols

/* A set of functions to deal with all the reflective needs. */
object Reflect {

    /* Create a classloader that can acquire all the necessary runtime
       definitions

       @classpath -- the set of URLs in which to find the compiled
           classes from other, already processed modules.

       @compilerOutput -- if the compiler stores the results of its
           work in memory without outputting the resulting bytecode
           anywhere, they are stored in a virtual directory. This
           is the case for REPL: after each line the compiler
           dumps the new compiled definitions into the virtual
           directory designated for this.

       @parent -- the parent classloader.*/
    def findMacroClassLoader(
        classpath: Seq[URL],
        compilerOutput: Option[AbstractFile] = None,
        parent: ClassLoader = this.getClass().getClassLoader()
    ): ClassLoader = {
        import scala.reflect.internal.util._
        val loader = ScalaClassLoader.fromURLs(classpath, parent)
        compilerOutput match {
            case Some(virtualDirectory) => {
                new AbstractFileClassLoader(virtualDirectory, loader) {}
            }
            case None => loader
        }
    }

    /* Instantiate a class by the given name with the given arguments to
       the constructor. */
    def instantiate[T](
        loader: ClassLoader, className: String, args: List[AnyRef]
    ): T = {
        val arg_types = args.map(xs => xs.getClass())
        val cls = Class.forName(className, true, loader)
        cls.getConstructor(arg_types.toArray: _*)
            .newInstance(args.toArray: _*)
            .asInstanceOf[T]
    }

}

/* The facade that allows to simply load the functions from our macro
   annotations. */
class ParadiseNgFunctionRetriever(loader: ClassLoader) {
    import scala.meta._

    /* Get the function that transforms scalameta trees. */
    def apply(className: String, args: List[AnyRef]): Tree => Tree = {
        import scala.language.reflectiveCalls
        Reflect.instantiate[{ def apply(annottee: Tree): Tree }](
            loader, className, args
        ).apply(_)
    }
}
