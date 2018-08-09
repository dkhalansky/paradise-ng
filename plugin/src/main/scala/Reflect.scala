package localhost.plugin
import scala.reflect.io.AbstractFile
import java.net.URL

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
        loader: ClassLoader, className: String, args: Seq[Any]
    ): T = {
        val cls = Class.forName(className, true, loader)
        val nargs = args.map(t => t.asInstanceOf[AnyRef]).toArray
        for (ctr <- cls.getConstructors) {
            try return ctr.newInstance(nargs: _*).asInstanceOf[T] catch {
                case e: java.lang.IllegalArgumentException =>
            }
        }
        throw new java.lang.NoSuchMethodException
    }

}
