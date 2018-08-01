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

    /* This is one strange function! What does it do?
       Well, in Scala, basic types (such as Int or Byte) can map to boxed
       values, that is, be stored on the heap, or they can be stored on
       the stack.

       In Java, these were separate types: `Integer` denoted the boxed
       variant and `int`-- the unboxed one. In Scala, it's all completely
       transparent.

       It is a good thing, but here comes a problem. Let's say we needed to
       use a constructor that accepted an `Int`. A user gives us its
       argument, `3`. If we treat this `3` as a value and ask what its class
       is, Scala answers, `java.lang.Integer`. But the constuctor was
       specialized to accept an unboxed `int`!

       So, we need to find the constructor with the given signature, and to
       infer the signature from the arguments that the user provided. And from
       the reflection's point of view, it matters whether a function accepts
       boxed or unboxed values: there can be two different functions,
       `f(Integer i)` and `f(int i)`, in the same class.

       Thus, this function. It assumes that the user specified their annotation
       with an `Int` or a `Boolean` and not with `java.lang.Integer` and tries
       to find the unboxed class descriptions where possible.
    */
    private def getWrappedClass(v: Any): Class[_] = v match {
        case x: Unit    => x.getClass
        case x: Boolean => x.getClass
        case x: Byte    => x.getClass
        case x: Short   => x.getClass
        case x: Int     => x.getClass
        case x: Long    => x.getClass
        case x: Float   => x.getClass
        case x: Double  => x.getClass
        case x: Char    => x.getClass
        case _          => v.getClass
    }

    /* Instantiate a class by the given name with the given arguments to
       the constructor. */
    def instantiate[T](
        loader: ClassLoader, className: String, args: Seq[Any]
    ): T = {
        val arg_types = args.map(xs => getWrappedClass(xs))
        val cls = Class.forName(className, true, loader)
        val nargs = args.map(t => t.asInstanceOf[AnyRef]).toArray
        cls.getConstructor(arg_types: _*)
            .newInstance(nargs: _*)
            .asInstanceOf[T]
    }

}

/* The facade that allows to simply load the functions from our macro
   annotations. */
class ParadiseNgFunctionRetriever(loader: ClassLoader) {
    import scala.meta._

    /* Get the function that transforms scalameta trees. */
    def apply(className: String, args: Seq[Any]): Stat => Stat = {
        import scala.language.reflectiveCalls
        val inst = Reflect.instantiate[{ def apply(annottee: Stat): Stat }](
            loader, className, args
        )
        inst.apply(_)
    }
}
