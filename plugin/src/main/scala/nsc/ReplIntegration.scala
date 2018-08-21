package com.github.dkhalansky.paradiseng.plugin
import scala.tools.nsc.interpreter._

/*  There is a special need for handling REPL because it works
    like this:

      * The initial query is parsed into a series of statements;
      * Each statement is preliminarily analyzed to determine which of the
        older REPL lines it references and what general effect it has
        (importing or defining something or just calculating);
      * The initial query is wrapped in a series of objects with the name
        of `$iw` that provide all the imports that were deemed necessary
        as per the previous phase;
      * This wrapped query is compiled (that's where our plugin kicks in) and
        executed.
      * /The handlers that were established at the priliminary analysis
        receive the newly-created symbols. /

    The problem is, when we change the set of top-level statements, the REPL
    fails to acknowledge it. If we replaced `class B` with
    `class Foo; import scala.collection.mutable._`, it would just say
    `defined class B`--even though class B is not in scope and trying to
    access it results in `B not found`.

    So, what we do here is we rebuild the set of tree handlers anew. */
trait ReplIntegration { self: ParadiseNgComponent =>
    import global._

    private def obtainField(cls: Class[_], name: String) = {
        val result = cls.getDeclaredField(name)
        result.setAccessible(true)
        result
    }

    private lazy val f_intp =
        obtainField(classOf[ReplReporter], "intp")
    private lazy val f_executingRequest =
        obtainField(classOf[IMain], "executingRequest")
    private lazy val f_handlers =
        obtainField(classOf[IMain#Request], "handlers")
    private lazy val f_referencedNames =
        obtainField(classOf[IMain#Request], "referencedNames")

    /*  Should only be called when there is a reason to, that is, the set of
        handlers could change. In particular, it shouldn't be called on the
        trees that arise during the REPL initialization; otherwise, there'll
        be a `NullPointerException`. */
    def updateReplHandlers(body: Tree) {
        if (!global.reporter.isInstanceOf[ReplReporter]) {
            return
        }
        val intp = f_intp.get(reporter).asInstanceOf[IMain]
        import intp._
        val req = f_executingRequest.get(intp).asInstanceOf[Request]
        import memberHandlers._

        /*  We find the innermost class or object with the name `$iw`.
            The code inside it is what we need.

            Maybe it's a good idea to check for `-Yrepl-class-based` flag to
            only look for classes if the flag is set and only for objects if
            it's not. Though it's probably a bad practice to use the name
            `$iw` when inside the REPL. It's probably easy to make it go haywire
            by doing that even without our plugin. */
        import scala.language.reflectiveCalls
        var innerMostIw = null.asInstanceOf[{def impl: Template}]
        val traverser = new Traverser {
            def goodName(name: AnyRef) = {
                name.toString == nme.INTERPRETER_IMPORT_WRAPPER.toString
            }

            override def traverse(tree: Tree) {
                tree match {
                    case m: ModuleDef if goodName(m.name) =>
                        innerMostIw = m
                    case m: ClassDef  if goodName(m.name) =>
                        innerMostIw = m
                    case _ =>
                }
                super.traverse(tree)
            }
        }
        traverser(body)

        /*  The search can fail, then we fail too, with a
            `NullPointerException`. It can happen if there is no `$iw` object,
            but if this place was reached, it means that it was called from the
            REPL, and in REPL there are no `$iw` objects only on initialization.
            We provide no failsafes for this case because failing to finh `$iw`
            must be a bug: this method is only called when there is a reason to
            do so, and not on initialization. */

        /*  The first definition in a template is `def <init>`, generated
            automatically on parsing. This definition shouldn't have a handler,
            hence the call to `tail` here. */
        val handlers = innerMostIw.impl.body.tail.map {
            memberHandlers chooseHandler _.asInstanceOf[intp.global.Tree] }
        val names = handlers flatMap (_.referencedNames)
        f_handlers.set(req, handlers)
        f_referencedNames.set(req, names)
    }
}
