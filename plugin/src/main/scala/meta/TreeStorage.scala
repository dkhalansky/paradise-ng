package localhost.plugin.meta
import scala.meta._
import java.io.InputStream
import java.nio.charset.StandardCharsets
import scala.meta.internal.tokens.TokenStreamPosition
import scala.meta.internal.trees.Origin
import scala.reflect.ClassTag

object TreeStorage {

    private case class Storage[T: ClassTag](payload: T, pos: Position)
    extends InputStream {
        override def read(): Int = -1
    }

    private def originField(tree: Tree): java.lang.reflect.Field = {
        val origin = tree.getClass()
            .getDeclaredFields()
            .find(_.getName == "privateOrigin")
            .get
        origin.setAccessible(true)
        origin
    }

    implicit class XtensionTreeStorage[T <: Tree](val tree: T) extends AnyVal {
        def storePayload[U: ClassTag](payload: U): T = {
            originField(tree).set(tree,
                Origin.Parsed(
                    Input.Stream(new Storage[U](payload, getPosition()),
                        StandardCharsets.UTF_8),
                    dialects.Scala212,
                    TokenStreamPosition(-1, -1)
                )
            )
            tree
        }

        def getPayload[U: ClassTag]() = originField(tree).get(tree) match {
            case Origin.Parsed(Input.Stream(Storage(p: U, _), _), _, _) =>
                Some(p)
            case _ => None
        }

        def getPosition(): Position = originField(tree).get(tree) match {
            case Origin.Parsed(Input.Stream(Storage(_, p), _), _, _) => p
            case _ => tree.pos
        }
    }

}
