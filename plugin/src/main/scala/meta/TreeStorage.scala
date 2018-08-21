package com.github.dkhalansky.paradiseng.plugin.meta
import scala.meta._
import java.io.InputStream
import java.nio.charset.StandardCharsets
import scala.meta.internal.tokens.TokenStreamPosition
import scala.meta.internal.trees.Origin
import scala.reflect.ClassTag

/*  This object provides an extension class that allows to store arbitrary
    payload in any scalameta tree. It was inspired by
    https://github.com/scalacenter/scala-syntax/blob/b225455dcd9b46bc8b3a1ea72df591cd8a76112e/format/src/main/scala/scala/meta/internal/format/Comments.scala

    The idea is to replace the object responsible for providing tree position
    with an arbitrary payload. Naturally, after this interference, calling
    `.pos` on a tree leads to a crash, so we also provide a 'getPosition`
    method that calls `.pos` on normal trees and remembers the old position
    on the trees with the payload. */
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
        // Store anything in a tree.
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

        // Get anything from a tree, given the type, if it has it.
        def getPayload[U: ClassTag]() = originField(tree).get(tree) match {
            case Origin.Parsed(Input.Stream(Storage(p: U, _), _), _, _) =>
                Some(p)
            case _ => None
        }

        /*  This must be called instead of `.pos` on all trees that carry
            a payload. Otherwise, an exception will be thrown. */
        def getPosition(): Position = originField(tree).get(tree) match {
            case Origin.Parsed(Input.Stream(Storage(_, p), _), _, _) => p
            case _ => tree.pos
        }
    }

}
