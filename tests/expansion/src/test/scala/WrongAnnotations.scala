package testWrongAnnotations
import scala.annotation.StaticAnnotation
import test.macros._
import org.scalatest.FunSuite

class FooAnnotation extends StaticAnnotation {}

// The compiler should not recognize this annotation
// as its own, despite the name
@FooAnnotation
class Hello {
    def bar() = 1
}

class WrongAnnotations extends FunSuite {

    test("Locally-defined classes shadow the annotations") {
        val hello = new Hello
        assert(hello.bar() === 1)
    }

}
