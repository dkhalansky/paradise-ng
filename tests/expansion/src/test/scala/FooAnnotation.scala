package test1
import org.scalatest.FunSuite
import test.macros._

@FooAnnotation
class Hello { }

class Main {
    def main(args: Array[String]) = {
    }
}

class FooAnnotationTest extends FunSuite {

    test("Top-level definitions are replaced") {
        val foo = new Foo()
        assert(foo.bar() === 15 + 16)
    }

}
