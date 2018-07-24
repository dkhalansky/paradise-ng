import org.scalatest.FunSuite
import test.macros._

class Companions extends FunSuite {

    test("A new companion object is created") {
        @AppendBarToCompanion
        class A {
            def baz() = 16
        }

        val v = new A

        assert(v.baz === 16)
        assert(A.bar === 42)
    }

    test("Existing companion object is modified [if not defined in code]") {

        // a temporary workaround: scalac doesn't find the companion objects
        // of classes defined inside code blocks for some reason.
        object Test {
            @AppendBarToCompanion
            class A {
                def baz() = 16
            }

            object A {
                def foo() = 15
            }

            val v = new A
        }

        assert(Test.v.baz === 16)
        assert(Test.A.foo === 15)
        assert(Test.A.bar === 42)
    }

    test("Existing companion object is modified") {
        @AppendBarToCompanion
        class B {
            def baz() = 16
        }

        object B {
            def foo() = 15
        }

        val v = new B

        assert(v.baz === 16)
        assert(B.foo === 15)
        assert(B.bar === 42)
    }

}

