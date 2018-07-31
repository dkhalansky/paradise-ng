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

    test("Annotated companion object is modified") {
        @AppendBarToCompanion
        class F {
            def baz() = 16
        }

        @AppendFooToObject
        object F

        val v = new F

        assert(v.baz === 16)
        assert(F.bar === 42)
        assert(F.foo === 43)
    }

    test("Annotated companion object is modified [if the object is above]") {
        @AppendFooToObject
        object E

        @AppendBarToCompanion
        class E {
            def baz() = 16
        }

        val v = new E

        assert(v.baz === 16)
        assert(E.bar === 42)
        assert(E.foo === 43)
    }

    test("Many companion objects can be created") {
        @AppendBarToCompanion
        class M {
            def baz() = 1024
        }

        @AppendBarToCompanion
        class N {
            def baz() = 2048
        }

        val v1 = new M
        val v2 = new N

        assert(N.bar === 42)
        assert(M.bar === 42)
        assert(v1.baz === 1024)
        assert(v2.baz === 2048)
    }

    test("Neighbors don't share companions") {
        @AppendBarToCompanion
        class M {
            def baz() = 1024
        }

        object M

        assert(M.bar === 42);

        object D {}

        {
            @AppendBarToCompanion
            class M {
                def baz() = 1024
            }

            object M

            assert(M.bar === 42)
        }

        {
            @AppendBarToCompanion
            class M {
                def baz() = 1024
            }

            object M

            assert(M.bar === 42)
        }

        object C {
            @AppendBarToCompanion
            class M {
                def baz() = 1024
            }

            object M
        }

        assert(C.M.bar === 42)
    }

}

