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

    test("Everything that should have a companion, has one") {
        @ReplaceCompanion
        class A
        object A
        assert(A.foo === 1556)

        @ReplaceCompanion
        trait B
        object B
        assert(B.foo === 1556)

        @ReplaceCompanion
        type C = Int
        object C
        assert(C.foo === 1556)
    }

    test("Everything that shouldn't have a companion, doesn't") {

        /* there is a bug in scalac itself:

               def f() {
                   class A
                   def a() {
                       object A
                   }
               }

           fails. Our code that looks for companion objects is sometimes more
           accurate than the reference one! (it was a tongue-in-a-cheek since
           our code for finding companions is ugly).

           So, we have to wrap it all in an object or a class to perform tests.
        */
        object Failsafe {
            @FailIfCompanionExists
            class A
            if (3 < 4) {
                object A
            }
            def a() {
                object A
            }

            @FailIfCompanionExists
            trait B
            def b() {
                object B
            }

            @FailIfCompanionExists
            type C = Int
            if (3 < 4) {
                object C
            }
            {
                object C
            }
            def c() {
                object C
            }
        }
    }

}
