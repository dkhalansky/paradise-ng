package testAppend
import org.scalatest.FunSuite
import test.macros._

class AppendAnnotations extends FunSuite {

    test("Nested macros expand") {
        var letters = ""

        @AppendAAnnotation
        def foo() = {
            @AppendBAnnotation
            def bar() = {}
            bar()
        }

        foo()

        assert(letters === "ba")
    }

    // A change from how Paradise behaves!
    test("Verify expansion order") {
        var letters = ""

        @AppendBAnnotation
        @AppendCAnnotation
        def foo() = {
            @AppendAAnnotation
            def bar() = {}
            bar()
        }

        foo()

        assert(letters === "acb")
    }

}

