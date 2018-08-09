import org.scalatest.FunSuite
import test.macros._

class TypeAnnotationsTest extends FunSuite {

    test("Annotations with a type parameter are expanded") {
        @InstantiateWithInt[Integer]("v", 500) object WillBeRemoved
        assert(v === 500)
    }

    test("Annotations with multiple type parameters are expanded") {
        @InstantiateMultiple[Integer, String]("v1", "v2", 500, "Help")
        object WillBeRemoved
        assert(v1 === 500)
        assert(v2 === "Help")
    }

}
