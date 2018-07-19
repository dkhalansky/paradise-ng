package testHelloAnnotation
import org.scalatest.FunSuite
import test.macros._

class HelloAnnotations extends FunSuite {

    test("Constant parameters work") {
        @HelloSomething("Hello", "World")
        def nothing() { }
        assert(HelloWorld() === "Hello, World!")
    }

    test("Named parameters work") {
        @HelloSomething(hello = "Hello", world = "World")
        def nothing() { }
        assert(HelloWorld() === "Hello, World!")
    }

    test("Named parameters in different order work") {
        @HelloSomething(world = "Venus", hello = "Sup")
        def nothing() { }
        assert(SupVenus() === "Sup, Venus!")
    }

    /*
    test("Using the default values in parameters works") {
        @HelloSomething("Hello")
        def nothing() { }
        assert(HelloEarth() === "Hello, Earth!")
        @HelloSomething(world = "Mars")
        def nothing() { }
        assert(HiMars() === "Hi, Mars!")
    }
    */

}
