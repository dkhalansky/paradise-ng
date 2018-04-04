package testWrongAnnotations
import scala.annotation.StaticAnnotation

class FooAnnotation extends StaticAnnotation {}

// The compiler should not recognize this annotation
// as its own, despite the name
@testWrongAnnotations.FooAnnotation
class Hello { }

class Foo { }

class Main {
    def main(args: Array[String]) = { }
}
