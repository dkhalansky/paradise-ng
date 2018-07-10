package test1
import test.macros._

@FooAnnotation
class Hello { }

class Main {
    def main(args: Array[String]) = {
        val foo = new Foo()
        foo.bar()
    }
}
