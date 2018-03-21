package test1
import localhost.lib._

@FooAnnotation
class Hello { }

class Main {
    def main(args: Array[String]) = {
        val foo = new Foo()
        foo.bar()
    }
}
