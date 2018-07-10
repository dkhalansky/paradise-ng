package testScalametaIdAnnotation
import test.macros._

@ScalametaIdAnnotation
class Hello {
    @ScalametaIdAnnotation
    def hi() = println("Hi!")
}

class Main {
    def main(args: Array[String]) = {
        val hello = new Hello()
        hello.hi()
    }
}
