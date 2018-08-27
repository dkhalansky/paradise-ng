import macros._

class Main {
    @replaceWithFoo(42)
    class WillBeRemoved

    def a() = {
        println(foo.bar)
    }
}
