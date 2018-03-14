import localhost.lib._

class myMetaAnnotation extends ParadiseNgAnnotation

@myMetaAnnotation
class Hello {
    def main(args: Array[String]) =
        println("Hello!")
}
