import localhost.lib._

class myMetaAnnotation extends ParadiseNgAnnotation

@myMetaAnnotation
class Hello {
    @myMetaAnnotation
    def main(@myMetaAnnotation args: Array[String]) =
        println("Hello!")
}
