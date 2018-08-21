import com.github.dkhalansky.paradiseng.lib._

/* Should fail since the annotation doesn't accept type parameters */
@FooAnnotation(34, "")
class Hello { }
