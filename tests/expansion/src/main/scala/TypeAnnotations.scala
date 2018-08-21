package test.macros
import scala.meta._
import com.github.dkhalansky.paradiseng.lib._

class InstantiateWithInt(T: Type, nm: String, x: Int)
extends ParadiseNgAnnotation {
    override def apply(s: Stat): Stat = {
        q"val ${Pat.Var(Term.Name(nm))} = new $T($x)"
    }
}

class InstantiateMultiple(
    T: Type, U: Type, nm1: String, nm2: String, x: Int, y: String
) extends ParadiseNgAnnotation {
    override def apply(s: Stat): Stat = {
        q"""
        {
            val ${Pat.Var(Term.Name(nm1))} = new $T($x)
            val ${Pat.Var(Term.Name(nm2))} = new $U($y)
        }
        """
    }
}
