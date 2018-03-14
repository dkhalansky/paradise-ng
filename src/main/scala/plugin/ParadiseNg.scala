package localhost.plugin
import localhost.lib.ParadiseNgAnnotation

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class ParadiseNg(val global: Global) extends Plugin {
    import global._
    val name = "paradise-ng"
    val description = "Manipulate ASTs"
    val components = new ParadiseNgComponent(this, global) :: Nil
}

class ParadiseNgComponent(plugin: Plugin, val global: Global) extends PluginComponent {
    import global._

    val phaseName = "paradise-ng"
    val runsAfter = "parser" :: Nil

    def newPhase(_prev: Phase) : StdPhase = new ParadiseNgPhase(_prev)

    private class ParadiseNgPhase(prev: Phase) extends StdPhase(prev) {
        def apply(unit: CompilationUnit) : Unit = {
        }
    }
}
