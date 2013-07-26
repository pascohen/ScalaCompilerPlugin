package dslprez.scala.plugins

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class BlackListPluginCompiler(val global: Global) extends Plugin {

  val name = "blacklist"
  val description = "Black List plugin"
  val components = List[PluginComponent](BLComponent)

  private object BLComponent extends PluginComponent {
    val global: BlackListPluginCompiler.this.global.type = BlackListPluginCompiler.this.global

    val runsAfter = List[String]("refchecks");

    val phaseName = "blacklist"
    def newPhase(prev: Phase): Phase = new BLCompilerPhase(prev)

    class BLCompilerPhase(prev: Phase) extends StdPhase(prev) {
      override def name = BlackListPluginCompiler.this.name

      def apply(unit: global.CompilationUnit) {
        for (global.Apply(fun, _) <- unit.body) {
          fun.symbol match {
            case method: global.MethodSymbol =>
              val classSymbol = method.owner
              val systemSymbol = global.findMemberFromRoot(global.TermName(classOf[System].getName))
              if (classSymbol.tpe <:< systemSymbol.tpe) unit.error(fun.pos, "System.exit forbidden")
            case _ => ()
          }
        }

        /*       
        val l = global.typeOf[System].termSymbol
        		println("BC = "+l)
        		
        for (b <- unit.body) {
                if (b.toString().contains("System.exit")) {
               	  unit.error(b.pos, "System.exit forbidden")
           }
        }*/
      }
    }
  }
}