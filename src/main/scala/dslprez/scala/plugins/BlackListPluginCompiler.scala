package dslprez.scala.plugins

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class BlackListPluginCompiler(val global: Global) extends Plugin {
  import global._

  val name = "blacklist"
  val description = "Black List plugin"
  val components = List[PluginComponent](BLComponent)
  
  private object BLComponent extends PluginComponent {
    val global: BlackListPluginCompiler.this.global.type = BlackListPluginCompiler.this.global
    		
    val runsAfter = List[String]("refchecks");
    
    val phaseName = BlackListPluginCompiler.this.name
    def newPhase(_prev: Phase) = new BLCompilerPhase(_prev)    
    
    class BLCompilerPhase(prev: Phase) extends StdPhase(prev) {
      override def name = BlackListPluginCompiler.this.name
      def apply(unit: CompilationUnit) {
        println("HHHH")
        for ( tree @ Apply(Select(rcvr, nme.DIV), List(Literal(Constant(0)))) <- unit.body;
             if rcvr.tpe <:< definitions.IntClass.tpe) 
          {
            unit.error(tree.pos, "definitely division by zero")
          }
      }
    }
  }
}