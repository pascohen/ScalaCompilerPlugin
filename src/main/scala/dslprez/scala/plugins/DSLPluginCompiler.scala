package dslprez.scala.plugins

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform

class DSLPluginCompiler(val global: Global) extends Plugin {

  val name = "dslplugin"
  val description = "various filters - blacklist/whitelist and timer plugin"
  val components = List[PluginComponent](DSLRestrictComponent, DSLTimerComponent)

  var blacklistFile: Option[String] = None
  var whitelistFile: Option[String] = None
  var timerValue: Option[Int] = None

  lazy val isTimerActivated = timerValue.isDefined
  lazy val restrictCalls = blacklistFile.isDefined || whitelistFile.isDefined

  override def processOptions(options: List[String], error: String => Unit) {
    for (option <- options) {
      if (option.startsWith("blacklistFile:")) {
        blacklistFile = Some(option.substring("blacklistFile:".length))
      } else if (option.startsWith("whitelistFile:")) {
        whitelistFile = Some(option.substring("whitelistFile:".length))
      } else if (option.startsWith("timerValue:")) {
        timerValue = Some(option.substring("timerValue:".length).toInt)
      } else {
        error("Option not understood: " + option)
      }
    }
  }

  private object DSLRestrictComponent extends PluginComponent {
    val global: DSLPluginCompiler.this.global.type = DSLPluginCompiler.this.global

    val runsAfter = List[String]("refchecks");

    val phaseName = "dslrestrict"
    def newPhase(prev: Phase): Phase = new DSLRestrictPhase(prev)

    class DSLRestrictPhase(prev: Phase) extends StdPhase(prev) {
      override def name = DSLPluginCompiler.this.name

      def apply(unit: global.CompilationUnit) {
        //println("What is B/Wlist status " + blacklistFile + "/" + whitelistFile + "/" + restrictCalls)
        if (restrictCalls) {
          for (global.Apply(fun, _) <- unit.body) {
            fun.symbol match {
              case method: global.MethodSymbol =>
                val classSymbol = method.owner
                val systemSymbol = global.findMemberFromRoot(global.TermName(classOf[System].getName))
                if ((method.nameString == "exit")
                  && (classSymbol.tpe <:< systemSymbol.tpe))
                  unit.error(fun.pos, "System.exit forbidden")
              case _ => ()
            }
          }
        }
      }
    }
  }

  private object DSLTimerComponent extends PluginComponent with Transform {
    val global: DSLPluginCompiler.this.global.type = DSLPluginCompiler.this.global

    val runsAfter = List[String]("refchecks", "dslrestrict");

    val phaseName = "dsltimer"

    def newTransformer(unit: global.CompilationUnit) = new TemplateTransformer

    class TemplateTransformer extends global.Transformer {

      //println("What is timer status " + isTimerActivated + "/" + timerValue)

      def preTransform(tree: global.Tree): global.Tree = tree

      def postTransform(tree: global.Tree): global.Tree = tree 
      /*match {
        case global.Apply(fun,_) =>
          println("post-transforming fun " + fun)
          if (fun.toString.contains("exit"))
        	  global.TreeReplacer(tree,(reify {System.out.println("test")}).tree)
          else tree
        case _ => tree
      }*/

      override def transform(tree: global.Tree): global.Tree = {
        postTransform(super.transform(preTransform(tree)))
        }
    }

    /*
    def newPhase(prev: Phase): Phase = new DSLTimerPhase(prev)

    class BLCompilerPhase(prev: Phase) extends StdPhase(prev) {
      override def name = DSLPluginCompiler.this.name

      def apply(unit: global.CompilationUnit) {
         println("What is timer status " + isTimerActivated + "/" + timerValue)
      }
    }*/
  }
}