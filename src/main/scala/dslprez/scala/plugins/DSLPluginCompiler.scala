package dslprez.scala.plugins

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.ast.TreeDSL

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
        if (restrictCalls) {
          //println("B/W list will be used based on BL file:" + blacklistFile + " and WL file:" + whitelistFile)
          // TODO: Filter on files
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

  private object DSLTimerComponent extends PluginComponent
    with Transform
    with TypingTransformers
    with TreeDSL {
    val global: DSLPluginCompiler.this.global.type = DSLPluginCompiler.this.global

    val runsAfter = List[String]("typer", "refchecks", "dslrestrict");

    val phaseName = "dsltimer"

    def newTransformer(unit: global.CompilationUnit) = new TemplateTransformer(unit)

    class TemplateTransformer(unit: global.CompilationUnit) extends TypingTransformer(unit) {

      //println("What is timer status " + isTimerActivated + "/" + timerValue)

      def preTransform(tree: global.Tree): global.Tree = tree

      def postTransform(tree: global.Tree): global.Tree = {
        //println("post-transforming fun " + tree)

        tree match {
          case global.Apply(fun, _) =>
            if (fun.toString.contains("move")) {
              //println("post-transforming fun " + fun)
              
              //TreeMethods(target).Int_>(other)
              val timerConst = CODE.LIT(timerValue.getOrElse(10))
              val scriptDuration = global.reify {((System.currentTimeMillis()-dslprez.scala.timer.MyTimer.getStartTime) / 1000).intValue }.tree
              val condTree = new CODE.TreeMethods(scriptDuration).INT_>=(timerConst)
                //CODE.fn(scriptDuration,global.TermName(">"),timerConst)
                //global.reify {((System.currentTimeMillis()-dslprez.timer.MyTimer.getStartTime) / 1000) > 4 }.tree
              val ifTree = global.reify {throw new RuntimeException("Execution timed out. Start time: "+new java.util.Date(dslprez.scala.timer.MyTimer.getStartTime))}.tree
              //val falseTree = global.reify { println("fake")}.tree
              val testTree = new CODE.IfStart(condTree,ifTree).ELSE(tree)
              global.typer.typed(testTree)           
            } else tree
          case _ => tree
        }
      }

      override def transform(tree: global.Tree): global.Tree = {
        if (timerValue.isDefined) {
        postTransform(super.transform(preTransform(tree)))
        } else {
          super.transform(tree)
        }
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
