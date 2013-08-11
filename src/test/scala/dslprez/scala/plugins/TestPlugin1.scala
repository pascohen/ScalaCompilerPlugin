//package dslprez.scala.plugins

object TestPlugin extends App {

val startT = System.currentTimeMillis()

def getStartTime() = startT

  def meth = {
    System.exit(1)
  }

  def meth2 = {
    java.lang.System.exit(1)
  }

  def meth3 = {
    System.getProperties()
    System.out.println("test")
  }

  System.exit(1)
  Thread.sleep(3000)
  System.getProperties()
  System.out.println("test")
  java.lang.System.exit(1)


  System.exit(1)
  Thread.sleep(3000)
  System.getProperties()
  System.out.println("test")
  java.lang.System.exit(1)


  meth
  meth2
  meth3
}
