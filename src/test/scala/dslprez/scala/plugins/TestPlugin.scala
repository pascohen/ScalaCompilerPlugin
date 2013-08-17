//package dslprez.scala.plugins

class C {
def move(s:String) = println("Moving "+s)
}

object TestPlugin extends App {

println("Start")
val I = new C

for (i <- 1 to 100) {
   //val m = "hello"+i 
   // println(m)
   I move "i="+i
   Thread.sleep(1000)
}

println("End")
}
