//package dslprez.scala.plugins

object TestPlugin extends App {

println("Start")

for (i <- 1 to 100) {
val m = "hello"+i 
    println(m)
   Thread.sleep(1000)
}

println("End")
}
