package base

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.util._
import scala.util.Random

class Adder_Tree_Test extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Adder_Tree"
  it should "produce right output" in {
    //Add your own functions here
    //Add your own values here
    val width = 8
    val nInputs = 8
    val steps = log2Ceil(nInputs)
    test(
      new AdderTree(
        width, nInputs
      )
    ) { c =>
        // Prepare Data
        val inputs: Seq[Int] = Seq.fill(nInputs)(Random.nextInt(10))
        val output = inputs.sum
        // Send data to your HW's IO ports with "c.io.<HW INPUT PORT NAME>.poke(<YOUR DATA NAME>)"
        c.io.reset.poke(true.B)
        // Apply clock with "c.clock.step(<CYCLE NUM>)"
        c.clock.step(1)
        // Read HW's Outputs with "print(c.io.<HW OUTPUT PORT NAME>.peek())"
        c.io.reset.poke(false.B)
        
        for(i <- 0 until nInputs)
          c.io.inputs(i).poke(inputs(i).U)

        for(i <- 1 to steps) {
          c.clock.step(1)
          println("Cycle["+ i +"]: " + c.io.output.peek().litValue())
        }
        
        println(">> Answer: "+output)
    }
  }
}
