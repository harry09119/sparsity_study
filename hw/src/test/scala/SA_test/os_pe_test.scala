package os_sa

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.util._
import scala.collection.mutable
import scala.util.Random

class OS_SA_Test extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Tile" 
  def SwitchArray(n: Int, m: Int, k: Int): Array[Array[Boolean]] = {
      Array.tabulate(n, m) { (_, j) => ((j / k) % 2) == 1
    }
  }

  def ToSystolic(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    val n = matrix.length
    val m = matrix.head.length
    Array.tabulate(n, m + n - 1) { (i, j) =>
      if (j >= i && j < i + m) matrix(i)(j - i) else 0
    }
  }

  def ToSystolic_B(matrix: Array[Array[Boolean]]): Array[Array[Boolean]] = {
    val n = matrix.length
    val m = matrix.head.length
    Array.tabulate(n, m + n - 1) { (i, j) =>
      if (j >= i && j < i + m) matrix(i)(j - i) else false
    }
  }
  it should "produce right output" in {
    //Add your own functions here
    //Add your own values here
    val s = 4
    val width = 8
    test(
      //S x S의 Systolic Array 선언
      new Tile(s,s,width)
    ) { c =>
        // Prepare Data
        val n = s
        val m = s
        val k = 8
        val weight: Array[Array[Int]] = Array.tabulate(n, k)((i, j) => i)
        val input : Array[Array[Int]] = Array.tabulate(m, k)((i, j) => i)


        val wgt_sys = ToSystolic(weight)
        val inp_sys = ToSystolic(input)
        val swt_sys = ToSystolic_B(SwitchArray(n,k+s,k))

        c.io.reset.poke(true.B)
        c.clock.step(1)

        c.io.reset.poke(false.B)

        //행렬 전달(k+s-1) + 완성된 Output 전달(n)
        val total_cycle = k + s - 1 + s
        for (cycle <- 0 until total_cycle) {
          //입력 행렬 전달 중
          if (cycle < k + s - 1) {
            for (col <- 0 until s) 
              c.io.inA(col).poke(inp_sys(col)(cycle).U)
          
            for (row <- 0 until s) {
              c.io.inB(row).poke(wgt_sys(row)(cycle).U)
            }
          }
          
          //입력 행렬 전달 완료. Output 저장을 대기 중
          else {
            for (col <- 0 until s) 
              c.io.inA(col).poke(0.U)
          
            for (row <- 0 until s) {
              c.io.inB(row).poke(0.U)
            }
          }
          //Double Buffering 관리 중
          for (row <- 0 until s) {
            c.io.inS(row).poke(swt_sys(row)(cycle).B)
          }
        }
      }
  }
}
