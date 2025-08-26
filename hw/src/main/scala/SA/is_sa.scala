package is_sa

import chisel3._
import chisel3.util._

class PE(val width: Int) extends Module {
  val io = IO(new Bundle {
    val inA = Input(UInt(width.W))   // 입력 입력값
    val inB = Input(UInt(width.W))   // 입력 가중치
    val inC = Input(UInt(32.W))   // 입력 합산 값
    val inS = Input(Bool())    // Double Buffer 스위치 신호
   
    val outA = Output(UInt(width.W))   // 입력 입력값
    val outB = Output(UInt(width.W))   // 입력 가중치
    val outC = Output(UInt(32.W)) // 다음 PE로 전달될 완성된 출력값
    val outS = Output(Bool())    // Double Buffer 스위치 신호

    val reset = Input(Bool())     // register reset
  })
  // input Register
  val inp = RegInit(0.U(8.W))
  inp := Mux(io.reset, 0.U, io.inA)
  io.outA := inp

  // weight Register
  val wgt0 = RegInit(0.U(8.W))
  val wgt1 = RegInit(0.U(8.W))
 
  wgt0 := Mux(io.reset, 0.U, Mux(io.inS, io.inB, wgt0))
  wgt1 := Mux(io.reset, 0.U, Mux(io.inS, wgt1, io.inB))

  io.outB := Mux(io.inS, wgt0, wgt1)

  // Multiply and Accumulate
  val mul = io.inA * Mux(io.inS, wgt1, wgt0)

  // Output Register Double Buffering
  val out = RegInit(0.U(32.W))

  out := Mux(io.reset, 0.U, io.inC + mul)
  io.outC := out
  
  val switch = RegInit(false.B)
  switch := Mux(io.reset, false.B, io.inS)
  io.outS := switch
}

class PErow(val rowL: Int, val width: Int) extends Module {
  val io = IO(new Bundle {
    val inA = Input(Vec(rowL, UInt(width.W)))  // "rowL"개의 입력값
    val inB = Input(UInt(width.W))  // 1개의 가중치
    val inC = Input(UInt(32.W))   // 입력 합산 값
    
    val outA = Output(Vec(rowL, UInt(width.W)))  // "rowL"개의 입력값
    val outB = Output(UInt(width.W))  // 1개의 가중치
    val outC = Output(UInt(32.W)) // 전달되는 합산값

    val inS = Input(Bool())    // Double Buffer 스위치 신호
    val reset = Input(Bool())     // register reset
  })

  // 2D Array of Processing Elements
  val perow = Seq.fill(rowL)(Module(new PE(width)))

  // Connect inputs to PEs and outputs

  for (i <- 0 until rowL) {
    perow(i).io.inA := io.inA(i)
    io.outA(i) := perow(i).io.outA
    
    if (i > 0) 
      perow(i).io.inB := perow(i - 1).io.outB
    
    if (i > 0) 
      perow(i).io.inC := perow(i - 1).io.outC
    
    if (i > 0)
      perow(i).io.inS := perow(i-1).io.outS
    
    perow(i).io.reset := io.reset
  }

    perow(0).io.inB := io.inB
    perow(0).io.inC := io.inC
    perow(0).io.inS := io.inS

    io.outB := perow(rowL-1).io.outB
    io.outC := perow(0).io.outC
}

class Tile(val rowL: Int, val colL: Int, val width: Int) extends Module {
  val io = IO(new Bundle {
    val inA = Input(Vec(rowL, UInt(width.W)))  // "rowL"개의 입력값
    val inB = Input(Vec(colL, UInt(width.W)))  // 1개의 가중치
    val inC = Input(Vec(colL, UInt(32.W)))   // 입력 합산 값
    val inS = Input(Vec(colL, Bool()))   // 입력 합산 값

    val outC = Output(Vec(colL, UInt(32.W))) // 전달되는 합산값
    val reset = Input(Bool())     // register reset
  })

  // 2D Array of Processing Elements
  val tile = Seq.fill(colL)(Module(new PErow(rowL, width)))

  for (i <- 0 until colL){
    for (j <- 0 until rowL) {
      if (i > 0)
        tile(i).io.inA(j) := tile(i-1).io.outA(j)
      else
        tile(i).io.inA(j) := io.inA(j)
    }

    tile(i).io.inB := io.inB(i)

    tile(i).io.inC := io.inC(i)

    tile(i).io.inS := io.inS(i)

    tile(i).io.reset := io.reset

    io.outC(i) := tile(i).io.outC
  }
}

object Tile {
  def apply(width:Int=8, rowL:Int=8,colL:Int=8): Tile = {
    new Tile(rowL,colL,width)
  }
}

object IS_SA_To_Verilog extends App {
  val rowL = 16
  val colL = 16
  val targetDir = "원하는 폴더 위치로 수정하시오"
  val top = "Tile"

  if (top == "Tile") {
    val verilogFileName = s"General_Tile_${rowL}_${colL}.v"  // 동적 파일명 생성
    println("\nGenerating Verilog of...",verilogFileName,"\n")
    (new chisel3.stage.ChiselStage).emitVerilog(
      Tile(rowL=rowL,colL=colL),
      Array("--target-dir", targetDir, "--output-file", verilogFileName)
    )
  }
}
