package sparse_sa

import chisel3._
import chisel3.util._
import scala.math._
import chisel3.util.log2Ceil

class SpPE(val width: Int, val parallel:Int) extends Module {
  def log2(x: Int): Int = log2Ceil(x)
  val io = IO(new Bundle {
    val inA = Input(Vec(parallel, UInt(width.W)))   // 입력 입력값
    val inB = Input(UInt(width.W))   // 입력 가중치 
    val tag = Input(UInt(log2(parallel).W))
    val inC = Input(UInt(32.W))   // 입력 합산 값
    val outC = Output(UInt(32.W)) // 다음 PE로 전달될 완성된 출력값
    val switch = Input(Bool())    // Double Buffer 스위치 신호
    val reset = Input(Bool())     // register reset
    
    //val debug = Output(Vec(2,UInt(32.W)))
  })
  // 두 개의 출력 레지스터
  val out0 = RegInit(0.U(32.W))
  val out1 = RegInit(0.U(32.W))

  // Multiply and Accumulate
  val mul = io.inA(io.tag) * io.inB

  out0 := Mux(io.reset, 0.U, Mux(io.switch, out0 + mul, io.inC))
  out1 := Mux(io.reset, 0.U, Mux(io.switch, io.inC, out1 + mul))

  io.outC := Mux(io.switch, out1, out0)
  //io.debug := Seq(out0, out1)
}

object SpPE {
  def apply(width:Int=8, parallel: Int=4): SpPE = {
    new SpPE(width, parallel)
  }
}

class SpPErow(val parallel:Int, val rowL: Int, val width: Int, val o_speed: Int) extends Module {
  def log2(x: Int): Int = log2Ceil(x)
  val io = IO(new Bundle {
    val inA = Input(Vec(rowL, Vec(parallel, UInt(width.W))))  // "rowL"개의 입력값
    val inB = Input(UInt(width.W))  // 1개의 가중치
    val tag = Input(UInt(log2(parallel).W))
    val inC = Input(Vec(o_speed, UInt(32.W)))   // 입력 합산 값
    val outC = Output(Vec(o_speed, UInt(32.W))) // 전달되는 합산값
    val switch = Input(Bool())    // Double Buffer 스위치 신호
    val reset = Input(Bool())     // register reset
    //val debug = Output(Vec(rowL, Vec(2, UInt(32.W))))
  })

  // 2D Array of Processing Elements
  val perow = Seq.fill(rowL)(Module(SpPE(width=width, parallel = parallel)))

  // Connect inputs to PEs and outputs

  for (i <- 0 until rowL) {
    for (j <- 0 until parallel)
      perow(i).io.inA(j) := io.inA(i)(j)  // 행 단위로 inA 연결
    perow(i).io.inB := io.inB       // 열 단위로 inB 연결
    perow(i).io.tag := io.tag
    
    if (i >= o_speed){ 
      perow(i - o_speed).io.inC := perow(i).io.outC  // 이전 PE의 출력 연결
      if (i >= rowL-o_speed)
        perow(i).io.inC := io.inC(rowL-i-1)
    }
    else
      io.outC(i) := perow(i).io.outC

    perow(i).io.switch := io.switch
    perow(i).io.reset := io.reset

    //io.debug(i)(0) := perow(i).io.debug(0)
    //io.debug(i)(1) := perow(i).io.debug(1)
  }
}

object SpPErow {
  def apply(width:Int=8, parallel:Int=4, rowL:Int=8, o_speed:Int=1): SpPErow = {
    new SpPErow(parallel, rowL, width, o_speed)
  }
}

class SpTile(val rowL: Int, val colL: Int, val width: Int, val parallel: Int, val o_speed:Int) extends Module {
  val io = IO(new Bundle {
    val inA = Input(Vec(rowL, Vec(parallel, UInt(width.W))))  // "rowL"개의 입력값
    val inB = Input(Vec(colL, UInt(width.W)))  // 1개의 가중치
    val tag = Input(Vec(colL, UInt(log2Ceil(parallel).W)))
    val inC = Input(Vec(colL, Vec(o_speed, UInt(32.W))))   // 입력 합산 값
    val outC = Output(Vec(colL, Vec(o_speed, UInt(32.W)))) // 전달되는 합산값
    val switch = Input(Bool())    // Double Buffer 스위치 신호
    val reset = Input(Bool())     // register reset
  })

  // 2D Array of Processing Elements
  val tile = Seq.fill(colL)(Module(SpPErow(rowL=rowL,parallel=parallel, width=width, o_speed=o_speed)))
  
  for (c <- 0 until colL) {
    for (i <- 0 until rowL) {
      for (j <- 0 until parallel)
        tile(c).io.inA(i)(j) := io.inA(i)(j)  // 행 단위로 inA 연결
    }
  }
  
  for (i <- 0 until colL){
    tile(i).io.inB := io.inB(i)       // 열 단위로 inB 연결
    tile(i).io.tag := io.tag(i)
    tile(i).io.switch := io.switch
    tile(i).io.reset := io.reset
    
    for (j <- 0 until o_speed) {
      io.outC(i)(j) := tile(i).io.outC(j)
      tile(i).io.inC(j) := io.inC(i)(j)
    }
  }
}

object SpTile {
  def apply(width:Int=8, parallel:Int=4, rowL:Int=8,colL:Int=8, o_speed:Int=1): SpTile = {
    new SpTile(rowL,colL,width,parallel,o_speed)
  }
}

object To_Verilog extends App {
  val rowL = 16
  val colL = 16
  val parallel = 8
  val o_speed = 1
  val targetDir = "/home/harry09119/chisel/src/main/scala/SA/verilog"
  val top = "Tile"
    
  if (top == "PErow") {
    val verilogFileName = s"Sp${top}_${rowL}_${parallel}_${o_speed}.v"  // 동적 파일명 생성
    println("\nGenerating Verilog of...",verilogFileName,"\n")
    (new chisel3.stage.ChiselStage).emitVerilog(
      SpPErow(parallel = parallel, o_speed = o_speed),
      Array("--target-dir", targetDir, "--output-file", verilogFileName)
    )
  }
  
  else if (top == "PE") {
    val verilogFileName = s"Sp${top}_${parallel}.v"  // 동적 파일명 생성
    println("\nGenerating Verilog of...",verilogFileName,"\n")
    (new chisel3.stage.ChiselStage).emitVerilog(
      SpPE(parallel = parallel),
      Array("--target-dir", targetDir, "--output-file", verilogFileName)
    )
  }
  
  else if (top == "Tile") {
    val verilogFileName = s"Sp${top}_${rowL}_${colL}_${parallel}_${o_speed}.v"  // 동적 파일명 생성
    println("\nGenerating Verilog of...",verilogFileName,"\n")
    (new chisel3.stage.ChiselStage).emitVerilog(
      SpTile(rowL=rowL,colL=colL,parallel = parallel, o_speed = o_speed),
      Array("--target-dir", targetDir, "--output-file", verilogFileName)
    )
  }
}
