package base

import chisel3._
import chisel3.util._

class Adder(width: Int) extends Module {
  val io = IO(new Bundle {
    val reset  = Input(Bool())
    val in0    = Input(UInt(width.W))
    val in1    = Input(UInt(width.W))
    val out = Output(UInt(32.W)) // +1은 carry 고려
  })

  val reg = RegInit(0.U(32.W))
  reg := Mux(io.reset, 0.U, io.in0 + io.in1)
  io.out := reg
}

class AdderTree(width: Int, nInputs: Int) extends Module {
  val io = IO(new Bundle {
    val reset  = Input(Bool())
    val inputs = Input(Vec(nInputs, UInt(width.W)))
    val output = Output(UInt((width + log2Ceil(nInputs)).W))
  })

  // 현재 레벨의 노드들을 저장하는 리스트
  var currentLevel: Seq[UInt] = io.inputs

  // 위로 올라가며 줄여가기
  while (currentLevel.length > 1) {
    currentLevel = currentLevel.grouped(2).map { case Seq(a, b) =>
      val adder = Module(new Adder(width))
      adder.io.reset := io.reset
      adder.io.in0 := a
      adder.io.in1 := b
      adder.io.out
    }.toSeq
  }

  // 최상위 루트 출력
  io.output := currentLevel.head
}

