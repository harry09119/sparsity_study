package base

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

class Base() extends Module {
  val io = IO(new Bundle {
    //Add your IO here
    val reset = Input(Bool())
    val next = Input(Bool())
    val output = Output(UInt(4.W)) 
  })
  
  //Add your HW operation here
  object State extends ChiselEnum {
    val idle = Value
    val first, second, third = Value
  }

  import State._

  val state = RegInit(idle)

  when(io.reset) {
    state := idle
  }.otherwise {
    when(io.next) {
      state := state.next
    }.otherwise {
      state := state
    }
  }

  when(state === idle) {
    io.output := 0.U(4.W)
  }.elsewhen(state === first) {
    io.output := 1.U(4.W)
  }.elsewhen(state === second) {
    io.output := 2.U(4.W)
  }.elsewhen(state === third) {
    io.output := 3.U(4.W)
  }.otherwise {
    io.output := 15.U(4.W)
  }

}
