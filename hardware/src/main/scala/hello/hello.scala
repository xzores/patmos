package SPI_memory

import chisel3._
import chisel3.util._

class hello extends Module {
    val io = IO(new Bundle {
        val in = Input(Bool())
        val out = Output(Bool())
    })

    io.out := RegNext(~io.in)
}

