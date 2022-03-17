package util

import chisel3._
import chisel3.experimental.Analog
import chisel3.util.HasBlackBoxResource


class BlackBoxFlash extends HasBlackBoxResource {
  val io = IO(new Bundle {
    val SI = Analog(Bool())
    val SO = Analog(Bool())
    val SCK = Input(Bool())
    val CSNeg = Input(Bool())
    val WPNeg = Analog(Bool())
    val ResetNeg = Analog(Bool())

  })
  addResource("flash-s25fs256.v")
}
