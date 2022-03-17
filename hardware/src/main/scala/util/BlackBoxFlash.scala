package util

import chisel3._
import chisel3.experimental.Analog
import chisel3.util.HasBlackBoxResource


class BlackBoxFlash extends HasBlackBoxResource {
  val io = IO(new Bundle {
    val SI = Analog(1.W)
    val SO = Analog(1.W)
    val SCK = Input(UInt(1.W))
    val CSNeg = Input(UInt(1.W))
    val WPNeg = Analog(1.W)
    val ResetNeg = Analog(1.W)

  })
  addResource("flash-s25fs256.v")
}
