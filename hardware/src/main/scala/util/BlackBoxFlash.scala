package util

import chisel3._
import chisel3.experimental.Analog
import chisel3.util._
import chisel3.util.HasBlackBoxResource

class BlackBoxFlash extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle{
    val SI = Analog(1.W)
    val SO = Analog(1.W)
    val SCK = Input(Bool())
    val CSNeg = Input(Bool())
    val WPNeg = Analog(1.W)
    val ResetNeg = Analog(1.W)
  })
  addResource("/flash-s25fs256s.v")
}

