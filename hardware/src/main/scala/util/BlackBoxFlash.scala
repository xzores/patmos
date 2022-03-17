package util

import chisel3._
import chisel3.util.HasBlackBoxResource


class BlackBoxFlash extends HasBlackBoxResource {
  val io = IO(new Bundle {})
  addResource("flash-s25fs256.v")
}
