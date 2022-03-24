package verilator

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest.experimental.TestOptionBuilder._

import hello._

class hello_verilator_test extends AnyFlatSpec with ChiselScalatestTester
{
  "hello" should "pass" in {
    try{
    test(new hello).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.io.in.poke(false.B)
      dut.clock.step()
      dut.io.out.expect(true.B)
    }
    }
    catch {
      case e: java.io.IOException => fail(); println("Failed to run varilator test, do you have verilator installed (linux only)");
    }
  }
}
