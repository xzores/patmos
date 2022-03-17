import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest.experimental.TestOptionBuilder._

import SPI_memory._

class hello_verilator_test extends AnyFlatSpec with ChiselScalatestTester
{
  "Hello verilator test" should "pass" in {
    test(new hello).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.io.in.poke(true.B)
      dut.clock.step()
      dut.io.out.expect(false.B)
    }
  }
}
