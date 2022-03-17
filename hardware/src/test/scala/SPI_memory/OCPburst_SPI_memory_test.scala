package SPI_memory

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class OCPburst_SPI_memory_test extends AnyFlatSpec with ChiselScalatestTester
{
  "Write read test software" should "pass" in {
    test(new OCPburst_SPI_memory()) { dut =>

      dut.io.OCP_interface.CmdAccept.poke();


      dut.clock.step ()
      //  println("Result is: " + dut.io.out.peek().toString)

    }
  }
}

