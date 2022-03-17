package SPI_memory

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class OCPburst_SPI_memory_test extends AnyFlatSpec with ChiselScalatestTester
{
  "Write read test software" should "pass" in {
    test(new OCPburst_SPI_memory()) { dut =>

      val master = dut.io.OCP_interface.M;

      master.Cmd.poke(OcpCmd.IDLE);
      master.Addr.poke(0U);
      //master.Data.poke();

      dut.clock.step ()
      //  println("Result is: " + dut.io.out.peek().toString)

    }
  }
}

