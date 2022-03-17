import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class OCPburst_SPI_memory_test extends AnyFlatSpec with ChiselScalatestTester
{
  "Write read test software" should "pass" in {
    test(new OCPburst_SPI_memory()) { dut =>

      dut.io.ReadAddress.poke(10.U)
      dut.io.ReadData.poke()
      dut.io.ReadCmd.poke(10.U)
      dut.io.WriteAddress.poke(10.U)
      dut.io.WriteData.poke()
      dut.io.WriteCmd.poke()

      dut.io.Valid.peek()
      dut.clock.step ()
      //  println("Result is: " + dut.io.out.peek().toString)
    }
  }
}

