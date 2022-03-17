package SPI_memory

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


class Software_Memory_Sim(dut: OCPburst_SPI_memory, var CE : Bool, var MOSI : Bool, var MISO : Bool) {

  var in_bits : Array[Boolean] = new Array[Boolean](8);
  var bits_read : Int = 0;

  def step (n : Int = 1) : Unit = {
    print("stepped: "); println(MOSI.peek());

    for( a <- 0 to n){
      dut.clock.step();
      if(CE.peek() == 0){
        //We are ready to recive data
        val MOSI_val : Boolean= MOSI.peek().litToBoolean;
        in_bits(bits_read) = MOSI_val;
        bits_read = bits_read + 1;
      }
    }

  };
}

class OCPburst_SPI_memory_test extends AnyFlatSpec with ChiselScalatestTester
{
  "Write read test software" should "pass" in {
    test(new OCPburst_SPI_memory()) { dut =>

      val master = dut.io.OCP_interface.M
      val slave = dut.io.OCP_interface.S

      val Software_Memory_Sim = new Software_Memory_Sim(dut, dut.io.CE, dut.io.MOSI, dut.io.MISO);

      master.Cmd.poke(OcpCmd.IDLE)
      master.Addr.poke(0.U)
      master.Data.poke(0.U)
      master.DataByteEn.poke(0xFF.U) //it just cuts the bits?? maybe?

      Software_Memory_Sim.step();

      master.Cmd.poke(OcpCmd.RD)
      master.Addr.poke(0.U)
      master.Data.poke(51.U)
      master.DataByteEn.poke(0xFF.U) //it just cuts the bits?? maybe?

      Software_Memory_Sim.step();

      slave.CmdAccept.expect(true.B);
      slave.Resp.expect(0.U);

      master.Cmd.poke(OcpCmd.IDLE)
      master.Addr.poke(0.U)
      master.Data.poke(0.U)
      master.DataByteEn.poke(0xFF.U) //it just cuts the bits?? maybe?

      Software_Memory_Sim.step();
      //CE IS LOW HERE

      Software_Memory_Sim.step(100);

      //  println("Result is: " + dut.io.out.peek().toString)

    }
  }
}

