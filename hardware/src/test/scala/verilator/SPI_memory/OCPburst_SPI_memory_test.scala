package verilator.SPI_memory

import SPI_memory.OCPburst_SPI_memory
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import ocp._
import org.scalatest.flatspec.AnyFlatSpec
import treadle.WriteVcdAnnotation
import chisel3.experimental.chiselName

class Software_Memory_Sim(dut: OCPburst_SPI_memory) {

  var in_bits : Array[Boolean] = new Array[Boolean](8);
  var bits_read : Int = 0;

  def step (n : Int = 1) : Unit = {

    for( a <- 0 to n-1){

      dut.clock.step();

      /*
      if(dut.io.CE.peek().litValue() == 0){
        //We are ready to recive data
        val MOSI_val : Boolean= dut.io.MOSI.peek().litToBoolean;
        in_bits(bits_read) = MOSI_val;
        bits_read = bits_read + 1;
        if(bits_read >= 8){
          bits_read = 0;
        }
        //convert_to_byte
      }
      */

    }
  };
}

class OCPburst_SPI_memory_test extends AnyFlatSpec with ChiselScalatestTester
{
  "Read OCP test software" should "pass" in {
    test(new OCPburst_SPI_memory()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      val master = dut.io.OCP_interface.M
      val slave = dut.io.OCP_interface.S

      val Software_Memory_Sim = new Software_Memory_Sim(dut);

      //////// clock cycle 1 /////////
      master.Cmd.poke(OcpCmd.IDLE)
      master.Addr.poke(0.U)
      master.Data.poke(0.U)
      master.DataByteEn.poke(0xFF.U) //it just cuts the bits?? maybe?

      dut.io.SR.expect(0.U);

      Software_Memory_Sim.step(); //A
      ////////////////////////////////

      //////// clock cycle 2 /////////
      master.Cmd.poke(OcpCmd.RD)
      master.Addr.poke(0.U)
      master.Data.poke(0.U)
      master.DataByteEn.poke(0xFF.U) //it just cuts the bits?? maybe?

      slave.Resp.expect(0.U);
      dut.io.SR.expect(0.U);

      Software_Memory_Sim.step(); //B

      slave.CmdAccept.expect(true.B);
      slave.Resp.expect(OcpResp.NULL);
      dut.io.SR.expect(1.U);
      ///////////////////////////////////////////////


      master.Cmd.poke(OcpCmd.IDLE)
      master.Addr.poke(0.U)
      master.Data.poke(0.U)
      master.DataByteEn.poke(0xFF.U) //it just cuts the bits?? maybe?
      dut.io.CntReg.expect(0.U);

      while(slave.Resp.peek().litValue() != OcpResp.DVA.litValue()) {
        //dut.io.SR.expect(1.U);
        //dut.io.SPI_DATA_VALUD.expect(false.B);

        if(slave.Resp.peek().litValue() == OcpResp.NULL.litValue()){
          Software_Memory_Sim.step();
          println("stepped, while waiting for data ready")
        }
        else if(slave.Resp.peek().litValue() == OcpResp.ERR.litValue()) {
          fail()
        }
        else if(slave.Resp.peek().litValue() == OcpResp.FAIL.litValue()){
          fail()
        }
        else if(slave.Resp.peek().litValue() == OcpResp.DVA.litValue()){
          println("DVA is here?? should not be here")
        }
        else {
          println(slave.Resp.peek())
          fail()
        }
      };

      dut.io.CntReg.expect(0.U)
      dut.io.SR.expect(3.U)
      slave.Resp.expect(OcpResp.DVA) //2

      Software_Memory_Sim.step()
      dut.io.SR.expect(3.U)
      dut.io.CntReg.expect(1.U)
      slave.Resp.expect(OcpResp.DVA) //3

      Software_Memory_Sim.step()
      dut.io.CntReg.expect(2.U)
      dut.io.SR.expect(3.U)
      slave.Resp.expect(OcpResp.DVA) //4

      Software_Memory_Sim.step()
      dut.io.CntReg.expect(3.U)
      dut.io.SR.expect(3.U)
      slave.Resp.expect(OcpResp.DVA) //4

      Software_Memory_Sim.step()
      slave.Resp.expect(OcpResp.NULL) //5

      //  println("Result is: " + dut.io.out.peek().toString)
    }
  }

  "Write OCP test software" should "pass" in {
    test(new OCPburst_SPI_memory()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      val master = dut.io.OCP_interface.M
      val slave = dut.io.OCP_interface.S

      val Software_Memory_Sim = new Software_Memory_Sim(dut);

      //////// clock cycle 1 /////////
      master.Cmd.poke(OcpCmd.IDLE)
      master.Addr.poke(0.U)
      master.Data.poke(0.U)
      master.DataByteEn.poke(0xFF.U) //it just cuts the bits?? maybe?

      dut.io.SR.expect(0.U);

      Software_Memory_Sim.step();
      ////////////////////////////////

      //////// clock cycle 2+ /////////
      master.Cmd.poke(OcpCmd.WR)
      master.Addr.poke(414.U)
      master.Data.poke(1241551.U)
      master.DataByteEn.poke(0xFF.U) //it just cuts the bits?? maybe?
      master.DataValid.poke(1.U)
      slave.Resp.expect(OcpResp.NULL)
      slave.DataAccept.expect(false.B)

      slave.Resp.expect(0.U)
      dut.io.SR.expect(0.U)

      while(slave.CmdAccept.peek().litValue() != 1){
        Software_Memory_Sim.step()
        slave.Resp.expect(OcpResp.NULL)
      }
      slave.DataAccept.expect(true.B)
      slave.CmdAccept.expect(true.B)

      ///////////////////////////////

      //////// next step /////////
      master.Cmd.poke(OcpCmd.IDLE)
      master.Addr.poke(0.U)
      master.Data.poke(4321.U)
      master.DataByteEn.poke(0x00.U) //it just cuts the bits?? maybe?

      Software_Memory_Sim.step()
      slave.DataAccept.expect(true.B)
      slave.CmdAccept.expect(false.B)

      //////// next step /////////

      Software_Memory_Sim.step()
      master.Data.poke(54321.U)
      slave.DataAccept.expect(true.B)
      slave.CmdAccept.expect(false.B)

      //////// next step /////////

      Software_Memory_Sim.step()
      master.Data.poke(123456.U)
      slave.DataAccept.expect(true.B)
      slave.CmdAccept.expect(false.B)
      master.DataValid.poke(0.U)

      //////// next step /////////

      Software_Memory_Sim.step()
      slave.DataAccept.expect(false.B)
      slave.CmdAccept.expect(false.B)
      slave.Resp.expect(OcpResp.DVA);

      Software_Memory_Sim.step()
      slave.Resp.expect(OcpResp.NULL);

    }
  }

  "Write read test software" should "pass" in {
    test(new OCPburst_SPI_memory()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>


    }
  }


}

