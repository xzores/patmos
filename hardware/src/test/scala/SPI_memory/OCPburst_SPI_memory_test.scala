package SPI_memory

import SPI_memory.OCPburst_SPI_memory
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import ocp._
import org.scalatest.flatspec.AnyFlatSpec
import treadle.WriteVcdAnnotation
import chisel3.experimental.chiselName

object STATE extends Enumeration {
  type STATE = Value
  val NULL, RESET_ENABLE, RESET, READ, WRITE = Value

}

class Software_Memory_Sim(dut: OCPburst_SPI_memory, fail_callback: () => Unit) {

  var in_bits : Array[Boolean] = new Array[Boolean](8)
  var bits_read : Int = 0

  var state = STATE.NULL;
  var clock_cycle = 0;

  var last_clock = false

  def bitsToByte(bits : Array[Boolean]) : Char = {
    var amount = 0;
    for(i <- bits){

      amount = amount << 1;
      if(i) {
        amount = amount + 1
      };
    }
    return amount.toChar;
  }

  def handle_byte(b : Char): Unit = {

    if (state == STATE.NULL || state == STATE.RESET_ENABLE){
      if(b == 0x66)
        state = STATE.RESET_ENABLE
      else if(b == 0x99)
        state = STATE.RESET
      else {
        println(Console.RED + "invalid byte was sent, state was: NULL/RESET_ENABLE, while bytes recived was 0x" + b.toHexString + Console.RESET);
        fail_callback()
      };
    }
    else if(state == STATE.RESET){
      if(b == 0x03) //read state
        state = STATE.READ
      else if(b == 0x02) //write state
        state = STATE.WRITE
      else if(b == 0x66)
        state = STATE.RESET_ENABLE
      else if(b == 0x99)
        state = STATE.RESET
      else {
        println(Console.RED + "invalid byte was sent, state was: RESET, while bytes recived was 0x" + b.toHexString + Console.RESET);
        fail_callback()
      };
    }
    if(state == STATE.READ){

    }
    if(state == STATE.WRITE){

    }

  }

  def rising_edge(b : Boolean): Boolean ={

    val last = last_clock
    last_clock = b;

    if(b == false)
      return false;
    else{
      if(last == true)
        return false;
      else
        return true
    }
  }

  def step (n : Int = 1) : Unit = {


    for( a <- 0 to n-1){

      dut.clock.step();

      if(rising_edge(dut.io.S_CLK.peek().litToBoolean)){

        if(dut.io.CE.peek().litValue() == 0 && dut.reset.peek().litValue() == 0){
          dut.io.CE.expect(false.B);

          if(dut.io.CE.peek().litValue() == 1){
            if(bits_read != 0){
              println(Console.RED + "#CE must be hold high for the entire operation, minimum 8 bits at a time, was was pulled high "
                + (bits_read + 1) + " bits in" + Console.RESET);
              fail_callback()
            }
          }

          //We are ready to recive data
          val MOSI_val : Boolean = dut.io.MOSI.peek().litToBoolean;

          //println(Console.BLUE + "Chip clock index " + (clock_cycle + 1) + ", MOSI was: " + MOSI_val + Console.RESET);
          clock_cycle = clock_cycle + 1;

          in_bits(bits_read) = MOSI_val;
          bits_read = bits_read + 1;
          if(bits_read >= 8){
            bits_read = 0;
            val in_val : Char = bitsToByte(in_bits);
            println(Console.BLUE + "in_val was: 0x" + in_val.toHexString + Console.RESET)
            handle_byte(in_val);
          }
        }
      }

    }
  };
}

class OCPburst_SPI_memory_test extends AnyFlatSpec with ChiselScalatestTester
{
  "Read OCP test software" should "pass" in {
    test(new OCPburst_SPI_memory()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.clock.setTimeout(10000);

      val master = dut.io.OCP_interface.M
      val slave = dut.io.OCP_interface.S

      val Software_Memory_Sim = new Software_Memory_Sim(dut, fail);
      Software_Memory_Sim.step(100);

      //////// clock cycle 1 /////////
      master.Cmd.poke(OcpCmd.IDLE)
      master.Addr.poke(123.U)
      master.Data.poke(0.U)
      master.DataByteEn.poke(0xFF.U) //it just cuts the bits?? maybe?

      dut.io.SR.expect(0.U);

      //////// clock cycle 2 /////////
      master.Cmd.poke(OcpCmd.RD)
      master.Addr.poke(123.U)
      master.Data.poke(0.U)
      master.DataByteEn.poke(0xFF.U) //it just cuts the bits?? maybe?

      slave.Resp.expect(0.U);
      dut.io.SR.expect(0.U);

      Software_Memory_Sim.step();

      slave.CmdAccept.expect(true.B);
      slave.Resp.expect(OcpResp.NULL);
      dut.io.SR.expect(1.U);
      ///////////////////////////////////////////////

      master.Cmd.poke(OcpCmd.IDLE)
      master.Addr.poke(123.U)
      master.Data.poke(0.U)
      master.DataByteEn.poke(0xFF.U) //it just cuts the bits?? maybe?
      dut.io.CntReg.expect(0.U);

      var count = 0;
      val max_count = 5000;
      while(slave.Resp.peek().litValue() != OcpResp.DVA.litValue() && count < max_count) {
        count = count + 1;

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
      if(count >= max_count-1) {
        println(Console.RED + "Too many steps, we stepped for " + (count) + " clocks" + Console.RESET)
        fail();
      };


      dut.io.CntReg.expect(0.U)
      dut.io.SR.expect(3.U)
      slave.Resp.expect(OcpResp.DVA) //2

      dut.clock.step();
      dut.io.SR.expect(3.U)
      dut.io.CntReg.expect(1.U)
      slave.Resp.expect(OcpResp.DVA) //3

      dut.clock.step();
      dut.io.CntReg.expect(2.U)
      dut.io.SR.expect(3.U)
      slave.Resp.expect(OcpResp.DVA) //4

      dut.clock.step();
      dut.io.CntReg.expect(3.U)
      dut.io.SR.expect(3.U)
      slave.Resp.expect(OcpResp.DVA) //4

      dut.clock.step();
      slave.Resp.expect(OcpResp.NULL) //5

      //  println("Result is: " + dut.io.out.peek().toString)
    }
  }

  /*
  "Write OCP test software" should "pass" in {
    test(new OCPburst_SPI_memory()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      val master = dut.io.OCP_interface.M
      val slave = dut.io.OCP_interface.S

      val Software_Memory_Sim = new Software_Memory_Sim(dut, fail);

      //////// clock cycle 1 /////////
      master.Cmd.poke(OcpCmd.IDLE)
      master.Addr.poke(0.U)
      master.Data.poke(0.U)
      master.DataByteEn.poke(0x0.U) //it just cuts the bits?? maybe?

      dut.io.SR.expect(0.U);

      for( a <- 0 to 100){
        Software_Memory_Sim.step();
      };

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
      master.DataByteEn.poke(0xFF.U)

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

      //////// next step /////////

      Software_Memory_Sim.step()
      master.DataByteEn.poke(0x00.U)
      //slave.DataAccept.expect(false.B)
      //slave.CmdAccept.expect(false.B)

      master.Data.poke(0.U)
      master.DataValid.poke(0.U)

      while(slave.Resp.peek().litValue() != OcpResp.DVA.litValue()){
        Software_Memory_Sim.step();
      }

      Software_Memory_Sim.step()
      slave.Resp.expect(OcpResp.NULL);

    }
  }

  "Write read test software" should "pass" in {
    test(new OCPburst_SPI_memory()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>


    }
  }
  */

}

