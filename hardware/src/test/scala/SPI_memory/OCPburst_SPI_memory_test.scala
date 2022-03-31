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

class Memory_helper_functions{

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

}

class Software_Memory_Sim(dut: OCPburst_SPI_memory, fail_callback: () => Unit) {

  var in_bits : Array[Boolean] = new Array[Boolean](8)
  var bits_read : Int = 0

  var state = STATE.NULL;
  var clock_cycle = 0;

  var funcs : Memory_helper_functions = new Memory_helper_functions();

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

  def step (n : Int = 1) : Unit = {


    for( a <- 0 to n-1){

      dut.clock.step();

      if(funcs.rising_edge(dut.io.S_CLK.peek().litToBoolean)){

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
            val in_val : Char = funcs.bitsToByte(in_bits);
            println(Console.BLUE + "in_val was: 0x" + in_val.toHexString + Console.RESET)
            handle_byte(in_val);
          }
        }
      }

    }
  };
}

class OCP_master_commands(master : OcpBurstMasterSignals, slave : OcpBurstSlaveSignals, step: (Int) => Unit, fail_callback: () => Unit) {

  var funcs : Memory_helper_functions = new Memory_helper_functions();

  val r = new scala.util.Random(System.currentTimeMillis());

  def randomize_read_dont_cares(): Unit ={
    master.Data.poke(r.nextInt(Integer.MAX_VALUE).U)
    master.DataByteEn.poke(r.nextInt(Integer.MAX_VALUE).U) //it just cuts the bits?? maybe?
  }

  def read_step(): Unit ={
    randomize_read_dont_cares();
    step(1);
  }

  def write_step(): Unit ={
    step(1);
  }

  def read_command(address : Int): Array[Int] ={

    var values = new Array[Int](4);

    master.Cmd.poke(OcpCmd.IDLE)
    master.Addr.poke(0.U)
    slave.Resp.expect(OcpResp.NULL);
    read_step();

    master.Cmd.poke(OcpCmd.RD)
    master.Addr.poke(address.U)
    slave.Resp.expect(OcpResp.NULL)
    read_step()

    master.Cmd.poke(OcpCmd.IDLE)
    master.Addr.poke(0.U)
    slave.Resp.expect(OcpResp.NULL);
    slave.CmdAccept.expect(true.B);
    read_step();


    while(slave.Resp.peek().litValue() != OcpResp.DVA.litValue()) {
      if(slave.Resp.peek().litValue() == OcpResp.NULL.litValue()){
        read_step();
      }
      else if(slave.Resp.peek().litValue() == OcpResp.ERR.litValue()) {
        fail_callback()
      }
      else if(slave.Resp.peek().litValue() == OcpResp.FAIL.litValue()){
        fail_callback()
      }
      else {
        fail_callback()
      }
    };

    slave.Resp.expect(OcpResp.DVA) //1
    values(0) = slave.Data.peek().litValue().intValue();
    read_step();

    slave.Resp.expect(OcpResp.DVA) //2
    values(1) = slave.Data.peek().litValue().intValue();
    read_step();

    slave.Resp.expect(OcpResp.DVA) //3
    values(2) = slave.Data.peek().litValue().intValue();
    read_step();

    slave.Resp.expect(OcpResp.DVA) //4
    values(3) = slave.Data.peek().litValue().intValue();
    read_step();

    slave.Resp.expect(OcpResp.NULL) //5

    return values
  }

  def write_command(address : Int, data : Array[BigInt], byte_en : Array[Array[Boolean]]): Unit ={

    master.Cmd.poke(OcpCmd.IDLE)
    master.Addr.poke(0.U)
    master.Data.poke(0.U)
    master.DataByteEn.poke(0x0.U)
    master.DataValid.poke(0.U)
    slave.Resp.expect(OcpResp.NULL)
    master.DataValid.poke(false.B);
    write_step()

    master.Cmd.poke(OcpCmd.WR)
    master.Addr.poke(address.U)
    master.Data.poke(data(0).U)
    master.DataByteEn.poke(0xF.U); //TODO use byte_en
    master.DataValid.poke(1.U)
    slave.Resp.expect(OcpResp.NULL)
    master.DataValid.poke(true.B);
    slave.DataAccept.expect(false.B)

    while(slave.CmdAccept.peek().litValue() != 1){
      write_step()
      slave.Resp.expect(OcpResp.NULL)
    }

    slave.DataAccept.expect(true.B)
    slave.CmdAccept.expect(true.B)
    master.Cmd.poke(OcpCmd.IDLE)
    master.Addr.poke(0.U)
    master.Data.poke(data(1).U)
    master.DataValid.poke(true.B);
    master.DataByteEn.poke(0xF.U) //TODO use byte_en
    write_step()

    slave.Resp.expect(OcpResp.NULL)
    slave.DataAccept.expect(true.B)
    master.Cmd.poke(OcpCmd.IDLE)
    master.Addr.poke(0.U)
    master.Data.poke(data(2).U)
    master.DataValid.poke(true.B);
    master.DataByteEn.poke(0xF.U) //TODO use byte_en
    write_step()

    slave.Resp.expect(OcpResp.NULL)
    slave.DataAccept.expect(true.B)
    master.Cmd.poke(OcpCmd.IDLE)
    master.Addr.poke(0.U)
    master.Data.poke(data(3).U)
    master.DataValid.poke(true.B);
    master.DataByteEn.poke(0xF.U) //TODO use byte_en
    write_step()

    slave.Resp.expect(OcpResp.DVA)
    slave.DataAccept.expect(false.B)
    master.Cmd.poke(OcpCmd.IDLE)
    master.Addr.poke(0.U)
    master.Data.poke(0.U)
    master.DataValid.poke(false.B);
    master.DataByteEn.poke(0x0.U)
    write_step()

    slave.Resp.expect(OcpResp.NULL)
    slave.DataAccept.expect(false.B)
    master.Cmd.poke(OcpCmd.IDLE)
    master.Addr.poke(0.U)
    master.Data.poke(0.U)
    master.DataValid.poke(false.B);
    master.DataByteEn.poke(0x0.U)
    write_step()

  }
}

class OCPburst_SPI_memory_test extends AnyFlatSpec with ChiselScalatestTester
{
  "Read OCP test software" should "pass" in {
    test(new OCPburst_SPI_memory()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.clock.setTimeout(20000);

      val Software_Memory_Sim = new Software_Memory_Sim(dut, fail);
      Software_Memory_Sim.step(100);

      val master = dut.io.OCP_interface.M
      val slave = dut.io.OCP_interface.S

      val ocp_tester = new OCP_master_commands(master, slave, Software_Memory_Sim.step, fail);
      ocp_tester.read_command(13512);
      Software_Memory_Sim.step(10);
      ocp_tester.read_command(1342);
      Software_Memory_Sim.step(100);
      ocp_tester.read_command(23456);
      ocp_tester.read_command(54321);
      ocp_tester.read_command(23456);

    }
  }

  "Write OCP test software" should "pass" in {
    test(new OCPburst_SPI_memory()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.clock.setTimeout(10000);

      val Software_Memory_Sim = new Software_Memory_Sim(dut, fail);
      Software_Memory_Sim.step(100);

      val master = dut.io.OCP_interface.M
      val slave = dut.io.OCP_interface.S

      val ocp_tester = new OCP_master_commands(master, slave, Software_Memory_Sim.step, fail);
      ocp_tester.write_command(141, Array(14, 1245, 114, 124), Array(Array()));

    }
  }

  "Write read test software" should "pass" in {
    test(new OCPburst_SPI_memory()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>


    }
  }


}

