package SPI_memory

import chisel3._
import chisel3.util._
import SPI_CMDS._

class SPI2(Count: Int, Bootup_wait : Int = 0x000F) extends Module {
  val io = IO(new Bundle {
    val ReadEnable = Input(Bool())
    val WriteEnable = Input(Bool())
    val Address = Input(UInt(24.W))
    val WriteData = Input(Vec(4,UInt(32.W)))
    val ByteEnable = Input(UInt(16.W))

    val ReadData = Output(Vec(4, UInt(32.W)))

    val DataValid = Output(Bool())
    val WriteCompleted = Output(Bool())
    val Completed = Output(Bool())

    val CE = Output(Bool())
    val MOSI = Output(Bool())
    val MISO = Input(Bool())
    val S_CLK = Output(Bool())
  })

  val DataReg = RegInit(0.U(128.W))

  io.CE := true.B
  io.MOSI := false.B
  io.DataValid := false.B
  io.Completed := false.B

  io.DataValid := false.B

  io.ReadData(0) := DataReg(127,96)
  io.ReadData(1) := DataReg(95,64)
  io.ReadData(2) := DataReg(63,32)
  io.ReadData(3) := DataReg(31,0)

  io.WriteCompleted := false.B

  val boot :: resetEnable :: resetWait :: setReset :: idle :: read :: write :: Nil = Enum(7)
  val transmitCMD :: transmitAddress :: transmitData :: writeDelay :: receiveData :: computeAddress :: Nil = Enum(6)

  val StateReg = RegInit(boot)
  StateReg := boot;

  val SubStateReg = RegInit(transmitCMD)
  SubStateReg := transmitCMD;

  val CntReg = RegInit(0.U(16.W))

  val CommandIndex = RegInit(0.U(8.W))

  val ClkCounter = RegInit(0.U(8.W))
  ClkCounter := ClkCounter + 1.U;

  val Rising_edge = Wire(UInt(1.W))
  Rising_edge := 0.U;

  val Self_clock = RegInit(0.U(1.W))
  when(ClkCounter === Count.U){
    Self_clock := !Self_clock;
    Rising_edge := !Self_clock;
    ClkCounter := 0.U;
  }

  when(Rising_edge === 1.U){
    CntReg := CntReg + 1.U
  }

  io.S_CLK := Self_clock;

  switch(StateReg) {
    is(boot){
      // Resets clock for reset command
      CntReg := CntReg + 1.U
      StateReg := boot

      when(CntReg === Bootup_wait.U){
        StateReg := resetEnable
        CntReg := 0.U
      }
    }
    is(resetEnable){
      io.CE := false.B
      StateReg := resetEnable
      CommandIndex := 7.U - CntReg;
      io.MOSI := CMDResetEnable(CommandIndex)

      when(CommandIndex === 0.U){
        StateReg := resetWait
        CntReg := 0.U
      }
    }
    is(resetWait){
      io.CE := true.B
      StateReg := resetWait
      CommandIndex := 7.U - CntReg;

      when(CommandIndex === 0.U){
        StateReg := resetWait
        CntReg := 0.U
      }
    }
    is(setReset) {
      io.CE := false.B
      StateReg := setReset
      CommandIndex := 7.U - CntReg;
      io.MOSI := CMDReset(CommandIndex)

      when(CntReg === 7.U) {
        io.CE := true.B
        CntReg := 0.U
        io.Completed := true.B

        StateReg := idle
      }
    }


  }

}

