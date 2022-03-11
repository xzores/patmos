import chisel3._
import chisel3.util._
import OcpCmd._
import OcpResp._


object OcpCmd {
  val IDLE = "b000".U(3.W)
  val WR   = "b001".U(3.W)
  val RD   = "b010".U(3.W)
}

object OcpResp {
  val NULL = "b00".U(2.W)
  val DVA  = "b01".U(2.W)
  val FAIL = "b10".U(2.W)
  val ERR  = "b11".U(2.W)
}

class Memory_interface extends Module {
  val io = IO(new Bundle {
    val MCmd = Input(UInt(4.W))
    val Address = Input(UInt(32.W))
    val Data = Input(UInt(32.W))
    val ByteEn = Input(UInt(4.W))
    val DataValid = Input(Bool())

    val SData = Output(UInt(32.W))
    val SResp = Output(UInt(4.W))
    val SCmdAccept = Output(Bool())
    val SDataAccept = Output(Bool())
    
    val CE = Output(Bool())
    val MOSI = Output(Bool())
    val MISO = Input(Bool())
  })

  //Defaults
  io.SResp := 0.U
  io.SCmdAccept := false.B
  io.SDataAccept := false.B
  io.SData := 0.U


  val SPI = Module(new SPI)
  for(i <- 0 until 4){
    SPI.io.WriteData(i) := 0.U
  }

  SPI.io.Address := 0.U
  SPI.io.ReadEnable := false.B
  SPI.io.WriteEnable := false.B
  SPI.io.ByteEnable := 0.U
  

  io.MOSI := SPI.io.MOSI
  io.CE := SPI.io.CE
  SPI.io.MISO := io.MISO

  val idle :: read :: sampleData :: write :: Nil = Enum(4)
  val StateReg = RegInit(idle)

  val CntReg = RegInit(0.U(8.W))

  val WriteData = Reg(Vec(4,UInt(32.W)))
  val WriteByteEN = Reg(Vec(4, UInt(4.W)))

  switch(StateReg) {
    is(idle) {
      switch(io.MCmd) {
        is(WR) {
          StateReg := sampleData
        }
        is(RD) {
          StateReg := read
        }
      }
    }
    is(read) {
      SPI.io.Address := io.Address
      SPI.io.ReadEnable := true.B
      when(SPI.io.DataValid) {
        io.SData := SPI.io.ReadData(CntReg)
        CntReg := CntReg + 1.U
        when(CntReg === 3.U) {
          CntReg := 0.U
          StateReg := idle
        }
        SPI.io.ReadEnable := false.B
        io.SResp := DVA
      }
    }
    is(sampleData) {
      io.SCmdAccept := true.B
      io.SDataAccept := true.B

      when(io.DataValid) {
        WriteData(CntReg) := io.Data
        WriteByteEN(CntReg) := io.ByteEn
        CntReg := CntReg + 1.U
      }

      when(CntReg === 3.U) {
        CntReg := 0.U
        StateReg := write
      }
    }
    is(write) {
      // OCP gives 32-bit address, RAM expects 24-bit, drop upper 8 bit
      SPI.io.Address := io.Address(23, 0)
      SPI.io.WriteEnable := true.B

      SPI.io.WriteData := WriteData
      SPI.io.ByteEnable := (WriteByteEN(3) << 12).asUInt + (WriteByteEN(2) << 8).asUInt + (WriteByteEN(1) << 4).asUInt + WriteByteEN(0)

      when(SPI.io.WriteCompleted) {
        io.SResp := 1.U
        StateReg := idle
      }
    }
  }
}

