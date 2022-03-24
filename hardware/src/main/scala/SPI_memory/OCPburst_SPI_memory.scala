package SPI_memory

import util.BlackBoxFlash
import util.TriStateDriver
import chisel3._
import chisel3.util._
import OcpCmd._
import OcpResp._
import ocp._


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


class OCPburst_SPI_memory extends Module {
  val io = IO(new Bundle {
    val OCP_interface = new OcpBurstSlavePort(24, 32, 0); //TODO what is burstLen
  })

  //Defaults
  io.OCP_interface.S.Resp := 0.U
  io.OCP_interface.S.CmdAccept := false.B
  io.OCP_interface.S.DataAccept := false.B
  io.OCP_interface.S.Data := 0.U

  val SPI = Module(new SPI(1))
  for(i <- 0 until 4){
    SPI.io.WriteData(i) := 0.U
  }
  SPI.io.Address := 0.U
  SPI.io.ReadEnable := false.B
  SPI.io.WriteEnable := false.B
  SPI.io.ByteEnable := 0.U

  val idle :: read :: sampleData :: write :: Nil = Enum(4)
  val StateReg = RegInit(idle)

  val CntReg = RegInit(0.U(8.W))

  val WriteData = Reg(Vec(4,UInt(32.W)))
  val WriteByteEN = Reg(Vec(4, UInt(4.W)))

  switch(StateReg) {
    is(idle) {
      switch(io.OCP_interface.M.Cmd) {
        is(WR) {
          StateReg := sampleData
        }
        is(RD) {
          StateReg := read
        }
      }
    }
    is(read) {
      SPI.io.Address := io.OCP_interface.M.Addr;
      SPI.io.ReadEnable := true.B
      io.OCP_interface.S.CmdAccept := true.B;

      when(SPI.io.DataValid) {
        io.OCP_interface.S.Data := SPI.io.ReadData(CntReg)
        CntReg := CntReg + 1.U
        when(CntReg === 3.U) {
          CntReg := 0.U
          StateReg := idle
        }
        SPI.io.ReadEnable := false.B
        io.OCP_interface.S.Resp := DVA
      }
    }
    is(sampleData) {
      io.OCP_interface.S.CmdAccept := true.B
      io.OCP_interface.S.DataAccept := true.B

      when(io.OCP_interface.M.DataValid.asBool()) {
        WriteData(CntReg) := io.OCP_interface.M.Data;
        WriteByteEN(CntReg) := io.OCP_interface.M.DataByteEn
        CntReg := CntReg + 1.U
      }

      when(CntReg === 3.U) {
        CntReg := 0.U
        StateReg := write
      }
    }
    is(write) {
      SPI.io.Address := io.OCP_interface.M.Addr;
      SPI.io.WriteEnable := true.B

      SPI.io.WriteData := WriteData
      SPI.io.ByteEnable := (WriteByteEN(3) << 12).asUInt + (WriteByteEN(2) << 8).asUInt + (WriteByteEN(1) << 4).asUInt + WriteByteEN(0)

      when(SPI.io.WriteCompleted) {
        io.OCP_interface.S.Resp := 1.U
        StateReg := idle
      }
    }
  }
}

object MemoryInterface extends App {
   (new chisel3.stage.ChiselStage).emitVerilog(new OCPburst_SPI_memory)
}

