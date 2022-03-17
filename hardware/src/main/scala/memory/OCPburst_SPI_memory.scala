

import chisel3._
import chisel3.util._

class OCPburst_SPI_memory() extends Module {
  val io = IO(new Bundle {
    val Operation = Input(UInt(2.W))

    //OCP burst pins
    val ReadAddress = Input(UInt(32.W))
    val ReadData = Output(Vec(4, UInt(32.W)))
    val ReadCmd = Input(UInt(8.W))

    val WriteAddress = Input(UInt(32.W))
    val WriteData = Input(Vec(4, UInt(32.W)))
    val WriteCmd = Input(UInt(8.W))

    val Valid = Output(Bool())


    //SPI pins
    val CE = Output(Bool())
    val SI = Output(Bool())
    val SO = Input(Bool())
  })

  val StateReg = RegInit(3.U(2.W))
  val OPCounter = RegInit(0.U(1.W))
  val CNTReg = RegInit(0.U(8.W))

  val DataReg = RegInit(0.U(128.W))

  StateReg := io.Operation

  //Defaults
  io.CE := true.B
  io.SI := false.B
  io.Valid := false.B

  io.ReadData(0) := 0.U
  io.ReadData(1) := 0.U
  io.ReadData(2) := 0.U
  io.ReadData(3) := 0.U

  switch(io.Operation) {
    is(102.U) {
      // Reset 102 = h66
      CNTReg := CNTReg + 1.U

    }
    is(3.U) {
      OPCounter := 0.U
      io.CE := false.B
    }
    is(0.U) {
      //Idle

      io.ReadData(0) := DataReg(127,96)
      io.ReadData(1) := DataReg(95,64)
      io.ReadData(2) := DataReg(63,32)
      io.ReadData(3) := DataReg(31,0)
    }
    is(1.U) {
      //Read
      switch(OPCounter) {
        is(0.U) {
          OPCounter := 1.U
          io.CE := false.B
        }
        is(1.U) {
          io.CE := false.B
          io.SI := io.ReadCmd(CNTReg)
          CNTReg := CNTReg + 1.U

          when(CNTReg === 7.U) {
            CNTReg := 0.U
            OPCounter := 1.U
          }
        }
        is(2.U) {
          io.CE := false.B
          io.SI := io.ReadAddress(CNTReg)
          CNTReg := CNTReg + 1.U

          when(CNTReg === 23.U) {
            CNTReg := 0.U
            OPCounter := 2.U
          }
        }
        is(3.U) {
          io.CE := false.B
          DataReg := Cat(DataReg, io.SO.asUInt)

          CNTReg := CNTReg + 1.U

          when(CNTReg === 127.U) {
            io.CE := true.B
            CNTReg := 0.U
            OPCounter := 0.U
            io.Valid := true.B
          }
        }
      }
    }
    is(2.U){
      //Write
      switch(OPCounter){
        is(0.U) {
          io.CE := false.B
          OPCounter := 1.U
          //Added so CE drops a single clk cycle before data transfer
        }
        is(1.U){
          io.CE := false.B
          io.SI := io.WriteCmd(CNTReg)

          /*
          DataReg(127,96) := io.WriteData(3)
          DataReg(95,64) := io.WriteData(2)
          DataReg(63,32) := io.WriteData(1)
          DataReg(31,0) := io.WriteData(0)
          */

          DataReg := (io.WriteData(3) << 95).asUInt + (io.WriteData(2) << 63).asUInt + (io.WriteData(1) << 31).asUInt + io.WriteData(0)

          when(CNTReg === 7.U){
            OPCounter := 2.U
            CNTReg := 0.U
          }
        }
        is(2.U){
          io.CE := false.B
          io.SI := DataReg(CNTReg)

          CNTReg := CNTReg + 1.U

          when(CNTReg === 127.U){
            CNTReg := 0.U
            OPCounter := 0.U
            io.Valid := true.B
          }
        }
      }
    }
  }
}