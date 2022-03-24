package SPI_memory

import chisel3._
import chisel3.util._
import SPI_CMDS._

object SPI_CMDS {
  val CMDResetEnable = 102.U(8.W)
  val CMDReset = 153.U(8.W)
  val CMDSPIRead = 3.U(8.W)
  val CMDSPIWrite = 2.U(8.W)
}

class SPI extends Module {
  val io = IO(new Bundle {
    val ReadEnable = Input(Bool())
    val WriteEnable = Input(Bool())
    val Address = Input(UInt(24.W))
    val WriteData = Input(Vec(4,UInt(32.W)))
    val ByteEnable = Input(UInt(16.W))

    val ReadData = Output(Vec(4, UInt(32.W)))

    val DataValid = Output(Bool())
    val WriteCompleted = Output(Bool())

    //SPI pins
    val CE = Output(Bool())
    val MOSI = Output(Bool())
    val MISO = Input(Bool())
    //val StateReg = Output()
    val CntReg = Output(UInt(8.W));
    val PosReg = Output(UInt(4.W));
  })

  // Defaults
  io.CE := true.B
  io.MOSI := false.B
  io.DataValid := false.B

  io.DataValid := false.B
  io.ReadData(0) := 0.U
  io.ReadData(1) := 0.U
  io.ReadData(2) := 0.U
  io.ReadData(3) := 0.U

  io.WriteCompleted := false.B

  val resetEnable :: setReset :: idle :: read :: write :: Nil = Enum(5)
  val StateReg = RegInit(resetEnable)

  val transmitCMD :: transmitAddress :: transmitData :: receiveData :: computeAddress :: Nil = Enum(5)
  val SubStateReg = RegInit(transmitCMD)

  val CntReg = RegInit(0.U(8.W))
  io.CntReg := CntReg

  /*
  TempAddress is the starting address of the current burst.
  When ByteEn of a specific byte is low,
  this address will jump to the starting address of the next valid byte
  */
  val TempAddress = RegInit(0.U(24.W))

  //PosReg is a pointer to the current byte in the byteEn integer being written to memory
  val PosReg = RegInit(0.U(4.W))
  io.PosReg := PosReg;

  val Carry = Wire(Vec(17, Bool()))

  switch(StateReg) {
    is(resetEnable) {
      io.CE := false.B
      io.MOSI := CMDResetEnable(7.U - CntReg)
      CntReg := CntReg + 1.U

      when(CntReg === 7.U) {
        io.CE := true.B
        CntReg := 0.U
        io.MOSI := 0.U

        StateReg := setReset
      }
    }
    is(setReset) {
      io.CE := false.B
      io.MOSI := CMDReset(7.U - CntReg)
      CntReg := CntReg + 1.U

      when(CntReg === 7.U) {
        io.CE := true.B
        CntReg := 0.U
        io.MOSI := 0.U

        when(io.ReadEnable) {
          StateReg := read
        }
        . elsewhen(io.WriteEnable) {
          StateReg := write
        }
        . otherwise {
          StateReg := idle
        }
      } 
    }
    is(idle) {
      io.CE := true.B
      io.DataValid := false.B
      SubStateReg := transmitCMD

      when(io.ReadEnable) {
        StateReg := read
      }
      . elsewhen(io.WriteEnable) {
        StateReg := write
      }
    }
    is(read) {
      switch(SubStateReg) {
        is(transmitCMD) {
          io.CE := false.B
          io.MOSI := CMDSPIRead(7.U - CntReg)
          CntReg := CntReg + 1.U

          when(CntReg === 7.U) {
            CntReg := 0.U
            SubStateReg := transmitAddress
          }
        }

        is(transmitAddress) {
          io.MOSI := io.Address(24.U - CntReg)
          CntReg := CntReg + 1.U

          when(CntReg === 23.U) {
            CntReg := 0.U
            SubStateReg := receiveData
          }
        }

        is(receiveData) {
          // Buffer for the read data
          val DataReg = RegInit(0.U(128.W))
          DataReg := Cat(DataReg, io.MISO.asUInt)

          CntReg := CntReg + 1.U
          when(CntReg === 127.U) {
            io.ReadData(0) := DataReg(127,96)
            io.ReadData(1) := DataReg(95,64)
            io.ReadData(2) := DataReg(63,32)
            io.ReadData(3) := DataReg(31,0)

            io.DataValid := true.B

            StateReg := idle
          }
        }
      }
    }
    is(write) {
      SubStateReg := computeAddress

      switch(SubStateReg) {
        is(computeAddress) {
          for(i <- 0 until 17) {
            Carry(i) := false.B
          }

          /* 
          The following code looks through the WriteByteEN UInt to find the next valid byte, and 
          increments the address to that byte 
          */
          for(i <- 0 until 16) {
            when(i.U === 0.U && io.ByteEnable(i) && PosReg === 0.U){
              TempAddress := io.Address
              Carry(i + 1) := true.B
            }
            . elsewhen(Carry(i)){
              Carry(i + 1) := true.B
            }
            . elsewhen(i.U > PosReg && io.ByteEnable(i)){
              TempAddress := io.Address + (i.U << 3).asUInt
              PosReg := i.U
              Carry(i + 1) := true.B
            }
            . otherwise{
              Carry(i + 1) := false.B
            }
          }
          SubStateReg := transmitCMD
        }
        is(transmitCMD) {
          io.CE := false.B
          io.MOSI := CMDSPIWrite(7.U - CntReg)
          CntReg := CntReg + 1.U
          when(CntReg === 7.U) {
            CntReg := 0.U
            SubStateReg := transmitAddress
          }
        }
        is(transmitAddress) {
          io.MOSI := TempAddress(23.U - CntReg)

          CntReg := CntReg + 1.U

          when(CntReg === 23.U){
            CntReg := 0.U
            SubStateReg := transmitData
          }
        }
        is(transmitData) {
          io.MOSI := io.WriteData(CntReg(7,5))(31.U - CntReg(4,0))
          CntReg := CntReg + 1.U
          
          when(CntReg === 7.U) {
            PosReg := PosReg + 1.U
            CntReg := 0.U

            when(!io.ByteEnable(PosReg + 1.U)) {
              CntReg := 0.U
              io.CE := true.B
            }
          }

          when((CntReg + (PosReg << 3).asUInt) === 127.U) {
            CntReg := 0.U
            PosReg := 0.U
            io.WriteCompleted := true.B
            StateReg := idle
          }
        }
      }
    }
  }
}
