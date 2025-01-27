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
  })

  // Defaults

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
  val StateReg = RegInit(boot)

  val transmitCMD :: transmitAddress :: transmitData :: writeDelay :: receiveData :: computeAddress :: Nil = Enum(6)
  val SubStateReg = RegInit(transmitCMD)

  val CntReg = RegInit(0.U(14.W))

  /*
  TempAddress is the starting address of the current burst.
  When ByteEn of a specific byte is low,
  this address will jump to the starting address of the next valid byte
  */

  val TempAddress = RegInit(0.U(24.W))

  val PosReg = RegInit(0.U(4.W)) //PosReg is a pointer to the current byte in the byteEn integer being written to memory

  val Carry = Wire(Vec(17, Bool())) // Used to find read address when byte enable is low
  for(i <- 0 until 17) {
    Carry(i) := false.B
  }

  // Clock stuff

  val ClkReg = RegInit(0.U(1.W))
  val ClkCounter = RegInit(0.U(8.W))

  val ClkRegDelay = RegInit(0.U(1.W)) // Used to determine rising and falling edge
  ClkRegDelay := ClkReg

  val NextState = Wire(Bool()) // Goes high one clock cycle before rising edge of SCLK
  val NextStateInv = Wire(Bool()) // Goes high one clock cycle before falling edge of SCLK

  val ClockEn = Wire(Bool()) // Enables SCLK output
  val ClockReset = Wire(Bool()) // Resets clock to 0

  val RisingEdge = Wire(Bool())
  val FallingEdge = Wire(Bool())


  NextState := false.B
  NextStateInv := false.B

  ClockEn := false.B
  ClockReset := false.B

  RisingEdge := false.B
  FallingEdge := false.B

  io.SCLK := (ClkReg & ClockEn)

  ClkCounter := ClkCounter + 1.U

  when(ClkCounter === Count.U){
    ClkReg := !ClkReg
    ClkCounter := 0.U

    when(!ClkReg.asBool){
      NextState := true.B
    }
    when(ClkReg.asBool){
      NextStateInv := true.B
    }
  }

  when(ClockReset){
    ClkReg := false.B
    ClkCounter := 0.U
  }

  when(ClkReg.asBool && !ClkRegDelay.asBool){
    RisingEdge := true.B
  }

  when(!ClkReg.asBool && ClkRegDelay.asBool){
    FallingEdge := true.B
  }


  // Actual state machine


  switch(StateReg) {
    is(boot){
      // Resets clock for reset command

      CntReg := CntReg + 1.U

      when(CntReg === "h3fff".U){
        io.CE := false.B
        ClockReset := true.B
        StateReg := resetEnable
        CntReg := 0.U
      }
    }
    is(resetEnable) {
      io.CE := false.B
      ClockEn := true.B

      io.MOSI := CMDResetEnable(7.U - CntReg)

      when(NextStateInv){
        CntReg := CntReg + 1.U
      }

      when(CntReg === 7.U && NextStateInv) {
        io.CE := true.B
        CntReg := 0.U
        io.MOSI := 0.U
        StateReg := resetWait
      }
    }
    is(resetWait){
      // A Delay between the two commands
      io.CE := true.B

      when(NextStateInv){
        ClockReset := true.B
        StateReg := setReset
      }
    }
    is(setReset) {
      io.CE := false.B
      ClockEn := true.B

      io.MOSI := CMDReset(7.U - CntReg)

      when(NextStateInv){
        CntReg := CntReg + 1.U
      }

      when(CntReg === 7.U && NextStateInv) {
        io.CE := true.B
        CntReg := 0.U
        io.Completed := true.B

        StateReg := idle
      }
    }
    is(idle) {
      // Waits for command from main

      io.CE := true.B

      when(io.ReadEnable) {
        StateReg := read
        SubStateReg := transmitCMD
        io.CE := false.B
        ClockReset := true.B
      }
        . elsewhen(io.WriteEnable) {
          StateReg := write
          SubStateReg := computeAddress
        }
    }
    is(read) {
      switch(SubStateReg) {
        is(transmitCMD) {
          io.CE := false.B
          ClockEn := true.B

          io.MOSI := CMDSPIRead(7.U - CntReg)
          SubStateReg := transmitCMD

          when(NextStateInv){
            CntReg := CntReg + 1.U
          }

          when(CntReg === 7.U && NextStateInv) {
            CntReg := 0.U
            SubStateReg := transmitAddress
          }
        }

        is(transmitAddress) {
          io.CE := false.B
          ClockEn := true.B

          io.MOSI := io.Address(24.U - CntReg)
          SubStateReg := transmitAddress

          when(NextStateInv){
            CntReg := CntReg + 1.U
          }

          when(CntReg === 23.U && NextStateInv) {
            CntReg := 0.U
            SubStateReg := receiveData
          }
        }
        is(receiveData) {
          io.CE := false.B
          ClockEn := true.B

          // Reads on the rising edge of SCLK

          when(RisingEdge){
            DataReg := Cat(DataReg, io.MISO.asUInt)
            CntReg := CntReg + 1.U
          }

          when(CntReg === 127.U && NextStateInv) {
            io.DataValid := true.B
            StateReg := idle
          }
        }
      }
    }
    is(write) {
      //SubStateReg := computeAddress

      switch(SubStateReg) {
        is(computeAddress) {
          when(PosReg === 15.U){
            PosReg := 0.U
            io.WriteCompleted := true.B
            StateReg := idle
            io.CE := true.B
          }

          // The following code scans through the ByteEn "array", to find the next position where ByteEn is high

          for(i <- 0 until 16){
            when(i.U === 0.U && io.ByteEnable(i) && PosReg === 0.U){
              TempAddress := io.Address
              Carry(i+1) := true.B
            }.elsewhen(Carry(i)){
              Carry(i+1) := true.B
            }.elsewhen(i.U > PosReg && io.ByteEnable(i) && !Carry(i)){
              TempAddress := io.Address + i.U //TODO Check how addresses work
              PosReg := i.U
              Carry(i + 1) := true.B
            }.otherwise{
              Carry(i + 1) := false.B
            }
          }

          when(!Carry(15).asBool){
            PosReg := 0.U
            io.WriteCompleted := true.B
            StateReg := idle
            io.CE := true.B
          }

          SubStateReg := transmitCMD

          io.CE := false.B
          ClockReset := true.B

        }
        is(transmitCMD) {
          io.CE := false.B
          ClockEn := true.B

          io.MOSI := CMDSPIWrite(7.U - CntReg)
          SubStateReg := transmitCMD

          when(NextStateInv){
            CntReg := CntReg + 1.U
          }

          when(CntReg === 7.U && NextStateInv) {
            CntReg := 0.U
            SubStateReg := transmitAddress
          }
        }
        is(transmitAddress) {
          io.CE := false.B
          ClockEn := true.B

          io.MOSI := TempAddress(23.U - CntReg)
          SubStateReg := transmitAddress

          when(NextStateInv){
            CntReg := CntReg + 1.U
          }

          when(CntReg === 23.U && NextStateInv){
            CntReg := 0.U
            SubStateReg := transmitData
          }
        }
        is(transmitData) {
          io.CE := false.B
          ClockEn := true.B

          io.MOSI := io.WriteData((PosReg >> 2).asUInt)(31.U - Cat(PosReg(1,0),CntReg(2,0)).asUInt)
          SubStateReg := transmitData

          when(NextStateInv){
            CntReg := CntReg + 1.U
          }

          when(CntReg === 7.U && NextStateInv) {
            PosReg := PosReg + 1.U
            CntReg := 0.U

            // When ByteEn for the next byte is low, the following code restarts the write.

            when(!io.ByteEnable(PosReg + 1.U)) {
              CntReg := 0.U
              SubStateReg := writeDelay
              io.CE := true.B
            }
          }

          when((CntReg + (PosReg << 3).asUInt) === 127.U && NextStateInv) {
            CntReg := 0.U
            PosReg := 0.U
            io.WriteCompleted := true.B
            StateReg := idle
            io.CE := true.B
          }
        }
        is(writeDelay){
          // Delay required between two write bursts

          // TODO check datasheet for actual required delay

          io.CE := true.B

          when(NextStateInv){
            SubStateReg := computeAddress
          }
        }
      }
    }
  }
}






  /*

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
          io.CE := true.B
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

      /* 
      TempAddress is the starting address of the current burst. 
      When ByteEn of a specific byte is low, 
      this address will jump to the starting address of the next valid byte 
      */
      val TempAddress = RegInit(0.U(24.W))

      //PosReg is a pointer to the current byte in the byteEn integer being written to memory
      val PosReg = RegInit(0.U(4.W))

      switch(SubStateReg) {
        is(computeAddress) {
          val Carry = Wire(Vec(17, Bool()))
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


*/