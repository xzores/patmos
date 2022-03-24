import util.BlackBoxFlash
import util.TriStateDriver
import SPI_memory.SPI
import chisel3._
import chisel3.experimental.Analog
import chisel3.util.HasBlackBoxInline

/**
 * This module allows to connect to tri-state busses with Chisel by using a Verilog blackbox.
 * @param width The width of the tri-state bus
 */
class SPI_memory_verilator_test extends Module {
    val io = IO(new Bundle{
        val ReadEnable = Input(Bool())
        val WriteEnable = Input(Bool())
        val Address = Input(UInt(24.W))
        val WriteData = Input(Vec(4,UInt(32.W)))
        val ByteEnable = Input(UInt(16.W))

        val ReadData = Output(Vec(4, UInt(32.W)))
        val DataValid = Output(Bool())
        val WriteCompleted = Output(Bool())
    })

    val SPI = Module(new SPI(2))
    val BusDriver = Module(new TriStateDriver(width = 4))    
    val Flash = Module(new BlackBoxFlash)

    Flash.io.CSNeg := SPI.io.CE 
    Flash.io.SCK := SPI.io.SCK 

    val SPIBus = Wire(UInt(4.W))

    BusDriver.io.driveData(0) := SPI.io.MOSI
    BusDriver.io.driveData(1) := false.B
    BusDriver.io.driveData(2) := true.B
    BusDriver.io.driveData(3) := true.B

    Flash.io.SI := BusDriver.io.bus
    Flash.io.SO := BusDriver.io.bus

}

object SPI_memory_verilator_test extends App {
   (new chisel3.stage.ChiselStage).emitVerilog(new SPI_memory_verilator_test)
}