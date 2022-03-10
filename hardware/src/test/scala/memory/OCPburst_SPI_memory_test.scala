import chisel3._;
import chisel3.iotesters;

class DeviceUnderTest extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt (2.W))
    val b = Input(UInt (2.W))
    val out = Output(UInt (2.W))
  })
  io.out := io.a & io.b;
};

class TesterSimplePeekPoke(dut: DeviceUnderTest) extends PeekPokeTester(dut) {
  poke(dut.io.a, 0.U);
  poke(dut.io.b, 1.U);
  step (1);
  println("Result is: " + peek(dut.io.out).toString);
  poke(dut.io.a, 3.U);
  poke(dut.io.b, 2.U);
  step (1);
  println("Result is: " + peek(dut.io.out).toString);
};

object TesterSimplePeekPoke extends App {
    chisel3.iotesters.Driver (() => new DeviceUnderTest ()) { c =>
        new TesterSimplePeekPoke (c);
    };
};

/*
import chisel3._;
import chisel3.iotesters;
import org.scalatest._;
import org.scalatest.flatspec.AnyFlatSpec;
import org.scalatest.matchers.should.Matchers;

class OCPburst_SPI_memory_test(dut: DeviceUnderTest ) extends PeekPokeTester(dut) {
  poke(dut.io.a, 0.U)
  poke(dut.io.b, 1.U)
  step (1)
  println("Result is: " + peek(dut.io.out).toString)
  poke(dut.io.a, 3.U)
  poke(dut.io.b, 2.U)
  step (1)
  println("Result is: " + peek(dut.io.out).toString)
}

object OCPburst_SPI_memory_test extends App {
  chisel3.iotesters.Driver (() => new DeviceUnderTest ()) { c =>
    new OCPburst_SPI_memory_test (c)
  }
}

/*

class TesterSimple(dut: DeviceUnderTest ) extends
  PeekPokeTester(dut) {
  poke(dut.io.a, 0.U)
  poke(dut.io.b, 1.U)
  step (1)
  println("Result is: " + peek(dut.io.out).toString)
  poke(dut.io.a, 3.U)
  poke(dut.io.b, 2.U)
  step (1)
  println("Result is: " + peek(dut.io.out).toString)
}

object TesterSimple extends App {
  chisel3. iotesters .Driver (() => new DeviceUnderTest ()) { c =>
    new TesterSimple (c)
  }
}


class OCPburst_SPI_memory_test extends FlatSpec with Matchers
{
  "Write read test software" should "pass" in {
    test(new OCPburst_SPI_memory()) { dut =>
      dut.io.ReadAddress.poke(10.U);
      dut.io.ReadData.poke();
      dut.io.ReadCmd.poke(10.U);
      dut.io.WriteAddress.poke(10.U);
      dut.io.WriteData.poke();
      dut.io.WriteCmd.poke();

      dut.io.Valid.peek();

      dut.clock.step ()
      println("Result is: " + dut.io.out.peek ().toString)
    }
  }
}
*/
*/

