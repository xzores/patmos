;buildInfoPackage: chisel3, version: 3.4.3, scalaVersion: 2.12.12, sbtVersion: 1.3.10
circuit hello : 
  module hello : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : UInt<1>, out : UInt<1>}
    
    node _T = not(io.in) @[hello.scala 12:23]
    reg REG : UInt<1>, clock @[hello.scala 12:22]
    REG <= _T @[hello.scala 12:22]
    io.out <= REG @[hello.scala 12:12]
    
