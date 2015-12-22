import Chisel._

class Mult extends Module {
  val BIT = 258
  val io = new Bundle {
    val a = UInt(INPUT, BIT)
    val b = UInt(INPUT, BIT)
    val n = UInt(INPUT, BIT)
    val start = Bool(INPUT)
    val done = Bool(OUTPUT)
    val out = UInt(OUTPUT, BIT)
  }

  val a = Reg(UInt(width=BIT))
  val b = Reg(UInt(width=BIT))
  val n = Reg(UInt(width=BIT))
  val ans = Reg(UInt(width=BIT))
  val done = Reg(Bool(true))
  
  when (io.start) {
    a := io.a
    b := io.b
    n := io.n
    done := Bool(false)
  } .elsewhen (b === UInt(0)) {
    done := Bool(true)
  } .otherwise {
    when (b(0) === UInt(1)) {
      val t1 = ans + a
      val t2 = Mux(t1(0) === UInt(1), t1 + n, t1)
      ans := (ans + t2) >> 1
    }
    b := b >> 1
  }
  io.out := ans
  io.done := done
}

class MultTest(c: Mult) extends Tester(c) {
  poke(c.io.a, 2)
  poke(c.io.b, 3)
  poke(c.io.n, 5)
  poke(c.io.start, 1)
  step(1)
  poke(c.io.start, 0)
  for (i <- 1 to 10) {
    step(1)
    peek(c.b)
    expect(c.io.out, 10)
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    chiselMainTest(args, () => Module(new Mult())) {
      c => new MultTest(c)
    }
  }
}
