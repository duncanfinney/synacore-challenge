import java.nio.file.{Paths, Files}

object BinReader {

  def getBytes(fileName: String): List[Int] = {
    Files
      .readAllBytes(Paths.get(fileName))
      .map(_.toChar.toInt)
      .sliding(2, 2)
      .map { case Array(b1, b2) => 0xFFFF & ((b2 & 0xFF) << 8 | (b1 & 0xFF)) }
      .toList


  }


  def getInstructionAtLocation(rawAsm: Map[Int, Int], location: Int): Instruction = {

    lazy val a = rawAsm(location + 1)
    lazy val b = rawAsm(location + 2)
    lazy val c = rawAsm(location + 3)

    rawAsm(location) match {
      case 0  => Halt
      case 1  => Set(a, b)
      case 2  => Push(a)
      case 3  => Pop(a)
      case 4  => Eq(a, b, c)
      case 5  => Gt(a, b, c)
      case 6  => Jump(a)
      case 7  => JumpT(a, b)
      case 8  => JumpF(a, b)
      case 9  => Add(a, b, c)
      case 10 => Multiply(a, b, c)
      case 11 => Modulo(a, b, c)
      case 12 => And(a, b, c)
      case 13 => Or(a, b, c)
      case 14 => Not(a, b)
      case 15 => RMem(a, b)
      case 16 => WMem(a, b)
      case 17 => Call(a)
      case 18 => Ret
      case 19 => Out(a)
      case 20 => In(a)
      case 21 => Noop
      case x  => throw new Error(s"Unknown opcode $x")

    }
  }

}
