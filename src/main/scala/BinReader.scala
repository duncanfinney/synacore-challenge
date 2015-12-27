import java.nio.file.{Paths, Files}

object BinReader {

  def getInstructions(fileName: String): Stream[Instruction] = {
    val rawAsm = Files
      .readAllBytes(Paths.get(fileName))
      .sliding(2, 2)
      .map { case Array(b1, b2) => (b2 << 8 | b1) & 0xFFFF }
      .toList

    getInstructions(rawAsm)
  }

  def getInstructions(rawAsm: List[Int]): Stream[Instruction] = {
    if (rawAsm.isEmpty) {
      return Stream.Empty
    }

    lazy val a = rawAsm(1)
    lazy val b = rawAsm(2)
    lazy val c = rawAsm(3)

    val instruction: Instruction = rawAsm.head match {
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
      case _  => Unknown

    }

    instruction #:: getInstructions(rawAsm.tail.drop(instruction.paramCount))
  }

}
