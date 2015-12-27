import java.nio.file.{Files, Paths}

object Main extends App {

  abstract class Instruction {
    def paramCount = 0
  }

  abstract class Instruction0 extends Instruction {
    override def paramCount = 0
  }

  abstract class Instruction1 extends Instruction {
    override def paramCount = 1
    def a: Int
  }

  abstract class Instruction2 extends Instruction {
    override def paramCount = 2
    def a: Int
    def b: Int
  }

  abstract class Instruction3 extends Instruction {
    override def paramCount = 3
    def a: Int
    def b: Int
    def c: Int
  }

  case object Halt extends Instruction0

  case class Set(a: Int, b: Int) extends Instruction2

  case class Push(a: Int) extends Instruction1

  case class Pop(a: Int) extends Instruction1

  case class Eq(a: Int, b: Int, c: Int) extends Instruction3

  case class Gt(a: Int, b: Int, c: Int) extends Instruction3

  case class Jump(a: Int) extends Instruction1

  case class JumpT(a: Int, b: Int) extends Instruction2

  case class JumpF(a: Int, b: Int) extends Instruction2

  case class Add(a: Int, b: Int, c: Int) extends Instruction3

  case class Multiply(a: Int, b: Int, c: Int) extends Instruction3

  case class Modulo(a: Int, b: Int, c: Int) extends Instruction3

  case class And(a: Int, b: Int, c: Int) extends Instruction3

  case class Or(a: Int, b: Int, c: Int) extends Instruction3

  case class Not(a: Int, b: Int) extends Instruction2

  case class RMem(a: Int, b: Int) extends Instruction2

  case class WMem(a: Int, b: Int) extends Instruction2

  case class Call(a: Int) extends Instruction1

  case object Ret extends Instruction

  case class Out(a: Int) extends Instruction1

  case class In(a: Int) extends Instruction1

  case object Noop extends Instruction0

  case object Unknown extends Instruction0

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
      case 9  => Add(a,b,c)
      case 10 => Multiply(a, b, c)
      case 11 => Modulo(a, b, c)
      case 12 => And(a, b, c)
      case 13 => Or(a, b, c)
      case 14 => Not(a, b)
      case 15 => RMem(a, b)
      case 16 => WMem(a, b)
      case 17 => Call(a)
      case 18 => Ret
      case 19 => Out(c.toChar)
      case 20 => In(a)
      case 21 => Noop
      case _  => Unknown

    }


    instruction #:: getInstructions(rawAsm.drop(1).drop(instruction.paramCount))
  }

  val rawAsm =
    Files
      .readAllBytes(Paths.get("src/main/resources/challenge.bin"))
      .map(_.toShort & 0xFFFF)
      .sliding(2, 2)
      .map { case Array(b1, b2) => (b2 << 8 | b1) & 0xFFFF }
      .toList

  println(getInstructions(rawAsm).toList.filter(_!=Unknown).zipWithIndex.map{ case (i, n) => s"[$n] -> $i" }.mkString("\n"))
//  println(rawAsm)
}


