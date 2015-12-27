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

