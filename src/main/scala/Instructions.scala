abstract class Instruction {
  def paramCount: Int

  def applyTo(vm: VM): VM = vm.moveInstructionPointer
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

case object Halt extends Instruction0 {
  override def applyTo(vm: VM) = {
    sys.exit(0)
    vm
  }
}

case class Set(a: Int, b: Int) extends Instruction2 {
  override def applyTo(vm: VM) = {
    vm
      .updateMemory(a, b)
      .moveInstructionPointer
  }
}

case class Push(a: Int) extends Instruction1 {
  override def applyTo(vm: VM) = vm.stackPush(a).moveInstructionPointer
}

case class Pop(a: Int) extends Instruction1 {
  override def applyTo(vm: VM) = {
    vm
      .updateMemory(a, vm.stack.head)
      .stackPop()
      .moveInstructionPointer
  }
}

case class Eq(a: Int, b: Int, c: Int) extends Instruction3 {
  override def applyTo(vm: VM) = {
    val value =
      if (vm.toValue(b) == vm.toValue(c)) 1
      else 0
    vm.updateMemory(a, value).moveInstructionPointer
  }
}

case class Gt(a: Int, b: Int, c: Int) extends Instruction3 {
  override def applyTo(vm: VM) = {
    val value =
      if (vm.toValue(b) > vm.toValue(c)) 1
      else 0
    vm.updateMemory(a, value).moveInstructionPointer
  }
}

case class Jump(a: Int) extends Instruction1 {
  override def applyTo(vm: VM) = vm.moveInstructionPointer(a)
}

case class JumpT(a: Int, b: Int) extends Instruction2 {
  override def applyTo(vm: VM) = {
    if (vm.toValue(a) != 0)
      vm.moveInstructionPointer(vm.toValue(b))
    else
      vm.moveInstructionPointer
  }
}

case class JumpF(a: Int, b: Int) extends Instruction2 {
  override def applyTo(vm: VM) = {
    if (vm.toValue(a) == 0)
      vm.moveInstructionPointer(vm.toValue(b))
    else
      vm.moveInstructionPointer
  }
}

case class Add(a: Int, b: Int, c: Int) extends Instruction3 {
  override def applyTo(vm: VM) = {
    vm
      .updateMemory(a, (vm.toValue(b) + vm.toValue(c)) % 32768)
      .moveInstructionPointer
  }
}


case class Multiply(a: Int, b: Int, c: Int) extends Instruction3 {
  override def applyTo(vm: VM) = {
    vm
      .updateMemory(a, (vm.toValue(b) * vm.toValue(c)) % 32768)
      .moveInstructionPointer
  }
}

case class Modulo(a: Int, b: Int, c: Int) extends Instruction3 {
  override def applyTo(vm: VM) = {
    vm
      .updateMemory(a, vm.toValue(b) % vm.toValue(c))
      .moveInstructionPointer
  }
}

case class And(a: Int, b: Int, c: Int) extends Instruction3 {
  override def applyTo(vm: VM) = {
    vm
      .updateMemory(a, vm.toValue(b) & vm.toValue(c))
      .moveInstructionPointer
  }
}

case class Or(a: Int, b: Int, c: Int) extends Instruction3 {
  override def applyTo(vm: VM) = {
    vm
      .updateMemory(a, vm.toValue(b) | vm.toValue(c))
      .moveInstructionPointer
  }
}

case class Not(a: Int, b: Int) extends Instruction2 {
  override def applyTo(vm: VM) = {
    vm
      .updateMemory(a, (~vm.toValue(b)) & 0xFFFF)
      .moveInstructionPointer
  }
}

case class RMem(a: Int, b: Int) extends Instruction2 {
  override def applyTo(vm: VM) = {
    vm
      .updateMemory(a, vm.toValue(b))
      .moveInstructionPointer
  }
}

case class WMem(a: Int, b: Int) extends Instruction2 {
  override def applyTo(vm: VM) = {
    vm
      .updateMemory(vm.toValue(a), vm.toValue(b))
      .moveInstructionPointer
  }
}

case class Call(a: Int) extends Instruction1 {
  override def applyTo(vm: VM) = {
    vm
      .stackPush(vm.instructionPointer + 1)
      .moveInstructionPointer(vm.toValue(a))
  }
}

case object Ret extends Instruction0 {
  override def applyTo(vm: VM) = {
    vm
      .moveInstructionPointer(vm.stack.head)
      .stackPop()
  }
}

case class Out(a: Int) extends Instruction1 {
  override def applyTo(vm: VM) = {
    print(a.toChar)
    vm.moveInstructionPointer
  }

  override def toString = s"Out(${a.toChar})"
}

case class In(a: Int) extends Instruction1 {
  override def applyTo(vm: VM) = {
    val char = scala.io.StdIn.readChar() & 0xFFFF
    vm.updateMemory(a, char)
  }
}

case object Noop extends Instruction0

case object Unknown extends Instruction0

