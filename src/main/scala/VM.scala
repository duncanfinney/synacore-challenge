case class VM(memory: Map[Int, Int] = Map().withDefault(_ => 0),
              stack: List[Int] = List(),
              instructions: List[Instruction],
              instructionPointer: Int) {

  val currentInstruction = instructions.lift(instructionPointer)

  def updateMemory(addr: Int, value: Int) = copy(memory = memory updated(addr, value))

  def stackPush(value: Int) = copy(stack = value :: stack)

  def stackPop() = copy(stack = stack.tail)

  def moveInstructionPointer: VM = moveInstructionPointer(instructionPointer + 1)

  def moveInstructionPointer(newAddr: Int): VM = copy(instructionPointer = newAddr)
}
