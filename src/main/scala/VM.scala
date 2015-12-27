case class VM(memory: Map[Int, Int] = Map().withDefault(_ => 0),
              stack: List[Int] = List(),
              instructionPointer: Int,
              debug: Boolean = false) {

  val currentInstruction = BinReader.getInstructionAtLocation(memory, instructionPointer)

  def updateRegister(r: Int, value: Int) = copy(memory = memory updated(r + 32768, value))

  def updateMemory(addr: Int, value: Int) = copy(memory = memory updated(addr, value))

  def stackPush(value: Int) = copy(stack = value :: stack)

  def stackPop() = copy(stack = stack.tail)

  def moveInstructionPointer: VM = moveInstructionPointer(instructionPointer + 1 + currentInstruction.paramCount)

  def moveInstructionPointer(newAddr: Int): VM = copy(instructionPointer = newAddr)

  def toValue(num: Int) = {
    if (num < 32768) num
    else if (num < 32776) memory(num)
    else throw new Error(s"Num $num is not valid")
  }
}
