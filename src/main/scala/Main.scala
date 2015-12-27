import scala.annotation.tailrec

object Main extends App {

  val instructions = BinReader.getInstructions("src/main/resources/challenge.bin").toList
  val vm = VM(instructions = instructions, instructionPointer = 0)

  def printDebug(vm: VM, in: Instruction) = {
    println(s"${vm.currentInstruction.getOrElse("none")}, VM(${vm.memory})")
  }

  @tailrec
  def execute(vm: VM): VM = vm.currentInstruction match {
    case Some(in) =>
      val nextState = in.applyTo(vm)
//      println(s"$in => VM(ins=${nextState.instructionPointer}, ${nextState.memory}")
      execute(nextState)
    case None => sys.exit(0)
  }

  execute(vm)

}


