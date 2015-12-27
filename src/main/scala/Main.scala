import scala.annotation.tailrec

object Main extends App {

  val debug = true

  def debugMemory(vm: VM): Unit = {
    if (!debug) {
      return
    }
    val registerOutput = (for (r <- 32768 to 32775) yield String.format("%-5s", vm.memory(r).toString)).mkString(" ")
    println(
      f"${vm.instructionPointer}%6s | $registerOutput%30s | ${vm.stack}%-10s | => ${vm.currentInstruction.debugString}"
    )

  }

  @tailrec
  def execute(vm: VM): VM = {
    debugMemory(vm)
    execute(vm.currentInstruction.applyTo(vm))
  }

  def disassemble(mem: Map[Int, Int], addr: Int = 0) : Unit = {
    val ins = BinReader.getInstructionAtLocation(mem, addr)
    println(f"[$addr%05d] -> $ins")
    disassemble(mem, addr + ins.paramCount + 1)
  }

  val initialMemory = BinReader
    .getBytes("src/main/resources/challenge.bin")
    .zipWithIndex
    .map { case (value, idx) => idx -> value }
    .toMap
    .withDefault(_ => 0)

//  disassemble(initialMemory)

    val vm = VM(initialMemory, List(), 0, debug)
    execute(vm)

}


