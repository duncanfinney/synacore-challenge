import scala.annotation.tailrec

object Main extends App {

  val debug = false

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

  val initialMemory = BinReader
    .getBytes("src/main/resources/challenge.bin")
    .zipWithIndex
    .map { case (value, idx) => idx -> value }
    .toMap
    .withDefault(_ => 0)

  val vm = VM(initialMemory, List(), 0, debug)
  execute(vm)

}


