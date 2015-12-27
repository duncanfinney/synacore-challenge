import scala.annotation.tailrec

object Main extends App {

  val debug = false

  def debugMemory(vm: VM): Unit = {
    println(vm.instructionPointer + " | [" + (for (r <- 32768 to 32775) yield vm.memory(r)).toList.mkString(" ") + "] | " + vm.currentInstruction)
  }

  @tailrec
  def execute(vm: VM): VM = {
    if (debug) {
      debugMemory(vm)
    }
    execute(vm.currentInstruction.applyTo(vm))
  }

  val initialMemory = BinReader
    .getBytes("src/main/resources/challenge.bin")
    .zipWithIndex
    .map { case (value, idx) => idx -> value }
    .toMap
    .withDefault(_ => 0)

  val vm = VM(initialMemory, List(), 0)
  execute(vm)

}


