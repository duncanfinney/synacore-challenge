object Main extends App {

  val instructions = BinReader.getInstructions("src/main/resources/challenge.bin")

  println(instructions.toList.filter(_!=Unknown).mkString("\n"))


}


