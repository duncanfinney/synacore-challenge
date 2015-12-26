import java.io.{FileInputStream, FileOutputStream, IOException, File}

object Main extends App {
  var in: Option[FileInputStream] = None
  try {
    in = Some(new FileInputStream("src/main/resources/challenge.bin"))
    var c = 0
    while ({c = in.get.read; c != -1}) {
      println(c)
    }
  } catch {
    case e: IOException => e.printStackTrace
  } finally {
    if (in.isDefined) in.get.close
  }
}
