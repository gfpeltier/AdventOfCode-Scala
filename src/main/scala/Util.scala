import scala.io.Source

object Util {
  def readInputLines(fpath: String): List[String] = {
    val sbuf = Source.fromFile(fpath)
    val lines = sbuf.getLines.toList
    sbuf.close()
    lines
  }

  def readInput(fpath: String): String = {
    val sbuf = Source.fromFile(fpath)
    val s = sbuf.mkString
    sbuf.close()
    s
  }
}
