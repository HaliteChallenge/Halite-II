package hlt

import java.io.{FileWriter, IOException}

object DebugLog {
  private var writer: FileWriter = _

  def addLog(message: String): Unit =
    try {
      writer.write(message)
      writer.write('\n')
      writer.flush()
    } catch {
      case e: IOException =>
        e.printStackTrace()
    }

  private[hlt] def initialize(f: FileWriter): Unit = writer = f
}
