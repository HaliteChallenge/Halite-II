package halite

import java.io.FileWriter
import java.io.IOException

class Log private constructor(private val file: FileWriter) {
    companion object {
        private var instance: Log? = null

        internal fun initialize(f: FileWriter) {
            instance = Log(f)
        }

        fun log(message: String) {
            try {
                instance!!.file.write(message)
                instance!!.file.write("\n")
                instance!!.file.flush()
            } catch (e: IOException) {
                e.printStackTrace()
            }

        }
    }
}
