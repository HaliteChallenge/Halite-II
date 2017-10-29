package halite

import java.io.FileWriter
import java.io.IOException

class DebugLog private constructor(private val file: FileWriter) {
    companion object {
        private var instance: DebugLog? = null

        internal fun initialize(f: FileWriter) {
            instance = DebugLog(f)
        }

        fun addLog(message: String) {
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
