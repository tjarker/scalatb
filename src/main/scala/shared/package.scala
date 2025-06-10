import java.io.File

import scalatb.WorkingDirectory

package object shared {

  implicit class PathToFileOps(val path: String) extends AnyVal {
    def toFile: File = new File(path)
    def toDir: WorkingDirectory = WorkingDirectory(path)
  }

}
