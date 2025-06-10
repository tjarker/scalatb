package scalatb

import java.io.File
import scala.collection.mutable.ArrayBuffer

object WorkingDirectory {
    def apply(dir: String): WorkingDirectory = {
        val dirFile = new File(dir)
        dirFile.mkdirs()
        if (!dirFile.isDirectory) {
            throw new IllegalArgumentException(s"Path $dir is not a directory.")
        }
        new WorkingDirectory(dirFile)
    }
}

class WorkingDirectory(val dir: File) {

    val artifacts = new ArrayBuffer[File]()


    def addFile(name: String, content: String): File = {
        val file = new File(dir, name)
        file.getParentFile.mkdirs()
        java.nio.file.Files.write(file.toPath, content.getBytes("UTF-8"), java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
        artifacts += file
        file
    }

    def addArtifact(file: File): Unit = {
        if (!file.exists()) {
            throw new IllegalArgumentException(s"File ${file.getAbsolutePath} does not exist.")
        }
        artifacts += file
    }

    def deleteArtifact(file: File): Unit = {
        if (file.exists()) {
            if (file.isDirectory) {
                file.list().foreach { child =>
                    deleteArtifact(new File(file, child))
                }
            }
            file.delete()
        }
    }

    def clean(): Unit = {
        artifacts.foreach(deleteArtifact)
        artifacts.clear()
    }

    def delete(): Unit = {
        clean()
        dir.delete()
    }

}
