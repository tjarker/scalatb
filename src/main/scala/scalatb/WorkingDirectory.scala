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
    val subdirs = new ArrayBuffer[WorkingDirectory]()

    def path: String = dir.getAbsolutePath

    def addFile(name: String, content: String): File = {
        val file = new File(dir, name)
        file.getParentFile.mkdirs()
        java.nio.file.Files.write(file.toPath, content.getBytes("UTF-8"), java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.TRUNCATE_EXISTING)
        artifacts += file
        file
    }

    def addSubDir(file: File): WorkingDirectory = {
        file.mkdirs()
        val subdir = new WorkingDirectory(file)
        subdirs += subdir
        subdir
    }

    def addArtifact(file: File): Unit = {
        artifacts += file
    }

    def /(name: String): File = {
        val file = new File(dir, name)
        file
    }

    def deleteArtifact(file: File): Unit = {
        if (file.exists() && !file.isDirectory) {
            file.delete()
        }
    }

    def clean(): Unit = {
        artifacts.foreach(deleteArtifact)
        subdirs.foreach(_.delete())
        artifacts.clear()
    }

    def delete(): Unit = {
        clean()
        dir.delete()
    }

}
