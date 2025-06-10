package scalatb

import com.sun.jna.NativeLibrary

import java.io.File
import scala.sys.process.Process

import shared._


class SharedObject(libFile: File) {

  def load(): NativeLibrary = {
    if (!libFile.exists()) {
      throw new RuntimeException(
        s"Shared object file ${libFile.getAbsolutePath} does not exist."
      )
    }
    NativeLibrary.getInstance(libFile.getAbsolutePath)
  }

}

object SharedObject {

  def compiler: String = {
    if (System.getProperty("os.name").toLowerCase.contains("windows")) "x86_64-w64-mingw32-gcc"
    else "gcc"
  }

  def gcc(
      sources: Seq[File],
      output: File,
      options: Seq[String] = Seq("-shared", "-fPIC")
  ): Unit = {
    val cmd = Seq("gcc") ++
      options ++
      sources.map(_.getAbsolutePath) ++
      Seq(
        "-o",
        output.getAbsolutePath
      )
    val process = Process(cmd)
    val exitCode = process.!
    if (exitCode != 0) {
      throw new RuntimeException(s"Compilation failed with exit code $exitCode")
    }
  }

  def sharedLibraryExtension: String = {
    if (System.getProperty("os.name").toLowerCase.contains("windows")) ".dll"
    else if (System.getProperty("os.name").toLowerCase.contains("mac")) ".dylib"
    else ".so"
  }

  def create(
      libname: String,
      dir: WorkingDirectory,
      sources: Seq[File],
      options: Seq[String] = Seq.empty
  ): SharedObject = {
    val libFile = new File(dir.dir, libname + sharedLibraryExtension)
    gcc(sources, libFile, Seq("-shared", "-fPIC") ++ options)
    dir.addArtifact(libFile)
    new SharedObject(libFile)
  }

}
