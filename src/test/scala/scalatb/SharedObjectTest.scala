package scalatb

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import shared._
import java.nio.file.Files
class SharedObjectTest extends AnyFunSpec with Matchers {

  describe("SharedObject") {
    it("should load a shared object and invoke a function") {
 
      val funName = "hello"

      val code = s"""
        |#include <stdio.h>
        |extern "C" int $funName(int a, int b) {
        |  return a + b;
        |}
        """.stripMargin

      val workingDir = new WorkingDirectory(Files.createTempDirectory("verilator-model-test-").toFile())

      val src = workingDir.addFile("hello.cpp", code)

      val sharedObject = SharedObject.createRecipe("libhello", workingDir, Seq(src)).invoke()

      val nativeLib = sharedObject.load()

      val helloFunction = nativeLib.getFunction(funName)

      helloFunction.invokeInt(Array(11, 31)) shouldEqual 42

      workingDir.delete()
    }
  }

}
