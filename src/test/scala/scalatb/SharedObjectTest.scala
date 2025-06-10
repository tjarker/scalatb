package scalatb

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import shared._
class SharedObjectTest extends AnyFunSpec with Matchers {

  describe("SharedObject") {
    it("should load a shared object and invoke a function") {
      // Assuming the SharedObject and related classes are defined as in the provided code

      val funName = "hello"

      val code = s"""
        |#include <stdio.h>
        |int $funName(int a, int b) {
        |  printf("Hello from C!\\n");
        |  return a + b;
        |}
        """.stripMargin

      val workingDir = "build/so-test".toDir

      val src = workingDir.addFile("hello.c", code)

      val sharedObject = SharedObject.create("libhello", workingDir, Seq(src))

      val nativeLib = sharedObject.load()

      val helloFunction = nativeLib.getFunction(funName)

      helloFunction.invokeInt(Array(11, 31)) shouldEqual 42

      workingDir.clean()
    }
  }

}
