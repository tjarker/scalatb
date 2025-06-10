package scalatb

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

import shared._
import Verilator.Arguments._

import scala.sys.process._

class VerilatorSmokeTest extends AnyFunSpec with Matchers {

  describe("Verilator") {
    it("should have a non-empty version string") {
      val version = Verilator.getVersion
      version should not be empty
      val matcher = """\d.\d+""".r
      matcher.findFirstIn(version.get) should not be empty
    }

    it("should run with a simple test") {
      val code = """
        |module test;
        |  initial begin
        |    $display("Hello, Verilator!");
        |    $finish;
        |  end
        |endmodule
        """.stripMargin

      val workingDir = "build/verilator-test".toDir
      val src = workingDir.addFile("test.sv", code)
      val buildDir = workingDir / "model"
      workingDir.addSubDir(buildDir)

      val result = Verilator(
        Seq(CC, Build, Main, EXE, BuildDir(buildDir.getAbsolutePath())),
        Seq(src)
      )
      result shouldBe a[Success[_]]

      // Check if the executable was created
      val exe = "build/verilator-test/model/Vtest".toFile
      workingDir.addArtifact(exe)
      exe.exists() shouldBe true

      // Run the executable
      val output = Seq("build/verilator-test/model/Vtest").!!

      output should startWith("Hello, Verilator!")

      workingDir.delete()
    }

  }

}
