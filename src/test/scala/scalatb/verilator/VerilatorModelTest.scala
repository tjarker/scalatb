package scalatb.verilator

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files
import scalatb.WorkingDirectory

import chisel3._
import shared.PathToFileOps

class VerilatorModelTest extends AnyFunSpec with Matchers {

  describe("VerilatorModel") {

    it("should have a compilable harness") {
      val dir = new WorkingDirectory(Files.createTempDirectory("verilator-model-harness-test-").toFile())

      val name = "TestModel"

      import VerilatorModel._
      val harness = VerilatorModelHarness.writeHarness(dir, name, Seq(
        Input("valid", 1, 0),
        Output("ready", 1, 1),
        Input("data", 32, 2),
        Output("result", 32, 3)
      ))

      harness should not be null
      harness.exists() shouldBe true
      harness.isFile shouldBe true

      val content = Files.readString(harness.toPath)
      content should include(s"${name}_create_context")
      content should include(s"${name}_eval")
      content should include(s"${name}_delete_context")
      content should include(s"${name}_tick")
      content should include(s"${name}_set")
      content should include(s"${name}_get")
      content should include(s"${name}_set_wide")
      content should include(s"${name}_get_wide")
      content should include(s"${name}_set_mem")
      content should include(s"${name}_get_mem")
      content should include(s"${name}_set_mem_wide")
      content should include(s"${name}_get_mem_wide")

    }

    it("should create a harness for a pass-through module") {
      val dir = new WorkingDirectory(Files.createTempDirectory("verilator-model-passthrough-test-").toFile())

      val name = "PassThroughModule"

      val file = s"""module $name (
                            |  input logic [31:0] a,
                            |  output logic [31:0] b
                            |);
                            |  assign b = a;
                            |endmodule
                            |""".stripMargin

      val src = dir.addFile(s"$name.sv", file)

      val model = VerilatorModel.build(
        name,
        dir,
        Seq(src),
        Seq(Verilator.Arguments.SystemVerilog, Verilator.Arguments.TraceFst),
        Seq()
      )

      val dut = model.create()

      val a = dut("a")
      val b = dut("b")

      a := 42.U
      dut.tick(10)
      b.peekInt() shouldEqual 42

      dut.delete()

    }

  }

}
