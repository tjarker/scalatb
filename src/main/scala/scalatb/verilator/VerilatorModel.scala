package scalatb.verilator

import java.io.File

import chisel3._
import scalatb.SharedObject
import scalatb.WorkingDirectory

import shared.PathToFileOps

object VerilatorModel {

  class SymbolId {
    private var id: Long = 0L
    def next(): Long = {
      val currentId = id
      id += 1
      currentId
    }
  }

  trait Symbol {
    def ref: String
    def id: Long
    def width: Int
    def access: String = this match {
      case p: Port   => ref
      case s: Symbol => "rootp->" + ref
    }
    val name: String = ref.split("__DOT__").last
    val hierName: String = ref.split("__DOT__") match {
      case Array(single) => single
      case parts         => parts.tail.mkString(".")
    }

    override def toString(): String = this match {
      case Input(_, _, _)         => s"input<$width> $hierName"
      case Output(_, _, _)        => s"output<$width> $hierName "
      case Signal(_, _, _)        => s"signal<$width> $hierName"
      case Memory(_, _, depth, _) => s"mem<$width x $depth> $hierName"
    }
  }
  trait Port extends Symbol
  trait Internal extends Symbol

  case class Input(ref: String, width: Int, id: Long) extends Port

  case class Output(ref: String, width: Int, id: Long) extends Port

  case class Signal(ref: String, width: Int, id: Long) extends Internal

  case class Memory(ref: String, width: Int, depth: Int, id: Long)
      extends Internal

  def create(
      name: String,
      dir: WorkingDirectory,
      sources: Seq[File],
      verilatorOptions: Seq[Verilator.Argument],
      options: Seq[String]
  ): VerilatorModel = {

    val verDir = dir.addSubDir(dir / "verilator")

    Verilator(
      Seq(
        Verilator.Arguments.CC,
        Verilator.Arguments.Build,
        Verilator.Arguments.PublicFlatRW,
        Verilator.Arguments.TopModule(name),
        Verilator.Arguments.TraceFst,
        Verilator.Arguments.BuildDir(verDir.path),
        Verilator.Arguments.CFlags("-fPIC -fpermissive")
      ) ++ verilatorOptions,
      sources
    ).get

    val symbols = SymbolCollector.collect(verDir / s"V${name}___024root.h")

    val harness = VerilatorModelHarness.writeHarness(
      dir,
      name,
      symbols
    )

    val sharedObject = SharedObject.create(
      libname = s"lib$name",
      dir = dir,
      sources = Seq(
        s"libV${name}.a",
        s"libverilated.a",
        s"verilated_fst_c.o",
        s"V${name}__ALL.a"
      ).map(n => verDir / n) ++ Seq(harness),
      options =
        Verilator.getIncludeDir().get.map(i => s"-I$i") ++ Seq("-lz", s"-I${verDir.path}") ++ options
    )

    new VerilatorModel(name, sharedObject, symbols, dir)
  }

}

import VerilatorModel._

class VerilatorModel(name: String, sharedObject: SharedObject, symbols: Seq[Symbol], dir: WorkingDirectory) {

  val lib = sharedObject.load()

  val createContextHandle = lib.getFunction(s"${name}_create_context")
  val deleteContextHandle = lib.getFunction(s"${name}_delete_context")
  val ptr = createContextHandle.invokePointer(Array((dir / "wave.fst").getAbsolutePath(), "1ns", Array.empty[String], 0))
  def delete(): Unit = {
    deleteContextHandle.invoke(Array(ptr))
  }

  val tickHandle = lib.getFunction(s"${name}_tick")
  def tick(delta: Long): Unit = {
    tickHandle.invoke(Array(ptr, delta))
  }

  val setHandle = lib.getFunction(s"${name}_set")
  def set(id: Long, value: Long): Unit = setHandle.invoke(Array(ptr, id, value))

  val quack = lib.getFunction("quack")

  quack.invoke(Array.empty)

  val hierName2Symbol: Map[String, Symbol] =
    symbols.map(s => s.hierName -> s).toMap

  def update(hierName: String, value: Data): Unit = {
    val symbol = hierName2Symbol.getOrElse(
      hierName,
      throw new IllegalArgumentException(s"Symbol $hierName not found.")
    )
    if (symbol.width > 64) {
      throw new IllegalArgumentException(
        s"Cannot set value for symbol $hierName with width ${symbol.width} > 64 bits."
      )
    }
    set(symbol.id, value.litValue.longValue)
  }

}

object VerilatoModelTest extends App {

  // Example usage of VserilatorModel
  val model = VerilatorModel.create(
    name = "MySvModule",
    dir = WorkingDirectory("./build/MyNewSvModule"),
    sources = Seq("MySvModule.sv".toFile),
    verilatorOptions = Seq(),
    options = Seq()
  )

  model("clk") = 1.U

  model.tick(100)

  model("clk") = 0.U

  model.tick(100)

  

  model.delete()

  println("VerilatorModel test completed successfully.")

}
