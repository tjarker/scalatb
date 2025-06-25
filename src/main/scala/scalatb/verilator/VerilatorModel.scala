package scalatb.verilator

import java.io.File

import chisel3.UInt
import chisel3.Data
import chisel3.UIntFactory
import chisel3._
import scalatb.SharedObject
import scalatb.WorkingDirectory

import scala.language.implicitConversions

import shared.{PathToFileOps, BigIntOps, WordArrayOps}
import VerilatorModel._

object VerilatorModel {

  class SymbolId {
    private var id: Long = 0L
    def next(): Long = {
      val currentId = id
      id += 1
      currentId
    }
  }

  class SymbolHandle(val s: Symbol, m: VerilatorModelInstance) {

    def :=(value: Data): Unit = {
      m.setSymbol(s, value)
    }

    def apply(index: Int): SymbolIndexHandle = {
      s match {
        case Memory(ref, width, depth, id) =>
          new SymbolIndexHandle(s, m, index)
        case _ =>
          throw new IllegalArgumentException(s"Symbol $s is not a memory.")
      }
    }

    def peekInt(): BigInt = {
      m.getSymbol(s).litValue
    }

    def peek(): UInt = {
      m.getSymbol(s)
    }

  }

  implicit def symbolHandleToUInt(handle: SymbolHandle): UInt = {
    handle.peek()
  }

  implicit def symbolHandleToBigInt(handle: SymbolHandle): BigInt = {
    handle.peekInt()
  }

  implicit def symbolHandleToInt(handle: SymbolHandle): Int = {
    handle.peekInt().toInt
  }

  class SymbolIndexHandle(
      val s: Symbol,
      m: VerilatorModelInstance,
      index: Int
  ) {

    def :=(value: Data): Unit = {
      m.setSymbol(s, value, Some(index))
    }

  }

  object SymbolHandle {
    def unapply(handle: SymbolHandle): Option[Symbol] = {
      Some(handle.s)
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

  def build(
      name: String,
      dir: WorkingDirectory,
      sources: Seq[File],
      verilatorOptions: Seq[Verilator.Argument],
      options: Seq[String]
  ): VerilatorModel = {

    val verDir = dir.addSubDir(dir / "verilator")

    val artifacts = Verilator
      .createRecipe(
        verDir,
        name,
        Seq(
          Verilator.Arguments.CC,
          Verilator.Arguments.Build,
          Verilator.Arguments.PublicFlatRW,
          Verilator.Arguments.TraceFst,
          Verilator.Arguments.CFlags("-fPIC -fpermissive")
        ) ++ verilatorOptions,
        sources
      )
      .invoke()

    val symbols = SymbolCollector.collect(verDir / s"V${name}___024root.h")

    val harness = VerilatorModelHarness.writeHarness(
      dir,
      name,
      symbols
    )

    val sharedObject = SharedObject
      .createRecipe(
        libname = s"lib$name",
        dir = dir,
        sources = artifacts ++ Seq(harness),
        options = Verilator.getIncludeDir().get.map(i => s"-I$i") ++ Seq(
          "-lz",
          s"-I${verDir.path}"
        ) ++ options
      )
      .invoke()

    new VerilatorModel(name, sharedObject, symbols, dir)
  }

}

class VerilatorModel(
    name: String,
    sharedObject: SharedObject,
    symbols: Seq[Symbol],
    dir: WorkingDirectory
) {

  def create(): VerilatorModelInstance = {
    new VerilatorModelInstance(name, sharedObject, symbols, dir)
  }

}

class VerilatorModelInstance(
    name: String,
    sharedObject: SharedObject,
    symbols: Seq[Symbol],
    dir: WorkingDirectory
) {

  val lib = sharedObject.load()

  val createContextHandle =
    lib.getFunction(VerilatorModelHarness.createContextFunName(name))
  val deleteContextHandle =
    lib.getFunction(VerilatorModelHarness.deleteContextFunName(name))
  val evalHandle =
    lib.getFunction(VerilatorModelHarness.evalFunName(name))
  val tickHandle =
    lib.getFunction(VerilatorModelHarness.tickFunName(name))
  val setHandle =
    lib.getFunction(VerilatorModelHarness.setFunName(name))
  val getHandle =
    lib.getFunction(VerilatorModelHarness.getFunName(name))
  val setWideHandle =
    lib.getFunction(VerilatorModelHarness.setWideFunName(name))
  val getWideHandle =
    lib.getFunction(VerilatorModelHarness.getWideFunName(name))
  val getMemoryHandle =
    lib.getFunction(VerilatorModelHarness.getMemFunName(name))
  val setMemoryHandle =
    lib.getFunction(VerilatorModelHarness.setMemFunName(name))
  val getMemoryWideHandle =
    lib.getFunction(VerilatorModelHarness.getMemWideFunName(name))
  val setMemoryWideHandle =
    lib.getFunction(VerilatorModelHarness.setMemWideFunName(name))

  // Create a context for the model
  val contextPtr = createContextHandle.invokePointer(
    Array(
      (dir / "wave.fst").getAbsolutePath(),
      "1ns",
      Array.empty[String],
      0
    )
  )

  val hierName2Symbol: Map[String, Symbol] =
    symbols.map(s => s.hierName -> s).toMap

  def getSymbolByRef(ref: String): Symbol = {
    hierName2Symbol.getOrElse(
      ref,
      throw new IllegalArgumentException(s"Symbol $ref not found.")
    )
  }

  def delete(): Unit =
    deleteContextHandle.invoke(Array(contextPtr))

  def eval(): Unit =
    evalHandle.invoke(Array(contextPtr))

  def tick(delta: Long): Unit =
    tickHandle.invoke(Array(contextPtr, delta))

  def set(id: Long, value: Long): Unit =
    setHandle.invoke(Array(contextPtr, id, value))

  def get(id: Long) = getHandle.invokeLong(Array(contextPtr, id))

  def setWide(id: Long, value: Array[Int]) =
    setWideHandle.invoke(Array(contextPtr, id, value))

  def getWide(id: Long, value: Array[Int]) =
    getWideHandle.invoke(Array(contextPtr, id, value))

  def setMemory(id: Long, index: Long, value: Long): Unit =
    setMemoryHandle.invoke(Array(contextPtr, id, value, index))

  def getMemory(id: Long, index: Long): Long =
    getMemoryHandle.invokeLong(Array(contextPtr, id, index))

  def setMemoryWide(id: Long, index: Long, value: Array[Int]): Unit =
    setMemoryWideHandle.invoke(Array(contextPtr, id, value, index))

  def getMemoryWide(id: Long, index: Long, value: Array[Int]): Unit =
    getMemoryWideHandle.invoke(Array(contextPtr, id, value, index))

  def quack() = lib.getFunction("quack").invoke(Array.empty)

  def getSymbolHandle(hierName: String): SymbolHandle = {
    val symbol = getSymbolByRef(hierName)
    new SymbolHandle(symbol, this)
  }

  def apply(hierName: String): SymbolHandle = getSymbolHandle(hierName)

  def setSymbol(s: Symbol, value: Data, index: Option[Int] = None): Unit = {
    s match {
      case VerilatorModel.Input(ref, width, id) if width <= 64 =>
        set(id, value.litValue.toLong)

      case VerilatorModel.Input(ref, width, id) if width > 64 =>
        val valueArray = value.litValue.toWordArray
        setWide(id, valueArray)

      case VerilatorModel.Signal(ref, width, id) if width <= 64 =>
        set(id, value.litValue.toLong)

      case VerilatorModel.Signal(ref, width, id) if width > 64 =>
        val valueArray = value.litValue.toWordArray
        setWide(id, valueArray)

      case VerilatorModel.Memory(ref, width, depth, id) if width <= 64 =>
        index match {
          case Some(i) => setMemory(id, i, value.litValue.toLong)
          case None =>
            throw new IllegalArgumentException(
              s"Memory $ref requires an index."
            )
        }

      case VerilatorModel.Memory(ref, width, depth, id) if width > 64 =>
        index match {
          case Some(i) =>
            val valueArray = value.litValue.toWordArray
            setMemoryWide(id, i, valueArray)
          case None =>
            throw new IllegalArgumentException(
              s"Memory $ref requires an index."
            )
        }
      case _ => throw new IllegalArgumentException(s"Unknown symbol type: ${s}")
    }
  }

  def getSymbol(s: Symbol, index: Option[Int] = None): UInt = {
    s match {
      case VerilatorModel.Input(ref, width, id) if width <= 64 =>
        get(id).U(width.W)

      case VerilatorModel.Input(ref, width, id) if width > 64 =>
        val valueArray = Array.ofDim[Int]((width + 31) / 32)
        getWide(id, valueArray)
        valueArray.toBigInt.U(width.W)

      case VerilatorModel.Output(ref, width, id) if width <= 64 =>
        get(id).U(width.W)

      case VerilatorModel.Output(ref, width, id) if width > 64 =>
        val valueArray = Array.ofDim[Int]((width + 31) / 32)
        getWide(id, valueArray)
        valueArray.toBigInt.U(width.W)

      case VerilatorModel.Signal(ref, width, id) if width <= 64 =>
        get(id).U(width.W)

      case VerilatorModel.Signal(ref, width, id) if width > 64 =>
        val valueArray = Array.ofDim[Int]((width + 31) / 32)
        getWide(id, valueArray)
        valueArray.toBigInt.U(width.W)

      case VerilatorModel.Memory(ref, width, depth, id) if width <= 64 =>
        index match {
          case Some(i) => getMemory(id, i).U(width.W)
          case None =>
            throw new IllegalArgumentException(
              s"Memory $ref requires an index."
            )
        }

      case VerilatorModel.Memory(ref, width, depth, id) if width > 64 =>
        index match {
          case Some(i) =>
            val valueArray = Array.ofDim[Int]((width + 31) / 32)
            getMemoryWide(id, i, valueArray)
            valueArray.toBigInt.U(width.W)
          case None =>
            throw new IllegalArgumentException(
              s"Memory $ref requires an index."
            )

        }
    }
  }

}
object VerilatoModelTest extends App {

  // Example usage of VserilatorModel
  val model = VerilatorModel.build(
    name = "MySvModule",
    dir = "./build/MyNewSvModule".toDir,
    sources = Seq("MySvModule.sv".toFile),
    verilatorOptions = Seq(),
    options = Seq()
  )

  val dut = model.create()

  val clk = dut("clk")

  dut("clk") := 0.U

  dut.tick(100)

  clk := 0.U

  dut("passThrough.nestedModule.myMem")(1) := 0x01.U

  dut.tick(100)

  dut("myReg") := 0x12345678.U

  dut.tick(100)

  

  println(s"Output is: ${dut("myReg").peekInt()} (${dut("myReg").peek()})")
  println(dut("myReg") + 10)

  dut.delete()

  println("VerilatorModel test completed successfully.")


}
