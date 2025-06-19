package scalatb.verilator

import java.io.File
import scala.io.Source

import VerilatorModel._
import scala.util.matching.Regex

object SymbolCollector {

  class Matcher[T <: Symbol](regex: Regex, builder: (Seq[String], Long) => T) {
    def tryMatch(line: String, idGen: SymbolId): Option[T] = {
      regex
        .findFirstMatchIn(line)
        .map(m => builder(m.subgroups, idGen.next()))
        .filterNot(_.ref.startsWith("__"))
    }
  }

  case object SignalMatcher
      extends Matcher(
        raw"""\wData/\*(\d+):(\d+)*\*/\s(\w+);""".r,
        (xs, id) =>
          Signal(
            ref = xs(2),
            width = xs(0).toInt - xs(1).toInt + 1,
            id
          )
      )

  case object WideSignalMatcher
      extends Matcher(
        raw"""VlWide<(\d+)>/\*(\d+):(\d+)\*/\s+(\w+);""".r,
        (xs, id) =>
          Signal(
            ref = xs(3),
            width = xs(1).toInt - xs(2).toInt + 1,
            id
          )
      )

  case object InputMatcher
      extends Matcher(
        raw"""VL_IN\d*\((\w+),(\d+),(\d+)\);""".r,
        (xs, id) =>
          Input(
            ref = xs(0),
            width = xs(1).toInt - xs(2).toInt + 1,
            id
          )
      )

  case object WideInputMatcher
      extends Matcher(
        raw"""VL_INW\((\w+),(\d+),(\d+),(\d+)\);""".r,
        (xs, id) =>
          Input(
            ref = xs(0),
            width = xs(1).toInt - xs(2).toInt + 1,
            id
          )
      )

  case object OutputMatcher
      extends Matcher(
        raw"""VL_OUT\d*\((\w+),(\d+),(\d+)\);""".r,
        (xs, id) =>
          Output(
            ref = xs(0),
            width = xs(1).toInt - xs(2).toInt + 1,
            id
          )
      )

  case object WideOutputMatcher
      extends Matcher(
        raw"""VL_OUTW\((\w+),(\d+),(\d+),(\d+)\);""".r,
        (xs, id) =>
          Output(
            ref = xs(0),
            width = xs(1).toInt - xs(2).toInt + 1,
            id
          )
      )

  case object MemoryMatcher
      extends Matcher(
        raw"""VlUnpacked<\w+/\*(\d+):(\d+)\*/,\s+(\d+)>\s+(\w+);""".r,
        (xs, id) =>
          Memory(
            ref = xs(3),
            width = xs(0).toInt - xs(1).toInt + 1,
            depth = xs(2).toInt,
            id
          )
      )

  case object MemoryMatcherWide
      extends Matcher(
        raw"""VlUnpacked<\w+<\d+>/\*(\d+):(\d+)\*/,\s+(\d+)>\s+(\w+);""".r,
        (xs, id) =>
          Memory(
            ref = xs(3),
            width = xs(0).toInt - xs(1).toInt + 1,
            depth = xs(2).toInt,
            id
          )
      )

  val matchers = Seq(
    SignalMatcher,
    WideSignalMatcher,
    InputMatcher,
    WideInputMatcher,
    OutputMatcher,
    WideOutputMatcher,
    MemoryMatcher,
    MemoryMatcherWide
  )

  def collect(file: File): Seq[Symbol] = {
    val source = Source.fromFile(file)
    val lines = source.getLines().toSeq
    source.close()

    val idGen = new SymbolId

    lines
      .flatMap { line =>
        matchers.flatMap(_.tryMatch(line, idGen))
      }
      .distinctBy(_.hierName)
  }

}

import shared.PathToFileOps

object SymbolCollectorTest extends App {
  SymbolCollector
    .collect("./build/MySvModule/VMySvModule___024root.h".toFile)
    .foreach(println)
}
