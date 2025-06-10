package scalatb

import scala.util.{Try, Success, Failure}
import coursier.core.Repository.Complete.Input.Ver
import scala.collection.mutable.ListBuffer

object Verilator {

  import scala.sys.process._

  sealed trait Argument {
    def toStrings: Seq[String] = this match {
      case CC                       => Seq("--cc")
      case EXE                      => Seq("--exe")
      case Main                     => Seq("--main")
      case Build                    => Seq("--build")
      case Help                     => Seq("--help")
      case Version                  => Seq("--version")
      case Assertions               => Seq("--assert")
      case Coverage                 => Seq("--coverage")
      case PublicFlatRW             => Seq("--public-flat-rw")
      case SystemVerilog            => Seq("-sv")
      case TraceVcd                 => Seq("--trace")
      case TraceFst                 => Seq("--trace-fst")
      case Timing                   => Seq("--timing")
      case NoTiming                 => Seq("--no-timing")
      case TopParam(name, value)    => Seq(s"-G$name=$value")
      case Include(path)            => Seq(s"-I$path")
      case Jobs(n)                  => Seq("-j", n.toString)
      case LdFlags(flags)           => Seq("-LDFLAGS", flags)
      case CFlags(flags)            => Seq("-CFLAGS", flags)
      case OptimizationLevel(level) => Seq(s"-O$level")
      case TopModule(name)          => Seq("--top-module", name)
      case BuildDir(path)           => Seq("--Mdir", path)
      case CreateDpiLib(name)       => Seq("--lib-create", name)
      case DefaultTimeScale(scale)  => Seq("--timescale", scale)
      case OverrideTimeScale(scale) => Seq("--timescale-override", scale)
      case CustomFlag(flag)         => Seq(flag)
    }
  }
  case object CC extends Argument
  case object EXE extends Argument
  case object Main extends Argument
  case object Build extends Argument
  case object Help extends Argument
  case object Version extends Argument
  case object Assertions extends Argument
  case object Coverage extends Argument
  case object PublicFlatRW extends Argument
  case object SystemVerilog extends Argument
  case object TraceVcd extends Argument
  case object TraceFst extends Argument
  case object Timing extends Argument
  case object NoTiming extends Argument
  case class TopParam(name: String, value: String) extends Argument
  case class Include(path: String) extends Argument
  case class Jobs(n: Int) extends Argument
  case class LdFlags(flags: String) extends Argument
  case class CFlags(flags: String) extends Argument
  case class OptimizationLevel(level: String) extends Argument
  case class TopModule(name: String) extends Argument
  case class BuildDir(path: String) extends Argument
  case class CreateDpiLib(name: String) extends Argument
  case class DefaultTimeScale(scale: String) extends Argument
  case class OverrideTimeScale(scale: String) extends Argument
  case class CustomFlag(flag: String) extends Argument

  def getVersion: Option[String] =
    Verilator(Seq(Version), Seq()).map(_.trim.split(" ").apply(1)).toOption

  def apply(args: Seq[Argument], files: Seq[String]): Try[String] = {
    val command = Seq("verilator") ++ args.flatMap(_.toStrings) ++ files
    println(command.mkString(" "))
    Try {
      val stdout = new StringBuilder
      val stderr = new StringBuilder
      val logger = ProcessLogger(
        s => stdout.append(s + "\n"),
        s => stderr.append(s + "\n")
      )
      val exitCode = command.!(logger)
      if (exitCode == 0)
        stdout.toString
      else
        throw new Exception(
          s"Verilator failed with exit code $exitCode\n" +
            s"Command: ${command.mkString(" ")}\n" +
            s"Error: ${stderr.toString}"
        )
    }
  }
}

object VerilatorTest extends App {
  println("Verilator version: " + Verilator.getVersion.getOrElse("Not found"))

  val res = Verilator(
    Seq(
      Verilator.CC,
      Verilator.Build,
      Verilator.SystemVerilog,
      Verilator.BuildDir("build_bb"),
      Verilator.TopModule("MyBlackBox"),
      Verilator.PublicFlatRW,
      Verilator.Jobs(6)
    ),
    Seq(
      "src/main/resources/blackbox.sv"
    )
  )
  println(res)

  val res2 = Verilator(
    Seq(
      Verilator.CC,
      Verilator.Build,
      Verilator.PublicFlatRW,
      Verilator.BuildDir("build_dtuss"),
      Verilator.TopModule("DtuSubsystem"),
      Verilator.Jobs(6)
    ),
    Seq(
      "DtuSubsystem.sv"
    )
  )

  println(res2)

  val res3 = Verilator(
    Seq(
      Verilator.CC,
      Verilator.Build,
      Verilator.PublicFlatRW,
      Verilator.BuildDir("build_sram"),
      Verilator.TopModule("mysram"),
      Verilator.Jobs(6)
    ),
    Seq(
      "mysram.sv"
    )
  )

  println(res3)
}
