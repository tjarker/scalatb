package scalatb.verilator

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scalatb.SharedObject

import scalatb.verilator.VerilatorModel._
import scalatb.WorkingDirectory

object VerilatorModelHarness {

  def imports(m: String) =
    s"""|#include <verilated.h>
        |#include <verilated_fst_c.h>
        |#include <stdint.h>
        |#include "V$m.h"
        |#include "V${m}___024root.h"
        |""".stripMargin

  def contextStruct(m: String) =
    s"""|struct ${m}_context_t {
        |  uint64_t time;
        |  VerilatedContext* context;
        |  V$m* model;
        |  VerilatedFstC* trace;
        |};
        |""".stripMargin

  def createContext(m: String) =
    s"""|${m}_context_t* ${m}_create_context(const char* fstFile, const char* time_unit, char** argv, int argc) {
        |  ${m}_context_t* ctx = new ${m}_context_t;
        |  ctx->time = 0;
        |  ctx->context = new VerilatedContext;
        |  ctx->context->commandArgs(argc, argv);
        |  ctx->context->traceEverOn(true);
        |
        |  ctx->model = new V$m(ctx->context, "Circuit");
        |
        |  ctx->trace = new VerilatedFstC;
        |  ctx->model->trace(ctx->trace, 99);
        |  ctx->trace->set_time_unit(time_unit);
        |  ctx->trace->set_time_resolution(time_unit);
        |  ctx->trace->open(fstFile);
        |
        |  return ctx;
        |}
        |""".stripMargin

  def deleteContext(m: String) =
    s"""|void ${m}_delete_context(${m}_context_t* ctx) {
        |  ctx->model->final();
        |  ctx->trace->dump(ctx->time);
        |  ctx->trace->flush();
        |  ctx->trace->close();
        |
        |  delete ctx->trace;
        |  delete ctx->model;
        |  delete ctx->context;
        |  delete ctx;
        |}
        |""".stripMargin

  def eval(m: String) =
    s"""|void ${m}_eval(${m}_context_t* ctx) {
        |  ctx->model->eval();
        |}
        |""".stripMargin

  def tick(m: String) =
    s"""|void ${m}_tick(${m}_context_t* ctx, uint64_t delta) {
        |  ctx->model->eval();
        |  ctx->trace->dump(ctx->time);
        |  ctx->time += delta;
        |}
        |""".stripMargin

  def createCase(id: String, comment: String, body: String) =
    s"case $id: // $comment\n${body.indent(2)}"

  def set(m: String, syms: Seq[Symbol]) = {
    val cases = syms
      .filter(_.width <= 64)
      .collect {
        case i: Input =>
          createCase(
            i.id.toString(),
            i.toString(),
            s"ctx->model->${i.access} = value; break;"
          )
        case s: Signal =>
          createCase(
            s.id.toString(),
            s.toString(),
            s"ctx->model->${s.access} = value; break;"
          )
      }
      .mkString("\n")

    s"""|void ${m}_set(${m}_context_t* ctx, uint64_t id, uint64_t value) {
        |  switch (id) {
        |${cases.indent(4)}
        |  }
        |}
        |""".stripMargin
  }

  def setWide(m: String, syms: Seq[Symbol]) = {
    val cases = syms
      .filter(_.width > 64)
      .map(s => s -> (s.width / 32d).ceil.toInt)
      .collect { case (i @ Input(ref, _, id), words) =>
        s"""|case $id: // ${i.toString()}
            |  for (int i = 0; i < $words; i++) 
            |    ctx->model->${ref}.data()[i] = value[i];
            |  break;
            |""".stripMargin
      }
      .mkString("\n")

    s"""|void ${m}_set_wide(${m}_context_t* ctx, uint64_t id, uint32_t value[]) {
        |  switch (id) {
        |${cases.indent(4)}
        |  }
        |}
        |""".stripMargin
  }

  def get(m: String, syms: Seq[Symbol]) = {
    val cases = syms
      .filter(_.width <= 64)
      .filterNot(_.isInstanceOf[Memory])
      .collect { case s: Symbol =>
        s"""|case ${s.id}:
              |  return ctx->model->${s.access};
              |""".stripMargin
      }
      .mkString("\n")

    s"""|uint64_t ${m}_get(${m}_context_t* ctx, uint64_t id) {
        |  switch (id) {
        |${cases.indent(4)}
        |    default:
        |      return 0; // or handle error
        |  }
        |}
        |""".stripMargin
  }

  def getWide(m: String, syms: Seq[Symbol]) = {
    val cases = syms
      .filter(_.width > 64)
      .map(s => s -> (s.width / 32d).ceil.toInt)
      .collect { case (i @ Input(ref, _, id), words) =>
        s"""|case $id: // ${i.toString()}
            |  for (int i = 0; i < $words; i++) 
            |    value[i] = ctx->model->${ref}.data()[i];
            |  break;
            |""".stripMargin
      }
      .mkString("\n")

    s"""|void ${m}_get_wide(${m}_context_t* ctx, uint64_t id, uint32_t value[]) {
        |  switch (id) {
        |${cases.indent(4)}
        |  }
        |}
        |""".stripMargin
  }

  def setMem(m: String, syms: Seq[Symbol]) = {
    val cases = syms
      .filter(_.width <= 64)
      .collect { case m: Memory =>
        s"""|case ${m.id}: // ${m.toString()}
              |  ctx->model->${m.access}[index] = value;
              |  break;
              |""".stripMargin
      }
      .mkString("\n")

    s"""|void ${m}_set_mem(${m}_context_t* ctx, uint64_t id, uint64_t value, uint32_t index) {
        |  switch (id) {
        |${cases.indent(4)}
        |  }
        |}
        |""".stripMargin
  }

  def getMem(m: String, syms: Seq[Symbol]) = {
    val cases = syms
      .filter(_.width <= 64)
      .collect { case m: Memory =>
        s"""|case ${m.id}: // ${m.toString()}
              |  return ctx->model->${m.access}[index];
              |""".stripMargin
      }
      .mkString("\n")

    s"""|uint64_t ${m}_get_mem(${m}_context_t* ctx, uint64_t id, uint32_t index) {
        |  switch (id) {
        |${cases.indent(4)}
        |    default:
        |      return 0; // or handle error
        |  }
        |}
        |""".stripMargin
  }

  def setMemWide(m: String, syms: Seq[Symbol]) = {
    val cases = syms
      .filter(_.width > 64)
      .map(s => s -> (s.width / 32d).ceil.toInt)
      .collect { case (m: Memory, words) =>
        s"""|case ${m.id}: // ${m.toString()}
            |  for (int i = 0; i < $words; i++) 
            |    ctx->model->${m.access}[index][i] = value[i];
            |  break;
            |""".stripMargin
      }
      .mkString("\n")

    s"""|void ${m}_set_mem_wide(${m}_context_t* ctx, uint64_t id, uint32_t value[], uint32_t index) {
        |  switch (id) {
        |${cases.indent(4)}
        |  }
        |}
        |""".stripMargin
  }

  def getMemWide(m: String, syms: Seq[Symbol]) = {
    val cases = syms
      .filter(_.width > 64)
      .map(s => s -> (s.width / 32d).ceil.toInt)
      .collect { case (m: Memory, words) =>
        s"""|case ${m.id}: // ${m.toString()}
            |  for (int i = 0; i < $words; i++) 
            |    value[i] = ctx->model->${m.access}[index][i];
            |  break;
            |""".stripMargin
      }
      .mkString("\n")

    s"""|void ${m}_get_mem_wide(${m}_context_t* ctx, uint64_t id, uint32_t value[], uint32_t index) {
        |  switch (id) {
        |${cases.indent(4)}
        |  }
        |}
        |""".stripMargin
  }

  def harness(m: String, syms: Seq[VerilatorModel.Symbol]): String =
    s"""|${imports(m)}
        |
        |double sc_time_stamp() { return 0; }
        |
        |${contextStruct(m)}
        |extern "C" {
        |${createContext(m)}
        |${deleteContext(m)}
        |${eval(m)}
        |${tick(m)}
        |${set(m, syms).indent(2)}
        |${setWide(m, syms).indent(2)}
        |${get(m, syms).indent(2)}
        |${getWide(m, syms).indent(2)}
        |${setMem(m, syms).indent(2)}
        |${getMem(m, syms).indent(2)}
        |${setMemWide(m, syms).indent(2)}
        |${getMemWide(m, syms).indent(2)}
        |  void quack() {
        |    printf("Quack!\\n");
        |  }
        |}
        |""".stripMargin


  def writeHarness(dir: WorkingDirectory, m: String, syms: Seq[Symbol]) = {
    dir.addFile(s"${m}_harness.cpp", harness(m, syms))
  }

}

import shared.PathToFileOps

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

object HarnessTest extends App {

  val m = "MySvModule"
  Verilator(
    Seq(
      Verilator.Arguments.CC,
      Verilator.Arguments.Build,
      Verilator.Arguments.PublicFlatRW,
      Verilator.Arguments.TopModule(m),
      Verilator.Arguments.TraceFst,
      Verilator.Arguments.BuildDir(s"./build/$m"),
      Verilator.Arguments.CFlags("-fPIC -fpermissive")
    ),
    Seq(s"$m.sv".toFile)
  ).get

  val dir = s"./build/$m".toDir

  val h = VerilatorModelHarness.writeHarness(
    dir,
    m,
    SymbolCollector.collect(s"./build/$m/V${m}___024root.h".toFile)
  )

  val so = SharedObject.create(
    libname = s"lib$m",
    dir,
    sources = Seq(
      s"libV${m}.a",
      s"libverilated.a",
      s"verilated_fst_c.o",
      s"V${m}__ALL.a",
      s"harness.cpp"
    ).map(n => dir / n),
    options = Verilator.getIncludeDir().get.map(i => s"-I$i") ++ Seq("-lz")
  )

  val lib = so.load()

  lib.getFunction("quack").invoke(Array.empty)

}
