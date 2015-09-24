package com.fathomdynamics.fcl

import scala.collection.mutable._

import scala.util.parsing.combinator.JavaTokenParsers
/*
FUNCTION_BLOCK F_Block1
VAR_INPUT
  humidity : REAL;
  color : color_type
END_VAR

VAR_INPUT
    <variable name> REAL; (* RANGE(<variable minimum value> .. <variable maximum value>) *)
END_VAR
VAR_OUTPUT
Valve: REAL;
END_VAR
FUZZIFY <variable name>
    TERM <term (or set) name> := <points that make up the term> ;
END_FUZZIFY
DEFUZZIFY valve
    METHOD: <defuzzification method>;
END_DEFUZZIFY
RULEBLOCK <ruleblock name>
    <operator>:<algorithm>;
    ACCUM:<accumulation method>;
    RULE <rule number>: IF <condition> THEN <conclusion>;
END_RULEBLOCK

END_FUNCTION_BLOCK
 */
class FclEngine extends JavaTokenParsers {
  val vars = Set[String]()
  val inDecls = Map[String, String]()
  val outDecls = Map[String, String]()

  // literals
  def eol: Parser[Any] = """[^\r\n]+""".r
// 61131-3-2003 Section
  def varType: Parser[String] = "REAL" | "INT" | "BOOL" | "SINT" | "INT" | "DINT" | "LINT" | "USINT" | "UINT" | "UDINT" | "ULINT" |
                                "BYTE" | "WORD"|"DWORD"|"LWORD" | "REAL" | "LREAL" | "TIME" | "DATE" | "TIME_OF_DAY" | "DATE_AND_TIME" | "STRING" | "WSTRING"
  def varName: Parser[String] = ident
  // (((humidity~:)~REAL)~None)
  def inputDecl : Parser[(String, String)] = varName~":"~varType ^^ {
    case name~":"~varType =>
    {
      inDecls += name -> varType
      (name, varType)
    }}

  def outputDecl : Parser[(String, String)] = varName~":"~varType ^^ {
    case name~":"~varType =>
    {
      outDecls += name -> varType
      (name, varType)
    }}

  def varInput : Parser[Any] = "VAR_INPUT"~rep(inputDecl~opt(";"))~"END_VAR"
  def varOutput : Parser[Any] = "VAR_OUTPUT"~rep(outputDecl~opt(";"))~"END_VAR"

  def funcBlock : Parser[Any] = "FUNCTION_BLOCK"~varName~varInput~varOutput~ "END_FUNCTION_BLOCK"

  def funcBlock : Parser[Any] = "FUNCTION_BLOCK"~varName~varInput~"END_FUNCTION_BLOCK"

}

