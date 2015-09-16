package com.fathomdynamics.fcl

import scala.collection.mutable._

import scala.util.parsing.combinator.JavaTokenParsers
/*
FUNCTION_BLOCK
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
  val varDecls = Map[String, String]()

  // literals
  def eol: Parser[Any] = """[^\r\n]+""".r

  def varType: Parser[String] = "REAL" | "INT"

  def varName: Parser[String] = ident
  // (((humidity~:)~REAL)~None)
  def decl : Parser[(String, String)] = varName~":"~varType ^^ {
    case name~":"~varType =>
    {
      varDecls += name -> varType
      (name, varType)
    }}

  def varInput : Parser[Any] = "VAR_INPUT"~rep(decl~opt(";"))~"END_VAR"
  def varOutput : Parser[Any] = "VAR_OUTPUT"~rep(decl~opt(";"))~"END_VAR"

}

