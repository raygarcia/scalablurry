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

case class Point(x:Any, y: Any){

}
class FclEngine extends JavaTokenParsers {
  val vars = Set[String]()

  def semiCol:Parser[Any] = opt(";")

  // literals
  def eol: Parser[Any] = """[^\r\n]+""".r
// 61131-3-2003 Section
  def varType: Parser[String] = "REAL" | "INT" | "BOOL" | "SINT" | "INT" | "DINT" | "LINT" | "USINT" | "UINT" | "UDINT" | "ULINT" |
                                "BYTE" | "WORD"|"DWORD"|"LWORD" | "REAL" | "LREAL" | "TIME" | "DATE" | "TIME_OF_DAY" | "DATE_AND_TIME" | "STRING" | "WSTRING"

  def value: Parser[String] = (decimalNumber)|(floatingPointNumber)|(varName) ^^ {case varName => {if (!inDecls.contains(varName)) throw(new Exception("var name " + varName + " doesn't exist as an input variable.")); varName}}
  def varName: Parser[String] = ident

  val inDecls = Map[String, String]()
  def inputDecl : Parser[(String, String)] = varName~":"~varType ^^ { case name~":"~varType => {inDecls += name -> varType;(name, varType)}}
  def varInput : Parser[Any] = "VAR_INPUT"~rep(inputDecl~semiCol)~"END_VAR"

  val outDecls = Map[String, String]()
  def outputDecl : Parser[(String, String)] = varName~":"~varType ^^ { case name~":"~varType => {outDecls += name -> varType; (name, varType)}}
  def varOutput : Parser[Any] = "VAR_OUTPUT"~rep(outputDecl~semiCol)~"END_VAR"

  def entries : Parser[Point] = value~","~value ^^ {case pX~","~pY => { Point(pX, pY)}}
  def point : Parser[Point] = "("~>entries<~")"

  val membershipFuncions = Map[String, List[Point]]()
  def termPair : Parser[Any] = ident~":="~rep(point) ^^ { case name~":="~pointList => {membershipFuncions += name -> pointList; (name -> pointList)}}

  def termDecl : Parser[Any] = "TERM"~termPair~semiCol
  def fuzzifyBlock : Parser[Any] = "FUZZIFY"~varName~rep(termDecl)~"END_FUZZIFY"
  def funcBlock : Parser[Any] = "FUNCTION_BLOCK"~varName~varInput~varOutput~fuzzifyBlock~"END_FUNCTION_BLOCK"
}

