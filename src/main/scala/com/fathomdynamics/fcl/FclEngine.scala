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
trait Validator{
  val vars = Set[String]()
  val inDecls = Map[String, String]()
  val outDecls = Map[String, String]()
  val localDecls = Map[String, String]()
  var errors = ListBuffer[String]()

  // make sure all vars used in points are declared
  def checkInDecls(varName: String) = {if (!inDecls.contains(varName)) {errors += ("var name \"" + varName + "\" doesn't exist as an input variable.");}}
  def checkOutDecls(varName: String) = {if (!outDecls.contains(varName)) {errors += ("var name \"" + varName + "\" doesn't exist as an output variable.");}}

  def dumpSemanticResults = if (errors.size > 0) {println(errors.size + " errors found.");errors.foreach(x => println("error: " + x)) }
}
case class Point(x:Any, y: Any){

}
class FclEngine extends JavaTokenParsers with Validator{
  def semiCol:Parser[Any] = opt(";")

  // literals
  def eol: Parser[Any] = """[^\r\n]+""".r
// 61131-3-2003 Section
  def varType: Parser[String] = ("REAL" | "INT" | "BOOL" | "SINT" | "INT" | "DINT" | "LINT" | "USINT" | "UINT" | "UDINT" | "ULINT" |
                                "BYTE" | "WORD"|"DWORD"|"LWORD" | "REAL" | "LREAL" | "TIME" | "DATE" | "TIME_OF_DAY" | "DATE_AND_TIME" | "STRING" | "WSTRING")<~semiCol

  def ptValue: Parser[String] = (decimalNumber)|(floatingPointNumber)|(varName) ^^ {case varName => {checkInDecls(varName); varName}}
  def varName: Parser[String] = ident

  def inputDecl : Parser[(String, String)] = varName~":"~varType ^^ { case name~":"~varType => {inDecls += name -> varType;(name, varType)}}
  def varInput : Parser[Any] = "VAR_INPUT"~rep(inputDecl)~"END_VAR"

  def outputDecl : Parser[(String, String)] = varName~":"~varType ^^ { case name~":"~varType => {outDecls += name -> varType; (name, varType)}}
  def varOutput : Parser[Any] = "VAR_OUTPUT"~rep(outputDecl)~"END_VAR"

  def localDeclVal : Parser[(String, String)] = varName~":"~varType ^^ { case name~":"~varType => {localDecls += name -> varType; (name, varType)}}
  def localDeclStmnt : Parser[Any] = "VAR_OUTPUT"~rep(localDeclVal)~"END_VAR"

  def entries : Parser[Point] = ptValue~","~ptValue ^^ {case pX~","~pY => { Point(pX, pY)}}
  def point : Parser[Point] = "("~>entries<~")"

  val membershipFuncions = Map[String, List[Point]]()
  def termPair : Parser[Any] = ident~":="~rep(point) ^^ { case name~":="~pointList => {membershipFuncions += name -> pointList; (name -> pointList)}}

  def termDecl : Parser[Any] = "TERM"~termPair~semiCol

  def openFuzzifyBlock : Parser[Any] = "FUZZIFY" ~> varName ^^ {case varName => {checkInDecls(varName); varName}}
  def fuzzifyBlock : Parser[Any] = openFuzzifyBlock~rep(termDecl)~"END_FUZZIFY"

  /*
  DEFUZZIFY variable_name
    RANGE(min..max);
    TERM term_name:= membership_function;
    defuzzification_method;
    default_value;
  END_DEFUZZIFY
   */

  val defuzRanges = Map[String, Point]()
  def num : Parser[String] = (decimalNumber)|(floatingPointNumber)
  def rangeVal : Parser[Any] = num ~ """[.]{2}""".r ~ num
  def rangeStmnt : Parser[Any] = "RANGE" ~":=" ~ "("~rangeVal~")"~semiCol
  def defuzMethodVal : Parser[Any] = ("CoG")|("CoGS")| ("CoA") | ("LM") | ("RM")
  def defuzMethodStmnt : Parser[Any] = "METHOD" ~":"~defuzMethodVal~semiCol
  def defaultVal : Parser[Any] = (num)| ("NC")
  def defaultStmnt : Parser[Any] = "DEFAULT" ~":="~defaultVal~semiCol
  def openDefuzzifyBlock : Parser[Any] = "DEFUZZIFY" ~> varName ^^ {case varName => {checkOutDecls(varName); varName}}
  case class dfb(name: String, defuzMethod: (Any)=>Double)
  def defuzzifyBlock : Parser[Any] = openDefuzzifyBlock~rangeStmnt~rep(termDecl)~defuzMethodStmnt~defaultStmnt~"END_DEFUZZIFY" ^^ {
    case openDefuzzifyBlock~rangeStmnt~termDclList~defuzMethodStmnt~defaultStmnt~"END_DEFUZZIFY" => openDefuzzifyBlock match{case varName => println("varName: " + varName)}
    }

  def funcBlock = "FUNCTION_BLOCK"~varName~varInput~varOutput~fuzzifyBlock~defuzzifyBlock~"END_FUNCTION_BLOCK"
}

