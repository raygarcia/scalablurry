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
trait Validators{
  val vars = Set[String]()
  val inDecls = Map[String, String]()
  val outDecls = Map[String, String]()
  val localDecls = Map[String, String]()
  var errors = ListBuffer[String]()

  // make sure all vars used in points are declared
  def checkInDecls(varName: String) = {if (!inDecls.contains(varName)) {errors += ("var name \"" + varName + "\" doesn't exist as an input variable.");}}
  def checkOutDecls(varName: String) = {if (!outDecls.contains(varName)) {errors += ("var name \"" + varName + "\" doesn't exist as an output variable.");}}

  // range-related validation
  def checkAgainstRange(range: Tuple2[Double, Double], value: Double) = {if (value < range._1 || value > range._2) errors += (value + " Not in range of " + range.toString()) }
  def checkRange (range: Tuple2[Double, Double]) =  {if (range._1 >= range._2) {errors += (range._1 + "not less than" + range._2 + "...Range format should be (LOW, HIGH)")}}

  def dumpSemanticResults = if (errors.size > 0) {println(errors.size + " errors found.");errors.foreach(x => println("error: " + x)) }
}
case class Point(x:Any, y: Any){

}
class FclEngine extends JavaTokenParsers with Validators{
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

  val membershipFunctions = Map[String, List[Point]]()
  def termPair : Parser[Any] = ident~":="~rep(point) ^^ { case name~":="~pointList => {membershipFunctions += name -> pointList; (name -> pointList)}}
  def termDecl : Parser[Any] = "TERM"~>termPair<~semiCol
  
  def openFuzzifyBlock : Parser[Any] = "FUZZIFY" ~> varName ^^ {case varName => {checkInDecls(varName); varName}}
  def fuzzifyBlock : Parser[Any] = openFuzzifyBlock~rep(termDecl)~"END_FUZZIFY"

  val singletonFunctions = Map[String, Double]()
  def varNameValPair : Parser[Any] = varName ~ ":=" ~ num ^^ { case name~":="~value => {singletonFunctions += name -> value.toDouble; (name -> value)}}
  def singleton : Parser[Any] = "TERM"~>varNameValPair<~semiCol

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
  def rangeVal : Parser[List[Double]] = num ~ """[.]{2}""".r ~ num ^^ {case low~delim~high => {List(low.toDouble,high.toDouble)}}
  def rangeStmnt : Parser[List[Double]] = "RANGE" ~":=" ~ "("~>rangeVal<~")"~semiCol
  def defuzMethodVal : Parser[String] = ("CoG")|("CoGS")| ("CoA") | ("LM") | ("RM")
  def defuzMethodStmnt : Parser[String] = "METHOD" ~":"~>defuzMethodVal<~semiCol
  def defaultVal : Parser[String] = (num)| ("NC")
  def defaultStmnt : Parser[Any] = "DEFAULT" ~":="~>defaultVal<~semiCol
  def defuzzifyBlockId : Parser[String] = "DEFUZZIFY" ~> varName ^^ {case varName => {checkOutDecls(varName); varName}}
  case class dfb(name: String, range: List[Double], defuzMethod: String)
  def defuzzifyBlock : Parser[Any] = defuzzifyBlockId~rangeStmnt~rep(termDecl|singleton)~defuzMethodStmnt~defaultStmnt~"END_DEFUZZIFY" ^^ {
     case defuzzifyBlockId~rangeStmnt~termDclList~defuzMethodStmnt~defaultStmnt~"END_DEFUZZIFY" => dfb(defuzzifyBlockId, rangeStmnt, defuzMethodStmnt)
    }

  def funcBlock = "FUNCTION_BLOCK"~varName~varInput~varOutput~fuzzifyBlock~defuzzifyBlock~"END_FUNCTION_BLOCK"
}

