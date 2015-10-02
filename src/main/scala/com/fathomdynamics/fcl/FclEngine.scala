package com.fathomdynamics.fcl

import scala.collection.mutable._
import scala.reflect.ClassTag
import scala.reflect.api.TypeTags
import scala.language.{implicitConversions, existentials}
import scala.runtime.ScalaRunTime.{ arrayClass, arrayElementClass }

import scala.util.parsing.combinator.JavaTokenParsers
import scala.reflect.runtime.universe._

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

  case class Point(xPos:Any, yPos: Any){
    val x = xPos match {
      case xVal:Double => xVal
      case inputVar:String => ()=>{inputStrm.get(inputVar)}
    }
    val y = yPos match {
      case yVal:Double => yVal
      case inputVar:String => ()=>{inputStrm.get(inputVar)}
    }
  }

  val inputStrm = Map[String,List[Any]]()
  def entries : Parser[Point] = ptValue~","~ptValue ^^ {case pX~","~pY => { Point(pX, pY)}}
  def point : Parser[Point] = "("~>entries<~")"


  def termPair : Parser[Tuple2[String, List[Point]]] = ident~":="~rep(point) ^^ {case name~":="~list => { (name -> list)}}
  def memFuncDecl : Parser[Tuple2[String, List[Point]]] = "TERM"~>termPair<~semiCol

  def nameValPair : Parser[Tuple2[String, Double]] = ident~":="~num ^^ { case name~":="~value => {(name -> value.toDouble)}}
  def singleton : Parser[Tuple2[String, Double]] = "TERM"~>nameValPair<~semiCol

 // def openFuzzifyBlock : Parser[Any] = "FUZZIFY" ~> varName ^^ {case varName => {checkInDecls(varName); varName}}

  case class FuzzifyBlock(inputName : String, memberFuncs : List[(String, List[Point])]){
    checkInDecls(inputName)

    memberFuncs.map(x=>getFuzzifier(x._1,x._2))

    def getFuzzifier(funcName: String, funcPoints: List[Point]) = (inVal:Double) =>{

      //y = mx + b
      val List(leftX, leftY, rightX, rightY) = List(funcPoints.head.xPos, funcPoints.head.yPos, funcPoints.last.xPos,funcPoints.head.yPos).map(x=> {
        x match {
        case numeric: Double => numeric
        case func: (() => Any) => func().asInstanceOf[Double]
      }})

      if (inVal < leftX) leftY
      else if (inVal > rightX) rightY

    }

  }
  def fuzzifyBlockDecls : Parser[Any] = "FUZZIFY" ~ varName~rep(memFuncDecl)~"END_FUZZIFY" ^^ {
    case open~id~memFuncDecls~close => FuzzifyBlock(id, memFuncDecls)
  }

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


  case class DefuzzifyBlock(name: String, range: List[Double], mixDecls: List[Tuple2[String, Any]], defuzMethod: String){

    val membershipFunctions = Map[String, List[Point]]()
    val singletonFunctions = Map[String, Double]()

    mixDecls.foreach(x =>{ x._2 match{
      //this will either be a singleton Tuple2[String, Double] or membership func Tuple2[String, List[Point]]
      // adapting to type erasure with an explicit downcast since there are only two cases

      case singletonFuncVal : Double => {singletonFunctions += x._1 -> singletonFuncVal; println("Singleton")}

      case membershipFuncPoints : Any  => {membershipFunctions += x._1 -> membershipFuncPoints.asInstanceOf[List[Point]]; println("Regular membership function")}
    }})
  }

  def mixDecl : Parser[Tuple2[String, Any]] =  (singleton|memFuncDecl)
  def defuzzifyBlockDecls : Parser[Any] = defuzzifyBlockId~rangeStmnt~rep(mixDecl)~defuzMethodStmnt~defaultStmnt~"END_DEFUZZIFY" ^^ {
    case defuzzifyBlockId~rangeStmnt~mixDecls~defuzMethodStmnt~defaultStmnt~"END_DEFUZZIFY" => { DefuzzifyBlock(defuzzifyBlockId, rangeStmnt, mixDecls, defuzMethodStmnt)}
  }

  def funcBlock = "FUNCTION_BLOCK"~varName~varInput~varOutput~fuzzifyBlockDecls~defuzzifyBlockDecls~"END_FUNCTION_BLOCK"
}




