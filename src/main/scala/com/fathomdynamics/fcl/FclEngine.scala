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

  def ptValue: Parser[String] = (decimalNumber)|(floatingPointNumber) //^^ {case varName => {checkInDecls(varName); varName}}
  def varName: Parser[String] = ident

  def inputDecl : Parser[(String, String)] = varName~":"~varType ^^ { case name~":"~varType => {inDecls += name -> varType;(name, varType)}}
  def varInput : Parser[List[(String, String)]] = "VAR_INPUT"~>rep(inputDecl)<~"END_VAR"

  def outputDecl : Parser[(String, String)] = varName~":"~varType ^^ { case name~":"~varType => {outDecls += name -> varType; (name, varType)}}
  def varOutput : Parser[List[(String, String)]] = "VAR_OUTPUT"~>rep(outputDecl)<~"END_VAR"


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
  def entriesDbl : Parser[Point] = ptValue~","~ptValue ^^ {case pX~","~pY => { Point(pX.toDouble, pY.toDouble)}}
  def entriesLeftVar : Parser[Point] = varName~","~ptValue ^^ {case pX~","~pY => { Point(pX, pY.toDouble)}}
  def entriesRightVar : Parser[Point] = ptValue~","~varName ^^ {case pX~","~pY => { Point(pX.toDouble, pY)}}
  def point : Parser[Point] = "("~>(entriesDbl|entriesLeftVar|entriesRightVar)<~")"

  def termPair : Parser[Tuple2[String, List[Point]]] = ident~":="~rep(point) ^^ {case name~":="~list => { (name -> list)}}
  def memFuncDecl : Parser[Tuple2[String, List[Point]]] = "TERM"~>termPair<~semiCol

  def nameValPair : Parser[Tuple2[String, Double]] = ident~":="~num ^^ { case name~":="~value => {(name -> value.toDouble)}}
  def singleton : Parser[Tuple2[String, Double]] = "TERM"~>nameValPair<~semiCol

  case class FuzzifyBlock(inputName : String, memberFuncs : List[(String, List[Point])]){
    checkInDecls(inputName)

    val fuzzifierMap = memberFuncs.map(func =>(func._1 -> getFuzzifier(func._2))).toMap

    def getFuzzifier(funcPoints: List[Point]) = (inVal:Double) =>{
      // generate a list of points of all doubles
      val intervals = funcPoints.map(boundary=> {
        val List(xPos:Double, yPos:Double) = List(boundary.x, boundary.y).map(compo => {
          compo match {
            case numeric: Double => numeric
            case func: (() => Any) => func().asInstanceOf[Double]
          }});
          (xPos, yPos)
        }).sliding(2).toList

      val List(leftX:Double, leftY:Double, rightX:Double, rightY:Double) = List(intervals.head.head._1, intervals.head.head._2, intervals.last.last._1,intervals.last.last._2)

      if (inVal <= leftX) leftY
      else if (inVal >= rightX) rightY
      // only a single segment (tuple2, tuple2) should exist here is the membership function is properly defined
      else {
        val segment = intervals.filter (x=>x.head._1 <= inVal && x.last._1 > inVal).flatten

        segment.head._2 + ((segment.last._2 - segment.head._2)/(segment.last._1 - segment.head._1))*(inVal-segment.head._1)
      }
    }
  }
  def fuzzifyBlockDecl : Parser[FuzzifyBlock] = "FUZZIFY" ~ varName~rep(memFuncDecl)~"END_FUZZIFY" ^^ {
    case open~id~memFuncDecls~close =>  FuzzifyBlock(id, memFuncDecls)
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

  def defuzzifyBlockDecl : Parser[DefuzzifyBlock] = defuzzifyBlockId~rangeStmnt~rep(mixDecl)~defuzMethodStmnt~defaultStmnt~"END_DEFUZZIFY" ^^ {
    case defuzzifyBlockId~rangeStmnt~mixDecls~defuzMethodStmnt~defaultStmnt~"END_DEFUZZIFY" => { DefuzzifyBlock(defuzzifyBlockId, rangeStmnt, mixDecls, defuzMethodStmnt)}
  }

  /*
  RULEBLOCK ruleblock_name
    operator_definition; operator: algorithm
        ---------- OR -------------------- PAIRING --------------------- AND ---------------
        MAX  -  Max (m1(x), m2(x))￼------------------>  MIN   - Min(m1(x), m2(x))
        ------------------------------------------------------------------------------------
        ASUM -  m1(x)+m2(x) – m1(x) m2(x) ----------->  PROD  - m1(x) m2(x)
        ------------------------------------------------------------------------------------
        BSUM -  Min(1, m1(x) + m2(x)) --------------->  BDIF  - Max (0, m1(x) + m2(x) -1)
        ------------------------------------------------------------------------------------
    [activation_method;] ACT: activation_method;
        PROD - m1(x) m2(x)
        MIN  - Min(m1(x), m2 (x))
    accumulation_method; ACCU: accumulation_method;
        Maximum         - MAX   - MAX (m1(x), m2(x))
        Bounded sum     - BSUM  - MIN (1, m1(x) + m2(x))
        Normalized sum  - NSUM  - m1(x) + m2(x)
                                  -------------
                             MAX (1, MAXx’ÎX (m1(x’) + m2(x’)))
    rules;
  END_RULEBLOCK
   */

  def orAlg : Parser[String] = ("OR"~":")~>("MAX"|"ASUM"|"BSUM")
  def andAlg : Parser[String] = ("AND"~":")~>("MIN"|"PROD"|"BDIF")

  def opDef : Parser[String] = (orAlg | andAlg)<~semiCol

  def actMeth : Parser[String] = ("ACT" ~ ":") ~> ("PROD"|"MIN")<~semiCol
  def accuMeth : Parser[String] = ("ACCU" ~ ":") ~> ("MAX"|"BSUM"|"NSUM")<~semiCol

  case class Clause (inputVar:String, fuzzyVar:String)
  def clauseExpr : Parser[Clause] = varName ~"IS"~ varName ^^ {case left~"IS"~right => Clause(left, right) }
// RULE 1: IF temp IS cold AND pressure IS low THEN valve IS inlet;
  case class Rule(name:Any, antecedent:Any, consequent:Any, weight:Any)
  def ruleDecl : Parser[Rule] = "RULE"~num~ ":"~"IF"~ condition~ "THEN"~ conclusion~opt("WITH"~weightFactor)~semiCol ^^ {
    case "RULE"~num~ ":"~"IF"~ condition~ "THEN"~ conclusion~weight~semiCol => Rule(num,condition, conclusion, weight)}

  def condition : Parser[Any] = x~rep(("AND"~x)|( "OR"~ x)) ^^ {case x~rest => println("Condition(x, rest): " + x + ", " + rest)}
  def x : Parser[Any] = opt("NOT")~ (subcondition | ("("~ condition ~")" ))
  def subcondition : Parser[Any] = (varName~ "IS" ~opt("NOT")~ varName)|varName
  def conclusion : Parser[Any] = rep((varName | clauseExpr) ~ ",")~(clauseExpr|varName)
  def weightFactor : Parser[String] = varName | num

  case class RuleBlock(name: String, opDef: String, actMeth:Option[String], accuMeth: String, rules:List[Rule])
  def ruleBlockDecl : Parser[Any] = "RULEBLOCK" ~ varName~opDef~opt(actMeth)~accuMeth~rep(ruleDecl)~"END_RULEBLOCK" ^^ {
    case open~id~opDef~actMeth~accuMeth~rules~close => RuleBlock(id, opDef,actMeth,accuMeth, rules)
  }

  case class FuncBlockDef(name:String, inputBlock:List[(String, String)],
                       outputBlock:List[(String, String)],
                        fuzzifyBlock:List[FuzzifyBlock], defuzzifyBlock: List[DefuzzifyBlock])
  var funcBlockDefs = Map[String, FuncBlockDef]()
  def funcBlock = "FUNCTION_BLOCK"~varName~varInput~varOutput~rep(fuzzifyBlockDecl)~rep(defuzzifyBlockDecl)~
    rep(ruleBlockDecl)~"END_FUNCTION_BLOCK" ^^ {
    case beg~varName~inBlk~outBlk~fuzzifyBlks~defuzzBlks~ruleBlockDecls~end => funcBlockDefs += (varName -> FuncBlockDef(varName,
                                                                                            inBlk, outBlk, fuzzifyBlks, defuzzBlks))
  }

}




