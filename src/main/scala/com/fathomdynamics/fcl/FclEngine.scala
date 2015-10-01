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


  def termPair : Parser[Tuple2[String, Any]] = ident~":="~rep(point) ^^ {case name~":="~list => { (name -> list)}}
  def memFuncDecl : Parser[Tuple2[String, Any]] = "TERM"~>termPair<~semiCol

  def nameValPair : Parser[Tuple2[String, Double]] = ident~":="~num ^^ { case name~":="~value => {(name -> value.toDouble)}}
  def singleton : Parser[Tuple2[String, Double]] = "TERM"~>nameValPair<~semiCol

  def openFuzzifyBlock : Parser[Any] = "FUZZIFY" ~> varName ^^ {case varName => {checkInDecls(varName); varName}}
  def fuzzifyBlock : Parser[Any] = openFuzzifyBlock~rep(memFuncDecl)~"END_FUZZIFY"

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

  //  val res = List((open,List(Point(0,1), Point(abc,0))), (close,List(Point(3,0), Point(27,one1))), (stable1,50), (stable,50), (close,List(Point(3,1), Point(27,one1))))
  case class dfb(name: String, range: List[Double], mixDecls: List[Tuple2[String, Any]], defuzMethod: String){
    val membershipFunctions = Map[String, List[Point]]()
    val singletonFunctions = Map[String, Double]()

    class Container[T](data: T) {
      def get = data
    }
    object Container {
      def unapply[T](x: Container[T]) = Some(x.get)
    }
    mixDecls.foreach(x =>{x._2 match{
      case List(_: Point, _*) => "it's a list of Points"
      //case membershipFuncPoints : List[Point] => {membershipFunctions += x._1 -> membershipFuncPoints; println("Regular membership function")}
      case singletonFuncVal : Double => {singletonFunctions += x._1->singletonFuncVal; println("Singleton")}
      case x @ Container(_: Point) => println(x)
    }})

    def foo[T](x: T)(implicit m: TypeTag[T]) = {
      println("m: " + m)
      if (m == typeTag[List[Point]])
        println("Hey, this list is full of strings")
      else
        println("Non-stringy list")
    }
/*
    def paramInfo2[T: TypeTag](x: T): Unit = {
      val targs = typeOf[T] match { case TypeRef(_, _, args) => args }
      println(s"type of $x has type arguments $targs")
    }
    def paramInfo[T](x: T)(implicit tag: TypeTag[T]): Unit = {
      val targs = tag.tpe match { case TypeRef(_, _, args) => args }
      println(s"type of $x has type arguments $targs")
    }
    def meth[A : TypeTag](xs: A) = typeOf[A] match {
      case t if t =:= typeOf[Any] => println("Any")
      case t if t <:< typeOf[Double] => println("Double")
    }

    mixDecls.foreach(x =>(implicit tiz : TypeTag[T]){x._2[T]  match {
      //case List(Point) => println("It's a list...")
     case membershipFuncPoints : TypeTag[Point] => {membershipFunctions += x._1 -> membershipFuncPoints; println("Regular membership function")}
     case singletonFuncVal : Double => {singletonFunctions += x._1->singletonFuncVal; println("Singleton")}
    }}*/
  //  )
  }

  def mixDecl : Parser[Tuple2[String, Any]] =  (singleton|memFuncDecl)
  def defuzzifyBlock : Parser[Any] = defuzzifyBlockId~rangeStmnt~rep(mixDecl)~defuzMethodStmnt~defaultStmnt~"END_DEFUZZIFY" ^^ {
    case defuzzifyBlockId~rangeStmnt~mixDecls~defuzMethodStmnt~defaultStmnt~"END_DEFUZZIFY" => { dfb(defuzzifyBlockId, rangeStmnt, mixDecls, defuzMethodStmnt)}
  }

  def funcBlock = "FUNCTION_BLOCK"~varName~varInput~varOutput~fuzzifyBlock~defuzzifyBlock~"END_FUNCTION_BLOCK"
}




