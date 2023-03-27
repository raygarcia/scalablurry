package com.fathomdynamics.fcl.engine

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.io.Source
import scala.util.{Try,Success,Failure}
import com.fathomdynamics.fcl.fuzzification.Fuzzification
import com.fathomdynamics.fcl.defuzzification.Defuzzification
import com.fathomdynamics.fcl.ruleBase.RuleBase
import com.fathomdynamics.fcl.util.{Validators, Utils}
//import scala.collection.mutable._

/**
  * Created by Raymond Garcia, Ph.D. (ray@fathomdynamics.com) on 10/10/2015.
 The MIT License (MIT)

Copyright (c) 2015-2023 Raymond Garcia

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
  */
trait FunctionBlockElements extends Validators with Fuzzification with Defuzzification with RuleBase with Utils {
  override val logger = Logger(LoggerFactory.getLogger("FunctionBlockElements"))

  case class FuncBlockDef(name: String, inputBlock: List[Tuple3[String, String, Option[String]]],
                          outputBlock: List[(String, String)],
                          varDeclBlock:Option[List[Tuple3[String, String, Option[String]]]],
                          fuzzifyBlock: List[FuzzifyBlockCore], defuzzifyBlock: List[DefuzzifyBlockCore],
                          ruleBlock: List[RuleBlock]) {
    functionBlock =>
    implicit val fb = functionBlock
    logger.debug("defuzzifyBlock: " + defuzzifyBlock)
    val fuzzyBlocks = fuzzifyBlock.map(f => (f.inputName -> FuzzifyBlock(f,fb))).toMap
    val defuzzyBlocks = defuzzifyBlock.map(d => (d.outputName -> DefuzzifyBlock(d,fb))).toMap
    val ruleBlocks = ruleBlock.map(r => (r.name -> r)).toMap
    val inputs = scala.collection.mutable.Map[String,Double]()

    // Const var storage: Tuple3(Name, Type, Value)
    // handling the const var blocks,
    // 1. look up the data type
    // 2. convert the 3rd part of the tuple to the datatype
    // Since these are constants, it is a one-time immutable use
    // here are the Scala types mapped to the FCL Types:
    /*
    Byte	8 bit signed value. Range from -128 to 127
    Short	16 bit signed value. Range -32768 to 32767
    Int	32 bit signed value. Range -2147483648 to 2147483647
    Long	64 bit signed value. -9223372036854775808 to 9223372036854775807
    Float	32 bit IEEE 754 single-precision float
    Double	64 bit IEEE 754 double-precision float
    Char	16 bit unsigned Unicode character. Range from U+0000 to U+FFFF
    String	A sequence of Chars
    BigInt  -- ULINT
     */
    def setupVars(lst:List[Tuple3[String, String, Option[String]]]):Map[String, Any] = {
      lst.map{ c => c._2 match {
        case "REAL" => c._1 -> c._3.fold(0.0)(_.toFloat)
        case "LREAL"  => c._1 -> c._3.fold(0.0)(_.toDouble)
        case "SINT"  => c._1 -> c._3.fold(0)(_.toByte) //8 bit
        case "INT" => c._1 -> c._3.fold(0)(_.toShort) // 16 bit
        case "DINT"  => c._1 -> c._3.fold(0)(_.toInt) // 32 bit
        case "LINT"  => c._1 -> c._3.fold(0L)(_.toLong) // 64 bit
        // upconvert for unsigned values
        case "USINT"  => c._1 -> c._3.fold(0)(_.toShort)
        case "UINT" => c._1 -> c._3.fold(0)(_.toInt)
        case "UDINT"  => c._1 -> c._3.fold(0L)(_.toLong)
        // upconverted to big-int
        case "ULINT"  => {val signedLong:Long = c._3.fold(0L)(_.toLong);
          c._1 -> (( BigInt(signedLong >>> 1) << 1) + (signedLong & 1)) }

        case "BOOL" => {val b: Boolean = c._3.fold[Short](0)(_.toShort); c._1 -> b }
        case "STRING" => c._1 -> c._3.getOrElse("")
        case "BYTE"  => c._1 -> c._3.fold(0)(_.toShort)
        case _ => println("nothing found for" + c); ""->""

        //         "WORD"|"DWORD"|"LWORD" | "TIME" | "DATE" | "TIME_OF_DAY" | "DATE_AND_TIME"| "WSTRING"
      }}.toMap
    }
    // The variable blocks (input, output, and var) are not scope-defined
    // but instead are teyp-defined by the block they belong to:
    //  * VAR_INPUT - Externally exposed and stores input
    //  * VAR_OUTPUT - Externally exposed and stores function output values
    //  * VAR - LOCAL variables
    // Therefore, names need to be unique and will search local variables
    // then input variables

    lazy val localVars:Map[String, Any] = varDeclBlock.fold(Map[String, Any]())(setupVars(_))


    def plot = {
      fuzzyBlocks.foreach(f => f._2.plot)
      defuzzyBlocks.foreach(f => f._2.plot)
    }

    def eval(inputVals: List[Double]): Map[String, Map[String, Double]] = {
      // error if var count doesn't equal inputVals
      for (i<-0 until inputVals.length){
        inputs += inputBlock(i)._1 -> inputVals(i)
      }

      // Map[RuleBlockName, Map[OutputName,Aggregate Function]
      val aggregationOut: Map[String, Map[String, (Double) => Double]] =
        ruleBlocks.map(rb => (rb._1 -> rb._2.eval))

      // since every rule block has its own ANDO/OR, activation,
      // and accumulation setting, the aggregation output is
      // based on each rule block name.
      // Map[RuleBlockName, Map[OutputName, OutputValue]]
      val out = aggregationOut.map(ao => {
        ao._1 -> ao._2.map { case (k, v) => k -> defuzzyBlocks(k).defuzzify(v) }
      })
      logger.debug("out: " + out.toString)
      out
    }
  }
  val funcBlockDefs = scala.collection.mutable.Map[String, FuncBlockDef]()
}

