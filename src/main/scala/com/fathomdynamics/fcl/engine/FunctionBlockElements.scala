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

Copyright (c) 2015 Raymond Garcia

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

  case class FuncBlockDef(name: String, inputBlock: List[(String, String)],
                          outputBlock: List[(String, String)],
                          fuzzifyBlock: List[FuzzifyBlock], defuzzifyBlock: List[DefuzzifyBlock],
                          ruleBlock: List[RuleBlock]) {
    functionBlock =>
    implicit val fb = functionBlock
   // println(defuzzifyBlock)
    val fuzzyBlocks = fuzzifyBlock.map(f => (f.inputName -> f)).toMap
    val defuzzyBlocks = defuzzifyBlock.map(d => (d.outputName -> d)).toMap
    val ruleBlocks = ruleBlock.map(r => (r.name -> r)).toMap
    val inputs = scala.collection.mutable.Map[String,Double]()

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
      //logger.debug("out: " + out.toString)
      out
    }
  }
  val funcBlockDefs = scala.collection.mutable.Map[String, FuncBlockDef]()
}

