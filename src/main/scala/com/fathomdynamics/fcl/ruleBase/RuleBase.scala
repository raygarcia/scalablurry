package com.fathomdynamics.fcl.ruleBase

import com.fathomdynamics.fcl.engine.FunctionBlockElements
import com.fathomdynamics.fcl.util.{Utils, Validators}

/**
 * Created by Raymond Garcia, Ph.D. (ray@fathomdynamics.com) on 10/10/15.
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

trait RuleBase extends Validators with Utils{
  trait ClauseFeature{
    def operator:Option[String]
    def inputVar:Option[String]
    def notOpt:Option[String]
    def fuzzyVar:Option[String]
    def clauses:Option[List[Clause]]
    def innerParens:Boolean = false
  }
  case class SimpleClause (operator:Option[String],inputVar:Option[String], notOpt:Option[String],
                     fuzzyVar:Option[String], clauses:Option[List[Clause]], override val innerParens:Boolean = false)
    extends ClauseFeature

  case class Clause (operator:Option[String],inputVar:Option[String], notOpt:Option[String],
                     fuzzyVar:Option[String], clauses:Option[List[Clause]], override val innerParens:Boolean = false)
    extends ClauseFeature{

    operator.fold()(println)
    def consequentEval(antecedent: Clause)(implicit fbd : FunctionBlockElements#FuncBlockDef, rb : RuleBlock)={
      antecedent.antecedentEval()
    }
    /* IF temp IS cold AND pressure IS low THEN valve IS inlet;
          ----- Produces ----
 Clause(None,None,None,None,Some(List(Clause(None,None,None,None,
  Some(List(Clause(None,Some(temp),None,Some(cold),None,false))),false), Clause(Some(AND),None,None,None,
  Some(List(Clause(None,None,None,None,Some(List(Clause(None,Some(pressure),None,Some(low),None,false))),false))),false))),false)
  */

    def antecedentEval()(implicit fbd : FunctionBlockElements#FuncBlockDef, rb : RuleBlock):Double = {
      // get algorithm


      // no operator + no list of clauses = input IS fuzzyName
      clauses.fold[Double] {
        fbd.fuzzyBlocks(inputVar.get).fuzzifierMap(fuzzyVar.get)(1.0) // TODO:get the input
      }(innerClauses => innerClauses.foldLeft(0.0)((r,c:Clause)=>r + c.antecedentEval()))
    }
  }

  case class Rule(name:String, antecedent:Clause, consequent:Clause, weight:Option[Any]){
    println("RULE " + name + ":" + " IF " + antecedent + " THEN " + consequent + " " + weight)
    val w:Double = weight.fold(1.0)(_ match {
      case num: Double => num
      case varRef: String => 1.0 // TODO: get the variable
    })

    def eval()(implicit fbd : FunctionBlockElements#FuncBlockDef, rb : RuleBlock)={
      consequent.consequentEval(antecedent)
    }
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
  case class RuleBlock(name: String, opDef: String, actMeth:Option[String], accuMeth: String, rules:List[Rule]){
    owner =>

    // Use accumulation method
    implicit val rb:RuleBlock = owner //
    def eval()(implicit fbd : FunctionBlockElements#FuncBlockDef) = {
      rules.foreach(r =>r.eval())
    }
  }

}
