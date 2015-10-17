package com.fathomdynamics.fcl.ruleBase

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

  case class Clause (operator:Option[String],inputVar:Option[String], notOpt:Option[String],
                     fuzzyVar:Option[String], clauses:Option[List[Clause]], priority:Boolean = false) {

    def eval(antecedent: Clause):Double ={

      1.0
    }

  }

  case class Rule(name:String, antecedent:Clause, consequent:Clause, weight:Option[Any]){
    println("RULE " + name + ":" + " IF " + antecedent + " THEN " + consequent + " " + weight)
    val w:Double = weight.fold(1.0)(_ match {
      case num: Double => num
      case varRef: String => 1.0
    })

    def eval={consequent.eval(antecedent)*w}
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

    // Use accumulation method
    val sum = rules.foldLeft(0.0)(_ + _.eval)
  }

}
