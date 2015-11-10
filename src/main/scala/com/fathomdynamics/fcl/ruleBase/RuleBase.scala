package com.fathomdynamics.fcl.ruleBase


import com.fathomdynamics.fcl.defuzzification.Defuzzification
import com.fathomdynamics.fcl.engine.FunctionBlockElements
import com.fathomdynamics.fcl.util.{Utils, Validators}
import com.typesafe.scalalogging._
import org.slf4j.LoggerFactory
import scala.collection.mutable.ListBuffer
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
  val logger = Logger(LoggerFactory.getLogger("RuleBase"))

  case class Expr(left:Either[String,Expr], op:String, right:Either[String,Expr]){
    def eval()(implicit fbd : FunctionBlockElements#FuncBlockDef, rb : RuleBlock): Double = {
     // logger.debug("fbd: " + fbd)

      logger.debug("OP:" + op)
      op match {
        case "IS" => val inputName = left.left.get; logger.debug("mem Deg:" + fbd.fuzzyBlocks(inputName).
          fuzzifierMap(right.left.get)(fbd.inputs(inputName)));logger.debug(inputName + ": " + fbd.inputs(inputName));
          fbd.fuzzyBlocks(inputName).fuzzifierMap(right.left.get)(fbd.inputs(inputName))
        case "OR"|"AND" => rb.activate(List(left.right.get.eval, right.right.get.eval),op)
      }
    }
  }

  trait ClauseFeature{
    def operator:Option[String]
    def inputVar:Option[String]
    def notOpt:Option[String]
    def fuzzyVar:Option[String]
    def clauses:Option[ListBuffer[Clause]]
    def innerParens:Boolean = false
  }
  case class SimpleClause (operator:Option[String],inputVar:Option[String], notOpt:Option[String],
                           fuzzyVar:Option[String], clauses:Option[ListBuffer[Clause]], override val innerParens:Boolean = false)
    extends ClauseFeature

  case class Clause (operator:Option[String],inputVar:Option[String], notOpt:Option[String],
                     fuzzyVar:Option[String], clauses:Option[ListBuffer[Clause]],
                     override val innerParens:Boolean = false)
    extends ClauseFeature{
    self=>

    lazy val expr:Either[String,Expr] = {
      //No list?  It's a simple expr
      inputVar.fold[Either[String,Expr]](clauses2Expr(clauses.get))(iVar =>{
      val basicExpr = Right(Expr(Left(inputVar.get),"IS",Left(fuzzyVar.get)))
      clauses.fold(basicExpr)(clist=>
      if (clist.size > 0){Right(Expr(Right(Expr(Left(inputVar.get),"IS",Left(fuzzyVar.get))),
        clauses.get.head.operator.get, clauses2Expr(clauses.get)))}
        else {
        basicExpr
        }
         )})
    }
    def clauses2Expr(cList:ListBuffer[Clause]):Either[String,Expr] = {
      if (cList.size == 0) {
        Right(Expr(Left(inputVar.get), "is", Left(fuzzyVar.get)))
      }else if (cList.size == 1) {
        cList.head.expr;
      } else if (cList.size == 2) {
        logger.debug("cList(0): " + cList(0).expr)
        logger.debug("cList.head.expr: " + cList.head.expr)
        Right(Expr(cList.head.expr, cList.last.operator.get, cList.last.expr))
      } else {
        // 3 or more
        // pair up based on operator
        logger.debug("Pre-paired Len:" + cList.length)
        val pList = pairUp(cList)
        logger.debug("Post-paired Len: " + pList.length)

        // now that all the AND operations are paired up ,
        // let's nest them

        val toBeDel = ListBuffer[Clause]()
        for (i <- 1 until (pList.length - 1)) {
          logger.debug("removing...")
          pList.head.clauses match {
            case Some(_) => {
              pList.head.clauses.get += pList(i)
              toBeDel += pList(i)
            }
            case None =>
          }
        }
        logger.debug("ToBeDeleted: " + toBeDel)
        toBeDel.foreach(i => pList -= i)

        logger.debug("Post-nested size: " + pList.length)
        logger.debug("Post-nested head size: " + pList.head.clauses.get.length)
        clauses.get.clear()
        clauses.get ++= pList
        logger.debug("cList: " + cList)
        logger.debug("clause size: " + cList.size)
        expr
      }
    }
    def pairUp(list: ListBuffer[Clause]):ListBuffer[Clause] = {
      val i = list.toIterator
      var k = 0;
      while(i.hasNext){
        i.next.clauses match{
          case Some(_) => {
            if (i.hasNext){
              list(k+1).operator match{
                case Some("AND") => {
                  list(k).clauses.get += list(k+1)
                  if (i.hasNext){
                    i.next()
                    k += 1
                  }
                }
                case None|Some(_) =>
              }}
          }
          case None => logger.debug("empty clauses...")
        }
        k += 1
      }

      val lf = list.filter(i => i.operator != Some("AND"))
      logger.debug("filtered: " + lf)
      lf
    }

    /*
         THEN valve IS inlet
          ----- Produces ----
  Clause(None,None,None,None,Some(List(Clause(Some(Imp),Some(valve),None,Some(inlet),None,false))),false)

  Clause(None,None,None,None,
  Some(List(Clause(Some(Imp),Some(comAgain),None,Some(heckNo),None,false), Clause(Some(Imp),Some(tip),None,Some(cheap),None,false))),false)
     */

    def consequentEval(antecedentResult: Double)(implicit fbd : FunctionBlockElements#FuncBlockDef, rb : RuleBlock):
    List[(String, (Double)=>Double)]={
      def resultOfImplication(memFunc:(Double)=>Double):((Double)=>Double) = {
        rb.actMeth.get match {
          case "PROD" => (inVal:Double) => {memFunc(inVal)*antecedentResult}
          case "MIN" => (inVal:Double) => {
            val rVal = memFunc(inVal)
            if (rVal > antecedentResult) antecedentResult else rVal
          }
        }
      }
      clauses.fold[List[(String, (Double)=>Double)]]{logger.debug("no clauses");List((""->((x:Double)=>{0.0})))}(
        consequents => consequents.map(consequent => {
          val varName = consequent.inputVar.get
          logger.debug("varName: " + varName +
            ", consequent.fuzzyVar.get: " + consequent.fuzzyVar.get)
          val memFunc = fbd.defuzzyBlocks(varName).membershipFunctions(consequent.fuzzyVar.get)
          varName -> resultOfImplication(memFunc)
        }
        ).toList
      )
    }
  }

  case class Rule(name:String, antecedent:Clause, consequent:Clause, weight:Option[Any]){
    logger.debug("RULE " + name + ":" + " IF " + antecedent + " THEN " + consequent + " " + weight)
    logger.debug("antecedent clauses: " + antecedent.clauses.get.size)

    val exprLst:Either[String, Expr] = antecedent.expr
 //   logger.debug(exprLst.toString)

    val w:Double = weight.fold(1.0)(_ match {
      case num: Double => num
      case varRef: String => 1.0 // TODO: get the variable
    })

    def eval()(implicit fbd : FunctionBlockElements#FuncBlockDef, rb : RuleBlock):
    List[(String, (Double)=>Double)]={
      val degOfSupport = exprLst.right.get.eval()
      logger.info("Rule: " + name + ", Degree of Support: " + degOfSupport)
      val rMf = consequent.consequentEval(degOfSupport)
      rMf.foreach(result => {println(result._1 + ": "); simplePlot(result._1,List(0,30),result._2)})

      rMf
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
  def bdifMin = (sides:List[Double], op:String) =>{
    op match {
      case "AND" => List[Double](0, sides.sum -1).max
      case "OR" => List[Double](1, sides.sum).min
    }
  }

  def minMax = (sides:List[Double], op:String) =>{
    op match {
      case "AND" => sides.min
      case "OR" => sides.max
    }
  }

  def prodAsum = (sides:List[Double], op:String) => {
    op match {
      case "AND" => sides.product
      case "OR" => sides.sum - sides.product
    }
  }

  case class RuleBlock(name: String, opDef: String, actMeth:Option[String], accuMeth: String, rules:List[Rule]){
    thisRuleBlock =>
    // get algorithm and generate an appropriate activation function
    def activate:(List[Double], String)=>Double = {
      opDef match {
        case "BDIF" | "BSUM" => bdifMin
        case "MIN" | "MAX" => minMax
        case "PROD" | "ASUM" => prodAsum
      }
    }
    /*
        accumulation_method; ACCU: accumulation_method;
        Maximum         - MAX   - MAX (m1(x), m2(x))
        Bounded sum     - BSUM  - MIN (1, m1(x) + m2(x))
        Normalized sum  - NSUM  - m1(x) + m2(x)
                                  -------------
                        MAX (1, MAXx’ÎX (m1(x’) + m2(x’)))
        */

    def maxAccu(funcList:List[(Double)=>Double]) =
      (x:Double)=> {
        val o = funcList.map{func => func(x)}.max
  //      val o = funcList.map{func => println("func(x): " + func(x));func(x)}.max
 //       println("maxAccu: " + o)

        o
      }

    def boundedSum(fList:List[(Double)=>Double]) =
      (x:Double)=> {
        math.min(1,fList.map{func => func(x)}.sum)
      }

    def normalizedSum(fList:List[(Double)=>Double]) =
      (x:Double)=> {
        1.0
        //        math.max(1,math.max(fList.map{func => func(x)}.sum))
      }

    def accumulate(fList:List[(Double)=>Double]):
    (Double)=>Double = {
      accuMeth match{
        case "MAX" => maxAccu(fList)
        case "BSUM" => boundedSum(fList)
        case "NSUM" => normalizedSum(fList)
      }
    }

    // Using the accumulation method
    // the resultmap goes from
    // Map[String, List[(Double)=>Double]]
    // to Map[String, (Double)=>Double]
    implicit val rb:RuleBlock = thisRuleBlock //
    def eval()(implicit fbd : FunctionBlockElements#FuncBlockDef):
    Map[String, (Double)=>Double] = {
      val resultMap = rules.map(r =>r.eval()).flatten.groupBy(_._1).map {
        case (k,v) => (k,v.map(_._2))
      }
      resultMap.map{case(k,v) => (k, accumulate(v))}
    }
  }

}
