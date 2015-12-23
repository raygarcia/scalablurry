
package com.fathomdynamics.fcl.ruleBase


import com.fathomdynamics.fcl.FclParser
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
      logger.debug("OP:" + op)
      op match {
        case "IS" => fbd.fuzzyBlocks(left.left.get).fuzzifierMap(right.left.get)(fbd.inputs(left.left.get))
        case "NOT" => 1-fbd.fuzzyBlocks(left.left.get).fuzzifierMap(right.left.get)(fbd.inputs(left.left.get))
        case "OR"|"AND" => rb.activate(List(left.right.get.eval, right.right.get.eval),op)
      }
    }
  }


  case class Clause (var operator:Option[String], inOrOutVar:Option[String], var notOpt:Option[String],
                     fuzzyVar:Option[String], clauses:Option[ListBuffer[Clause]],
                     innerParens:Boolean = false, weight:Option[Any]= None) {
    self=>
    operator match{
      case Some("NOT") => invert()
      case None | Some(_) =>
    }
    def invert():Unit = {
      operator = operator match{
        case Some("AND") => Some("OR")
        case Some("OR") => Some("AND")
        case Some("NOT") => None
        case None | Some(_) => operator
      }
      notOpt = notOpt match{
        case None => Some("NOT")
        case Some(_) => None
      }
      clauses.fold[Unit]()(cList => cList.foreach(_.invert()))
    }
    lazy val expr:Either[String,Expr] = {
      //      logger.debug("Expr clauses: " + clauses)
      //No list?  It's a simple expr
      inOrOutVar.fold[Either[String,Expr]](clauses2Expr(
        clauses.getOrElse(ListBuffer())))(iVar =>{
        val basicExpr = Right(Expr(Left(inOrOutVar.get),
          notOpt.getOrElse("IS"),Left(fuzzyVar.get)))

        clauses.fold(basicExpr)(clist=>
          if (clist.nonEmpty){
            Right(Expr(Right(Expr(Left(inOrOutVar.get),notOpt.getOrElse("IS"),
              Left(fuzzyVar.get))),
              clauses.get.head.operator.get, clauses2Expr(clauses.get)))}
          else {
            basicExpr
          }
        )})
    }
    def clauses2Expr(cList:ListBuffer[Clause]):Either[String,Expr] = {
      if (cList.size == 1) {
        cList.head.expr
      } else if (cList.size == 2) {
        //logger.debug("cList.head.expr: " + cList.head.expr)
        Right(Expr(cList.head.expr, cList.last.operator.get, cList.last.expr))
      } else {
        // 3 or more
        // pair up based on operator
        //       logger.debug("Pre-paired Len:" + cList.length)
        val pList = pairUp(cList)
        //       logger.debug("Post-paired Len: " + pList.length)

        // now that all the AND operations are paired up ,
        // let's nest them

        val toBeDel = ListBuffer[Clause]()
        for (i <- 1 until (pList.length - 1)) {
          //          logger.debug("removing...")
          pList.head.clauses match {
            case Some(_) => pList.head.clauses.get += pList(i)
              toBeDel += pList(i)

            case None =>
          }
        }
        logger.debug("ToBeDeleted: " + toBeDel)
        toBeDel.foreach(i => pList -= i)

        logger.debug("Post-nested size: " + pList.length)
        logger.debug("Post-nested head size: " + pList.head.clauses.get.length)
        clauses.get.clear()
        clauses.get ++= pList
        // logger.debug("cList: " + cList)
        logger.debug("clause size: " + cList.size)
        expr
      }
    }
    def pairUp(list: ListBuffer[Clause]):ListBuffer[Clause] = {
      val i = list.toIterator
      var k = 0
      while(i.hasNext){
        i.next.clauses match{
          case Some(_) => if (i.hasNext){
            list(k+1).operator match{
              case Some("AND") => list(k).clauses.get += list(k+1)
                if (i.hasNext){
                  i.next()
                  k += 1
                }

              case None|Some(_) =>
            }}

          case None => logger.debug("empty clauses...")
        }
        k += 1
      }

      val lf = list.filter(i => !i.operator.contains("AND"))
      logger.debug("filtered: " + lf)
      lf
    }

    def consequentEval(antecedentResult: Double)(implicit fbd : FunctionBlockElements#FuncBlockDef, rb : RuleBlock):
    List[(String, (Double)=>Double)]={

      // this returns the function that is used in aggregation
      // supporting implication
      def resultOfImplication(memFunc:(Double)=>Double, w:Double):((Double)=>Double) = {

        rb.actMeth.get match {
          case "PROD" => (inVal:Double) => {memFunc(inVal)*antecedentResult*w}
          case "MIN" => (inVal:Double) => {
            val rVal = memFunc(inVal)
            val base = if (rVal > antecedentResult) antecedentResult else rVal
            logger.debug("base * w: " + base * w)
            base * w
          }
        }
      }
      clauses.fold[List[(String, (Double)=>Double)]]{logger.debug("no clauses");List(""->((x:Double)=>{0.0}))}(
        consequents => consequents.map(consequent => {
          val varName = consequent.inOrOutVar.get
          logger.debug("varName: " + varName + ", consequent.fuzzyVar.get: " + consequent.fuzzyVar.get)
          val memFunc = fbd.defuzzyBlocks(varName).membershipFunctions(consequent.fuzzyVar.get)
          val w:Double = consequent.weight.fold(1.0){
            case (num: Double) => num;
            // it's a variable so check the inputs then
            // check the local vars
            case (varRef: String) =>
              fbd.inputs.get(varRef).fold[Double](fbd.localVars(varRef).asInstanceOf[Double])(_.toDouble)
          }

          varName -> resultOfImplication(memFunc,w)
        }
        ).toList
      )
    }
  }

  /*
  RuleFeatures,
   */
  trait RuleFeatures{
    //  logger.debug("RULE " + name + ":" + " IF " + antecedent + " THEN " + consequent + " " + weight)
    //  logger.debug("antecedent clauses: " + antecedent.clauses.get.size)
    def name:String
    def antecedent:Clause
    def consequent:Clause


    lazy val exprLst:Either[String, Expr] = antecedent.expr
    logger.debug(exprLst.toString)


    def eval()(implicit fbd : FunctionBlockElements#FuncBlockDef, rb : RuleBlock):
    List[(String, (Double)=>Double)]={
      val degOfSupport = exprLst.right.get.eval()
      logger.debug("Rule: " + name + ", Degree of Support: " + degOfSupport)
      val rMf = consequent.consequentEval(degOfSupport)
      //rMf.foreach(result => {println(result._1 + ": "); simplePlot(result._1,List(0,30),result._2)})

      rMf
    }
  }

  object ruleBaseParser extends FclParser
  case class FclRule(ruleString:String) extends RuleFeatures {
    val x:Either[String, (String, Clause, Clause)] =  ruleBaseParser.parseAll(ruleBaseParser.ruleDecl, ruleString) match {
      case ruleBaseParser.Success(result, _) => result match{
        case ruleBaseParser.ParsedRule(a,b,c) => Right((a,b.asInstanceOf[RuleBase.this.Clause],c.asInstanceOf[RuleBase.this.Clause]))
      }
      case ruleBaseParser.Error(a,b) => Left(ruleBaseParser.Error(a,b).toString)
      case ruleBaseParser.Failure(a,b) => Left(ruleBaseParser.Failure(a,b).toString )
    }

    override val (name, antecedent, consequent) = x.right.get
    override def toString ={
      "name: " + name +
        "\nantecedent: " +antecedent.toString +
        "\nconsequent: " + consequent.toString
    }
  }
  case class ParsedRule(name:String, antecedent:Clause, consequent:Clause) extends RuleFeatures{
  }
  object RuleFeatures {
    def unapply(rule: ParsedRule): Option[(String, Clause, Clause)] =
      Some((rule.name, rule.antecedent, rule.consequent))
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

  case class RuleBlock(name: String, opDef: String, actMeth:Option[String], accuMeth: String, rules:List[ParsedRule]){
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
      val resultMap = rules.flatMap(r =>r.eval()).groupBy(_._1).map {
        case (k,v) => (k,v.map(_._2))
      }
      resultMap.map{case(k,v) => (k, accumulate(v))}
    }
  }

}
object RuleBase extends RuleBase
