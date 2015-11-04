package com.fathomdynamics.fcl.ruleBase

import com.fathomdynamics.fcl.engine.FunctionBlockElements
import com.fathomdynamics.fcl.util.{Utils, Validators}
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
  case class Expr(left:Either[String,Expr], op:String, right:Either[String,Expr]){

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

    def expr:Either[String,Expr] = {
      //No list?  It's a simple expr
      inputVar.fold[Either[String,Expr]](clauses2Expr(clauses.get))(iVar =>{
      val basicExpr = Right(Expr(Left(inputVar.get),"is",Left(fuzzyVar.get)))
      clauses.fold(basicExpr)(clist=>
      if (clist.size > 0){Right(Expr(Right(Expr(Left(inputVar.get),"is",Left(fuzzyVar.get))),
        clauses.get.head.operator.get, clauses2Expr(clauses.get)))}
        else {
        basicExpr
        }
         )})
    }
    def clauses2Expr(cList:ListBuffer[Clause]):Either[String,Expr] = {
      if (cList.size == 0) {
        Right(Expr(Left(inputVar.get), "is", Left(fuzzyVar.get)))
      }
      else if (cList.size == 1) {
        cList.head.expr;
      } else if (cList.size == 2) {
        println("cList(0): " + cList(0).expr)
        println("cList.head.expr: " + cList.head.expr)
        Right(Expr(cList.head.expr, cList.last.operator.get, cList.last.expr))
      } /*else if (clist.size == 3){
          clist(1).cList.get += clist.last
          clist -= clist.last
          Right(Expr(clist.head.expr, clist.last.operator.get, clist.last.expr))
        }*/
      else {
        // 4 or more
        // pair up based on operator
        println("Pre-paired Len:" + cList.length)
        val pList = pairUp(cList)
        println("Post-paired Len: " + pList.length)

        // now that all the AND operations are paired up ,
        // let's nest them

        val toBeDel = ListBuffer[Clause]()
        for (i <- 1 until (pList.length - 1)) {
          println("removing...")
          pList.head.clauses match {
            case Some(_) => {
              pList.head.clauses.get += pList(i)
              toBeDel += pList(i)
            }
            case None =>
          }
        }
        println("ToBeDeleted: " + toBeDel)
        toBeDel.foreach(i => pList -= i)

        //assert(pList.length == 1)
        println("Post-nested size: " + pList.length)
        println("Post-nested head size: " + pList.head.clauses.get.length)
        clauses.get.clear()
        clauses.get ++= pList
        println("cList: " + cList)
        println("clause size: " + cList.size)
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
          case None => println("empty clauses...")
        }
        k += 1
      }

      val lf = list.filter(i => i.operator != Some("AND"))
      println("filtered: " + lf)
      lf
    }


    /*
  IF service IS poor AND food IS rancid OR service IS good AND food IS delicious OR
    (service IS average OR (service IS poor AND food IS rancid))

Right(Expr(
  Right(Expr(
    Right(Expr(
      Right(Expr(Left(service),is,Left(poor))),AND,Right(Expr(Left(food),is,Left(rancid))))),OR,
    Right(Expr(
      Right(Expr(Left(service),is,Left(good))),AND,Right(Expr(Left(food),is,Left(delicious))))))),OR
  ,Right(Expr(
    Right(Expr(Left(service),is,Left(average))),OR,
    Right(Expr(
      Right(Expr(Left(service),is,Left(poor))),AND,Right(Expr(Left(food),is,Left(rancid)))))))))

  ListBuffer(
  Clause(None,Some(service),None,Some(poor),Some(ListBuffer(
    Clause(Some(AND),None,None,None,Some(ListBuffer(
      Clause(None,None,None,None,Some(ListBuffer(
        Clause(None,Some(food),None,Some(rancid),Some(ListBuffer()),false))),false))),false))),false),
  Clause(Some(OR),None,None,None,Some(ListBuffer(
    Clause(None,None,None,None,Some(ListBuffer(
      Clause(None,Some(service),None,Some(good),Some(ListBuffer()),false))),false),
    Clause(Some(AND),None,None,None,Some(ListBuffer(
      Clause(None,None,None,None,Some(ListBuffer(
        Clause(None,Some(food),None,Some(delicious),Some(ListBuffer()),false))),false))),false))),false))

    IF service IS poor AND food IS rancid OR
        service IS good AND food IS delicious THEN tip IS cheap, comAgain IS heckNo
  Right(Expr(
  Right(Expr(
    Right(Expr(Left(service),is,Left(poor))),AND,
    Right(Expr(Left(food),is,Left(rancid))))),
  OR,
  Right(Expr(
    Right(Expr(Left(service),is,Left(good))),AND,
    Right(Expr(Left(food),is,Left(delicious)))))))

    */
    /* IF temp IS cold AND pressure IS low
          ----- Produces ----
  Clause(None,None,None,None,Some(
  List(Clause(None,None,None,None,Some(
    List(Clause(None,Some(temp),None,Some(cold),None,false))),false),
      Clause(Some(AND),None,None,None,Some(
        List(Clause(None,None,None,None,Some(
          List(Clause(None,Some(pressure),None,Some(low),None,false))),false))),false))),false)

  Clause(None,None,None,None,Some(
  List(Clause(None,None,None,None,Some(
    List(Clause(None,None,None,None,Some(
      List(Clause(None,None,None,None,Some(
        List(Clause(None,Some(service),None,Some(poor),None,false))),false))),false))),true), Clause(Some(OR),None,None,None,Some(List(Clause(None,None,None,None,Some(List(Clause(None,None,None,None,Some(List(Clause(None,None,None,None,Some(List(Clause(None,Some(food),None,Some(rancid),None,false))),false), Clause(Some(AND),None,None,None,Some(List(Clause(None,None,None,None,Some(List(Clause(None,Some(service),None,Some(good),None,false))),false))),false))),false))),true))),false))),false)


  */
    /*
            RULE 1000: IF (service IS poor) OR
      (food IS rancid OR (service IS good AND food IS delicious))
  Right(Expr(Right(Expr(Left(service),is,Left(poor))),OR,
  Right(Expr(Right(Expr(Left(food),is,Left(rancid))),OR,
  Right(Expr(Right(Expr(Left(service),is,Left(good))),AND,
    Right(Expr(Left(food),is,Left(delicious)))))))))

  Clause(None,None,None,None,Some(List(
  Clause(None,None,None,None,Some(List(
    Clause(None,None,None,None,Some(List(
      Clause(None,None,None,None,Some(List(
        Clause(None,Some(service),None,Some(poor),None,false))),false))),false))),true),
  Clause(Some(OR),None,None,None,Some(List(
    Clause(None,None,None,None,Some(List(
      Clause(None,None,None,None,Some(List(
        Clause(None,None,None,None,Some(List(
          Clause(None,Some(food),None,Some(rancid),None,false))),false),
        Clause(Some(OR),None,None,None,Some(List(
          Clause(None,None,None,None,Some(List(
            Clause(None,None,None,None,Some(List(
              Clause(None,None,None,None,Some(List(
                Clause(None,Some(service),None,Some(good),None,false))),false),
              Clause(Some(AND),None,None,None,Some(List(
                Clause(None,None,None,None,Some(List(
                  Clause(None,Some(food),None,Some(delicious),None,false))),false))),false))),false))),true))),false))),false))),true))),false))),false)

            RULE 1001: IF (service IS poor) OR (food IS rancid) OR
             (service IS good AND food IS delicious)
  Clause(None,None,None,None,Some(List(
  Clause(None,None,None,None,Some(List(
    Clause(None,None,None,None,Some(List(
      Clause(None,None,None,None,Some(List(
        Clause(None,Some(service),None,Some(poor),None,false))),false))),false))),true),
  Clause(Some(OR),None,None,None,Some(List(
    Clause(None,None,None,None,Some(List(
      Clause(None,None,None,None,Some(List(
        Clause(None,None,None,None,Some(List(
          Clause(None,Some(food),None,Some(rancid),None,false))),false))),false))),true))),false),
  Clause(Some(OR),None,None,None,Some(List(
    Clause(None,None,None,None,Some(List(
      Clause(None,None,None,None,Some(List(
        Clause(None,None,None,None,Some(List(
          Clause(None,Some(service),None,Some(good),None,false))),false),
        Clause(Some(AND),None,None,None,Some(List(
          Clause(None,None,None,None,Some(List(
            Clause(None,Some(food),None,Some(delicious),None,false))),false))),false))),false))),true))),false))),false)

            RULE 1002: IF (service IS poor) OR (food IS rancid) OR
             (service IS good AND (food IS delicious))
  Clause(None,None,None,None,Some(List(
  Clause(None,None,None,None,Some(List(
    Clause(None,None,None,None,Some(List(
      Clause(None,None,None,None,Some(List(
        Clause(None,Some(service),None,Some(poor),None,false))),false))),false))),true),
  Clause(Some(OR),None,None,None,Some(List(
    Clause(None,None,None,None,Some(List(
      Clause(None,None,None,None,Some(List(
        Clause(None,None,None,None,Some(List(
          Clause(None,Some(food),None,Some(rancid),None,false))),false))),false))),true))),false),
  Clause(Some(OR),None,None,None,Some(List(
    Clause(None,None,None,None,Some(List(
      Clause(None,None,None,None,Some(List(
        Clause(None,None,None,None,Some(List(
          Clause(None,Some(service),None,Some(good),None,false))),false),
        Clause(Some(AND),None,None,None,Some(List(
          Clause(None,None,None,None,Some(List(
            Clause(None,None,None,None,Some(List(
              Clause(None,None,None,None,Some(List(
                Clause(None,Some(food),None,Some(delicious),None,false))),false))),false))),true))),false))),false))),true))),false))),false)

            RULE 1003: IF (service IS poor) OR (food IS rancid) OR
             ((service IS good) AND food IS delicious)
  Clause(None,None,None,None,Some(List(
  Clause(None,None,None,None,Some(List(
    Clause(None,None,None,None,Some(List(
      Clause(None,None,None,None,Some(List(
        Clause(None,Some(service),None,Some(poor),None,false))),false))),false))),true),
  Clause(Some(OR),None,None,None,Some(List(
    Clause(None,None,None,None,Some(List(
      Clause(None,None,None,None,Some(List(
        Clause(None,None,None,None,Some(List(
          Clause(None,Some(food),None,Some(rancid),None,false))),false))),false))),true))),false),
  Clause(Some(OR),None,None,None,Some(List(
    Clause(None,None,None,None,Some(List(
      Clause(None,None,None,None,Some(List(
        Clause(None,None,None,None,Some(List(
          Clause(None,None,None,None,Some(List(
            Clause(None,None,None,None,Some(List(
              Clause(None,Some(service),None,Some(good),None,false))),false))),false))),true),
        Clause(Some(AND),None,None,None,Some(List(
          Clause(None,None,None,None,Some(List(
            Clause(None,Some(food),None,Some(delicious),None,false))),false))),false))),false))),true))),false))),false)

    */

    var x=0
    // groups = all none with  inner = true
    // build a stack of operations that eval to a double
    // every clause returns: Nothing, op, or expr
    // nothing => look for expr
    // expr ==> look for op
    // op => look for expr
    // if nothing but a list, keep looking
    // if op

    def iterateClauses(c:Clause):Either[String, Expr]={
      val e =dump(c)
      println("clause: " + c)
      println(e)
      e
    }

    def dump(c:Clause):Either[String,Expr] = {

      val e:Either[String, Expr] =
        c.operator.fold[Either[String,Expr]](c.inputVar.
        fold[Either[String,Expr]](c.clauses.fold[Either[String,Expr]](Left("no clauses")) (innerClauses =>
            innerClauses.foldLeft[Either[String, Expr]](Left("nada"))((finalExpr, c:Clause) =>
              iterateClauses(c))))(inVar =>
            Right(Expr(Left(inVar), "is",Left(c.fuzzyVar.get)))))(op=>
            Left(op))

      e
    }
    def processClauses(clauses: ListBuffer[Clause]):Double = {
      2
    }

    def antecedentEval()(implicit fbd : FunctionBlockElements#FuncBlockDef, rb : RuleBlock):Double = {

      // no operator + no list of clauses statement = input IS fuzzyName
      clauses.fold[Double] {
        fbd.fuzzyBlocks(inputVar.get).fuzzifierMap(fuzzyVar.get)(1.0) // TODO:get the input
      }(innerClauses => processClauses(innerClauses))

      /*
            (innerClauses => innerClauses.foldLeft(0.0)(
                (r,c:Clause) => r
                  + c.clauses.fold[Double](fbd.fuzzyBlocks(inputVar.get).fuzzifierMap(fuzzyVar.get)(1.0))
                  (clauseList => clauseList.foreach(_.antecedentEval()))
              ))
      */
    }

    /*
         THEN valve IS inlet
          ----- Produces ----
  Clause(None,None,None,None,Some(List(Clause(Some(Imp),Some(valve),None,Some(inlet),None,false))),false)

  Clause(None,None,None,None,
  Some(List(Clause(Some(Imp),Some(comAgain),None,Some(heckNo),None,false), Clause(Some(Imp),Some(tip),None,Some(cheap),None,false))),false)
     */

    def consequentEval(antecedent: Clause)(implicit fbd : FunctionBlockElements#FuncBlockDef, rb : RuleBlock):
    ListBuffer[(String, (Double)=>Double)]={
      val antecedentResult = antecedent.antecedentEval()
      def resultOfImplication(memFunc:(Double)=>Double):((Double)=>Double) = {
        rb.actMeth.get match {
          case "PROD" => (inVal:Double) => {memFunc(inVal)*antecedentResult}
          case "MIN" => (inVal:Double) => {
            val rVal = memFunc(inVal)
            if (rVal > antecedentResult) antecedentResult else rVal
          }
        }
      }
      clauses.fold[ListBuffer[(String, (Double)=>Double)]](ListBuffer((""->((x:Double)=>{0.0}))))(
        consequents => consequents.map(consequent => {
          val varName = consequent.inputVar.get
          val memFunc = fbd.defuzzyBlocks(varName).membershipFunctions(consequent.fuzzyVar.get)
          varName -> resultOfImplication(memFunc)
        }
        )
      )
    }
  }

  case class Rule(name:String, antecedent:Clause, consequent:Clause, weight:Option[Any]){
    println("RULE " + name + ":" + " IF " + antecedent + " THEN " + consequent + " " + weight)
    println("antecedent clauses: " + antecedent.clauses.get.size)

    val exprLst = antecedent.expr
    println(exprLst)

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
  def bdifMin = (sides:ListBuffer[Double], op:String) =>{
    op match {
      case "AND" => List(0, sides.sum -1).max
      case "OR" => List(1, sides.sum).min
    }
  }

  def minMax = (sides:ListBuffer[Double], op:String) =>{
    op match {
      case "AND" => sides.min
      case "OR" => sides.max
    }
  }

  def prodAsum = (sides:ListBuffer[Double], op:String) =>{
    op match {
      case "AND" => sides.product
      case "OR" => sides.sum - sides.product
    }
  }

  case class RuleBlock(name: String, opDef: String, actMeth:Option[String], accuMeth: String, rules:ListBuffer[Rule]){
    thisRuleBlock =>
    // get algorithm and generate an appropriate activation function
    def activate= {
      rb.opDef match {
        case "BDIF" | "MIN" =>bdifMin
        case "MIN" | "MAX" => minMax
        case "PROD" | "ASUM" => prodAsum
      }
    }

    // Use accumulation method
    implicit val rb:RuleBlock = thisRuleBlock //
    def eval()(implicit fbd : FunctionBlockElements#FuncBlockDef) = {
      rules.foreach(r =>r.eval())
    }
  }

}
