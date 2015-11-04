/**
 * Created by Raymond Garcia, Ph.D. (ray@fathomdynamics.com) on 9/13/2015.
 *  The MIT License (MIT)

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

package com.fathomdynamics.fcl

import scala.util.parsing.combinator.JavaTokenParsers
import com.fathomdynamics.fcl.defuzzification.Defuzzification
import com.fathomdynamics.fcl.engine.FunctionBlockElements
import com.fathomdynamics.fcl.fuzzification.Fuzzification
import com.fathomdynamics.fcl.ruleBase.RuleBase

import scala.collection.mutable._
import scala.language.{implicitConversions, existentials}


class FclParser extends JavaTokenParsers with Fuzzification with Defuzzification with FunctionBlockElements with RuleBase{
  //================================================  Literals and Support Decls  ===============================================
  // 61131-3-2003 Section
  def semiCol:Parser[Any] = opt(";")
  def varType: Parser[String] = ("REAL" | "INT" | "BOOL" | "SINT" | "INT" | "DINT" | "LINT" | "USINT" | "UINT" | "UDINT" | "ULINT" |
    "BYTE" | "WORD"|"DWORD"|"LWORD" | "REAL" | "LREAL" | "TIME" | "DATE" | "TIME_OF_DAY" | "DATE_AND_TIME" | "STRING" | "WSTRING")<~semiCol

  def ptValue: Parser[String] = decimalNumber|floatingPointNumber
  def varName: Parser[String] = ident
  def entriesDbl : Parser[Point] = ptValue~","~ptValue ^^ {case pX~","~pY => { Point(pX.toDouble, pY.toDouble)}}
  def entriesLeftVar : Parser[Point] = varName~","~ptValue ^^ {case pX~","~pY => { Point(pX, pY.toDouble)}}
  def entriesRightVar : Parser[Point] = ptValue~","~varName ^^ {case pX~","~pY => { Point(pX.toDouble, pY)}}
  def point : Parser[Point] = "("~>(entriesDbl|entriesLeftVar|entriesRightVar)<~")"
  //-----------------------------------------------------------------------------------------------------------------------------
  //==================================================  Input/Output Blocks  ====================================================
  def inputDecl : Parser[(String, String)] = varName~":"~varType ^^ { case name~":"~varType => {(name, varType)}}
  def varInput : Parser[List[(String, String)]] = "VAR_INPUT"~>rep(inputDecl)<~"END_VAR"

  def outputDecl : Parser[(String, String)] = varName~":"~varType ^^ { case name~":"~varType => {(name, varType)}}
  def varOutput : Parser[List[(String, String)]] = "VAR_OUTPUT"~>rep(outputDecl)<~"END_VAR"
  //-----------------------------------------------------------------------------------------------------------------------------
  //=================================================  Fuzzification Blocks  ====================================================
  def termPair : Parser[Tuple2[String, List[Point]]] = ident~":="~rep(point) ^^ {case name~":="~list => { name -> list}}
  def memFuncDecl : Parser[Tuple2[String, List[Point]]] = "TERM"~>termPair<~semiCol
  def fuzzifyBlockDecl : Parser[FuzzifyBlock] = "FUZZIFY" ~ varName~rep(memFuncDecl)~"END_FUZZIFY" ^^ {
    case open~id~memFuncDecls~close =>  FuzzifyBlock(id, memFuncDecls)
  }
  def num : Parser[String] = decimalNumber|floatingPointNumber
  //------------------------------------------------------------------------------------------------------------------------------
  //================================================  Defuzzification  ===========================================================
  /*
  DEFUZZIFY variable_name
    RANGE(min..max);
    TERM term_name:= membership_function;
    defuzzification_method;
    default_value;
  END_DEFUZZIFY
   */

  def rangeVal : Parser[List[Double]] = num ~ """[.]{2}""".r ~ num ^^ {case low~delim~high => {List(low.toDouble,high.toDouble)}}
  def rangeStmnt : Parser[List[Double]] = "RANGE" ~":=" ~ "("~>rangeVal<~")"~semiCol
  def nameValPair : Parser[Tuple2[String, Double]] = ident~":="~num ^^ { case name~":="~value => {name -> value.toDouble}}
  def singleton : Parser[Tuple2[String, Double]] = "TERM"~>nameValPair<~semiCol
  def defuzMethodVal : Parser[String] = "CoG"|"CoGS"| "CoA" | "LM" | "RM"
  def defuzMethodStmnt : Parser[String] = "METHOD" ~":"~>defuzMethodVal<~semiCol
  def defaultVal : Parser[String] = num| "NC"
  def defaultStmnt : Parser[Any] = "DEFAULT" ~":="~>defaultVal<~semiCol
  def defuzzifyBlockId : Parser[String] = "DEFUZZIFY" ~> varName ^^ {case varName => checkOutDecls(varName); varName}
  def mixDecl : Parser[Tuple2[String, Any]] =  singleton|memFuncDecl
  def defuzzifyBlockDecl : Parser[DefuzzifyBlock] = defuzzifyBlockId~rangeStmnt~rep(mixDecl)~defuzMethodStmnt~defaultStmnt~"END_DEFUZZIFY" ^^ {
    case defuzzifyBlockId~rangeStmnt~mixDecls~defuzMethodStmnt~defaultStmnt~"END_DEFUZZIFY" => { DefuzzifyBlock(defuzzifyBlockId, rangeStmnt, mixDecls, defuzMethodStmnt)}
  }
  //-----------------------------------------------------------------------------------------------------------------------------
  //=====================================================  Rules  ===============================================================
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

  // RULE 1: IF temp IS cold AND pressure IS low THEN valve IS inlet;

  def ruleDecl : Parser[Rule] = "RULE"~num~ ":"~"IF"~ conditionExpr~ "THEN"~ conclusion~opt(weightFactor)~semiCol ^^ {
    case "RULE"~num~ ":"~"IF"~ condition~ "THEN"~ conclusion~weight~semiCol => Rule(num,condition, conclusion, weight)}

  def subExpr : Parser[Clause] = opt("NOT")~ subcondition ^^ {case op~expr =>
    Clause(operator = op, inputVar= None, notOpt = None, fuzzyVar=None, clauses=Option(ListBuffer(expr)/*.sortWith(_.innerParens > _.innerParens )*/))}

  def x : Parser[Clause] = (subExpr) | (opt("NOT")~"("~ conditionExpr ~")" ) ^^ { case op~"("~expr~")" =>
    Clause(operator = op, inputVar= None, notOpt = None, fuzzyVar=None, clauses=Option(ListBuffer(expr)/*.sortWith(_.innerParens > _.innerParens )*/), innerParens=true)}

  def subcondition : Parser[Clause] = conditionClauseExpr|varName ^^ { case varName =>
    Clause(operator = None, inputVar= None, notOpt = None, fuzzyVar=Option(varName), clauses=None)}

  def andOrOpExpr : Parser[Clause] = ("AND"|"OR")~x ^^ {case op ~ expr =>
    Clause(operator = Option(op), inputVar= None, notOpt = None, fuzzyVar=None, clauses=Option(ListBuffer(expr)/*.sortWith(_.innerParens > _.innerParens )*/))}

  def conditionExpr : Parser[Clause] = x~rep(andOrOpExpr) ^^ {case left~exprLst =>
    Clause(operator = None, inputVar= None, notOpt = None, fuzzyVar=None, clauses=Option((left +=: exprLst.to[ListBuffer])/*.sortWith(_.innerParens > _.innerParens )*/))}

  def conditionClauseExpr:Parser[Clause] =  varName~ "IS" ~opt("NOT")~ varName ^^ {case left~"IS"~optNot~right =>
    Clause(operator = None, inputVar= Option(left), notOpt = optNot, fuzzyVar=Option(right), clauses=Option(ListBuffer()))}

  def conclusionClauseExpr : Parser[Clause] = varName ~"IS"~ varName ^^ {case left~"IS"~right =>
    Clause(operator = Option("Imp"), inputVar= Option(left), notOpt = None, fuzzyVar=Option(right), clauses=Option(ListBuffer()))}

  def termAssignmentOrVar : Parser[Clause] = conclusionClauseExpr|varName ^^ {case varName =>
    Clause(operator = None, inputVar= None, notOpt = None, fuzzyVar=Option(varName), clauses=None)}

  def termAssignmentOrVarSeq : Parser[Clause] = termAssignmentOrVar <~ ","
  def conclusion : Parser[Clause] = rep(termAssignmentOrVarSeq)~termAssignmentOrVar ^^ {case termLst~termAssign =>
    Clause(operator = None, inputVar= None, notOpt = None, fuzzyVar=None, clauses=Option((termLst.to[ListBuffer] += termAssign)/*.sortWith(_.innerParens > _.innerParens )*/))}

  def weightFactor : Parser[Any] = "WITH"~>(varName | num) ^^ {case num => num.toDouble}

  def ruleBlockDecl : Parser[RuleBlock] = "RULEBLOCK" ~ varName~opDef~opt(actMeth)~accuMeth~rep(ruleDecl)~"END_RULEBLOCK" ^^ {
    case open~id~opDef~actMeth~accuMeth~rules~close => RuleBlock(id, opDef,actMeth,accuMeth, rules.to[ListBuffer])
  }
  //-------------------------------------------------------------------------------------------------------------------------------
  //===================================================  Function Block  ==========================================================
  def funcBlock = "FUNCTION_BLOCK"~varName~varInput~varOutput~rep(fuzzifyBlockDecl)~rep(defuzzifyBlockDecl)~
    rep(ruleBlockDecl)~"END_FUNCTION_BLOCK" ^^ {
    case beg~varName~inBlk~outBlk~fuzzifyBlks~defuzzBlks~ruleBlockDecls~end => funcBlockDefs += (varName -> FuncBlockDef(varName,
      inBlk, outBlk, fuzzifyBlks, defuzzBlks, ruleBlockDecls))
  }
  //-------------------------------------------------------------------------------------------------------------------------------

}




