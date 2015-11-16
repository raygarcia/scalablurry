

import com.fathomdynamics.fcl.FclParser
import org.scalatest._

import scala.collection.mutable.ListBuffer

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


class FCLEnginSpec extends FlatSpec with Matchers {
  object DeclBlockTest extends FclParser{
    val tipper = stripLineComments(stripBlockComments(
    """
      FUNCTION_BLOCK Fuzzy_FB
          VAR_INPUT
            food: REAL; (* input which can be "rancid" or "delicious" *)
            service: REAL;
          END_VAR

          VAR_OUTPUT
            tip: REAL;
          END_VAR

          FUZZIFY service
            TERM poor := (0, 1) (4, 0) ;
            TERM good := (1, 0) (4,1) (6,1) (9,0);
            TERM excellent := (6, 0) (9, 1);
          END_FUZZIFY

          (* //Fuzzify input variable 'food': { 'rancid', 'delicious' }*)
          FUZZIFY food
            TERM rancid := (0, 1) (1, 1) (3,0) ;
            TERM delicious := (7,0) (9,1);
          END_FUZZIFY

          DEFUZZIFY tip
            RANGE := (0 .. 30);
            TERM cheap   := (0,0) (5,1) (10,0) ;
            TERM average  := (10,0) (15,1) (20,0)
            TERM generous := (20,0) (25,1) (30,0);
            METHOD : CoG;
            DEFAULT := NC;
          END_DEFUZZIFY

          RULEBLOCK RuleBlock_1
            AND: MIN;
            ACT: MIN
            ACCU: MAX;

         //   RULE 1 : IF service IS poor OR food IS rancid THEN tip IS cheap;
         //   RULE 4 : IF service IS NOT poor OR NOT (food IS rancid) THEN tip IS cheap;
         //   RULE 2 : IF service IS good THEN tip IS average;
         //   RULE 3 : IF (service IS excellent AND food IS delicious) THEN tip IS generous;
            RULE 51 : IF (service IS excellent AND food IS rancid) OR (service IS excellent AND food IS delicious) THEN tip IS generous;
            RULE 52 : IF NOT (NOT (service IS excellent AND food IS rancid)) OR NOT (NOT (service IS excellent AND food IS delicious)) THEN tip IS generous;
            RULE 53 : IF NOT ((service IS excellent AND food IS rancid) OR (service IS excellent AND food IS delicious)) THEN tip IS generous;
         //   RULE 6 : IF NOT (service IS excellent AND food IS delicious) THEN tip IS generous;
         //   RULE 7 : IF NOT (service IS excellent AND food IS delicious) THEN tip IS generous;
          END_RULEBLOCK
      END_FUNCTION_BLOCK"""))

    val funcInput = """
      FUNCTION_BLOCK Fuzzy_FB
          VAR_INPUT
            Food: REAL;
            Service: REAL;
          END_VAR

          VAR_OUTPUT
            tip: REAL;
            comeAgain : REAL;
          END_VAR

          FUZZIFY Service
            TERM poor := (0,1) (4, 0) ;
            TERM good := (3,0) (5, 1) (9, 0) ;
            TERM excellent := (6,1) (10, 1);
          END_FUZZIFY

          FUZZIFY Food
            TERM rancid := (0,1) (1, 1)(3, 0)  ;
            TERM delicious := (7,0) (9, 1);
          END_FUZZIFY

          DEFUZZIFY tip
            RANGE := (0 .. 30);
            TERM cheap   := (0,0) (5,1) (10,0) ;
            TERM average  := (10,0) (15,1) (20,0)
            TERM generous := (20,0) (25,0) (30,0);
            METHOD : CoG;
            DEFAULT := NC;
          END_DEFUZZIFY

          DEFUZZIFY comeAgain
            RANGE := (0 .. 30);
            TERM heckNo   := (0,0) (5,1) (10,0) ;
            TERM maybe  := (10,0) (15,1) (20,0)
            TERM definitely := (20,0) (25,0) (30,0);
            METHOD : CoG;
            DEFAULT := NC;
          END_DEFUZZIFY

          RULEBLOCK No1
            AND: MIN;
            ACT: MIN
            ACCU: MAX;
            RULE 1000: IF (service IS poor) OR (food IS rancid OR
             (service IS good AND food IS delicious)) THEN tip IS cheap, comAgain IS heckNo
            RULE 1001: IF (service IS poor) OR (food IS rancid) OR
             (service IS good AND food IS delicious) THEN tip IS cheap, comAgain IS heckNo
            RULE 1002: IF (service IS poor) OR (food IS rancid) OR
             (service IS good AND (food IS delicious)) THEN tip IS cheap, comAgain IS heckNo
            RULE 1003: IF (service IS poor) OR (food IS rancid) OR
             ((service IS good) AND food IS delicious) THEN tip IS cheap, comAgain IS heckNo
            RULE 1004: IF (service IS poor) AND (food IS rancid) OR
             (service IS good) AND (food IS delicious) THEN tip IS cheap, comAgain IS heckNo
            RULE 1005: IF service IS poor AND food IS rancid OR
             service IS good AND food IS delicious THEN tip IS cheap, comAgain IS heckNo
            RULE 1006: IF service IS poor AND food IS rancid OR
             service IS good AND food IS delicious OR (service IS average OR (service IS poor AND food IS rancid)) THEN tip IS cheap, comAgain IS heckNo
            RULE 2: IF (service IS good) THEN tip IS average
            RULE 3: IF (service IS excellent) OR (food IS delicious) THEN tip IS generous
          END_RULEBLOCK
      END_FUNCTION_BLOCK"""

    val errFuncInput = """
      FUNCTION_BLOCK Fuzzy_FB
          VAR_INPUT
            Temp: REAL;
            Pressure: REAL;
          END_VAR

          VAR_OUTPUT
            Valve1: REAL;
            Valve2: REAL;
          END_VAR

          FUZZIFY Temp
            TERM cool := (Pressure,1) (abc, 0) ;
            TERM warm := (3,0) (27, one1) ;
            TERM HOT := (0,0) (100, 1);
          END_FUZZIFY

          DEFUZZIFY Valve1
            RANGE := (0 .. 100);
            TERM open   := (0,1) (abc, 0) ;
            TERM close  := (3,0) (27, one1) ;
            TERM stable1 := 50;
            TERM stable := 50;

            TERM close  := (3,1) (27, one1) ;

            METHOD : CoG;
            DEFAULT := NC;
          END_DEFUZZIFY

          RULEBLOCK No1
            AND: MIN;
            ACCU: MAX;
            RULE 1: IF temp IS cold AND pressure IS low THEN valve IS inlet;
            RULE 2: IF temp AND pressure IS high THEN valve IS closed WITH 0.8;
            RULE 3: IF temp IS hot AND pressure IS low  THEN valve IS closed;
            RULE 4: IF temp IS hot AND pressure IS high THEN valve IS drainage;
          END_RULEBLOCK


      END_FUNCTION_BLOCK"""

    def runFuncBlock = parseAll(DeclBlockTest.funcBlock, errFuncInput)


    def runRuleBlock = parseAll(DeclBlockTest.ruleBlockDecl, """
           RULEBLOCK No1
            AND: MIN;
            ACCU: MAX;
            RULE 1: IF temp IS cold AND pressure IS low AND (A IS B OR A IS C) THEN valve IS inlet;
  ￼￼        RULE 2: IF temp IS cold AND pressure IS high THEN valve IS closed WITH 0.8;
            RULE 3: IF temp IS hot AND pressure IS low  THEN valve IS closed;
            RULE 4: IF temp IS hot AND pressure IS high THEN valve IS drainage, valve IS closed;
          END_RULEBLOCK
                                                             """)
  }

  "Function ERROR-free Block" should "eval input and generate output" in {
    println(DeclBlockTest.tipper)
    val fBlocks = {
      val tipParser = DeclBlockTest.parseAll(DeclBlockTest.funcBlock,
        DeclBlockTest.tipper)
      //println("DeclBlockTest.funcBlockDefs: " +  DeclBlockTest.funcBlockDefs)
      DeclBlockTest.funcBlockDefs
    }
//    fBlocks.foreach(fb => println("FuzzyBloc: " + fb._1 + " " + fb._2.toString))
    fBlocks.foreach(fb => {
      val out = ListBuffer[Map[String, Double]]();

/*
      for (i <-0.0 to 10.0 by .10){
        val x = fb._2.eval(List(i,i))
        out ++= (x).map(o => o._2)
      }
*/
      println(fb._2.eval(List(8.5,9.0)))
//      println(out)
      out.foreach(o =>println("in: " + fb._2.inputs + ", out: " + o))
//      System.in.read()
    })
    //   println(DeclBlockTest.parseAll(DeclBlockTest.funcBlock, DeclBlockTest.tipper))

    /*
    Consequent fbd: FuncBlockDef(
          Fuzzy_FB,
          List((food,REAL), (service,REAL)),List((tip,REAL)),
          List(
            FuzzifyBlock(service,List((poor,List(Point(0.0,1.0), Point(4.0,0.0))), (good,List(Point(3.0,0.0), Point(5.0,1.0), Point(9.0,0.0))), (excellent,List(Point(6.0,1.0), Point(10.0,1.0))))),
            FuzzifyBlock(food,List((rancid,List(Point(0.0,1.0), Point(1.0,1.0), Point(3.0,0.0))), (delicious,List(Point(7.0,0.0), Point(9.0,1.0)))))),
          List(
            DefuzzifyBlock(tip,List(0.0, 30.0),List(
              (cheap,List(Point(0.0,0.0), Point(5.0,1.0), Point(10.0,0.0))),
              (average,List(Point(10.0,0.0), Point(15.0,1.0), Point(20.0,0.0))),
              (generous,List(Point(20.0,0.0), Point(25.0,0.0), Point(30.0,0.0)))),CoG),
            DefuzzifyBlock(comeAgain,List(0.0, 30.0),List(
              (heckNo,List(Point(0.0,0.0), Point(5.0,1.0), Point(10.0,0.0))),
              (maybe,List(Point(10.0,0.0), Point(15.0,1.0), Point(20.0,0.0))),
              (definitely,List(Point(20.0,0.0), Point(25.0,0.0), Point(30.0,0.0)))),CoG)),
          List(RuleBlock(No1,MIN,Some(MIN),MAX,List(Rule(1,Clause(None,None,None,None,Some(ListBuffer(Clause(None,None,None,None,Some(ListBuffer(Clause(None,Some(service),None,Some(poor),Some(ListBuffer()),false))),false), Clause(Some(OR),None,None,None,Some(ListBuffer(Clause(None,None,None,None,Some(ListBuffer(Clause(None,Some(food),None,Some(rancid),Some(ListBuffer()),false))),false))),false))),false),Clause(None,None,None,None,Some(ListBuffer(Clause(Some(Imp),Some(tip),None,Some(cheap),Some(ListBuffer()),false))),false),None), Rule(2,Clause(None,None,None,None,Some(ListBuffer(Clause(None,None,None,None,Some(ListBuffer(Clause(None,Some(service),None,Some(good),Some(ListBuffer()),false))),false))),false),Clause(None,None,None,None,Some(ListBuffer(Clause(Some(Imp),Some(tip),None,Some(average),Some(ListBuffer()),false))),false),None), Rule(3,Clause(None,None,None,None,Some(ListBuffer(Clause(None,None,None,None,Some(ListBuffer(Clause(None,Some(service),None,Some(excellent),Some(ListBuffer()),false))),false), Clause(Some(AND),None,None,None,Some(ListBuffer(Clause(None,None,None,None,Some(ListBuffer(Clause(None,Some(food),None,Some(delicious),Some(ListBuffer()),false))),false))),false))),false),Clause(None,None,None,None,Some(ListBuffer(Clause(Some(Imp),Some(tip),None,Some(generous),Some(ListBuffer()),false))),false),None))))), varName: tip, consequent.fuzzyVar.get: cheap

    DeclBlockTest.funcBlockDefs:
      Map(Fuzzy_FB ->
          FuncBlockDef(Fuzzy_FB,
            List((Food,REAL), (Service,REAL)),
            List((tip,REAL)),
            List(
              FuzzifyBlock(Service,List((poor,List(Point(0.0,1.0), Point(4.0,0.0))), (good,List(Point(3.0,0.0), Point(5.0,1.0), Point(9.0,0.0))), (excellent,List(Point(6.0,1.0), Point(10.0,1.0))))),
              FuzzifyBlock(Food,List((rancid,List(Point(0.0,1.0), Point(1.0,1.0), Point(3.0,0.0))), (delicious,List(Point(7.0,0.0), Point(9.0,1.0)))))),
            List(
              DefuzzifyBlock(tip,List(0.0, 30.0),
                List((cheap,List(Point(0.0,0.0), Point(5.0,1.0), Point(10.0,0.0))),
                  (average,List(Point(10.0,0.0), Point(15.0,1.0), Point(20.0,0.0))),
                  (generous,List(Point(20.0,0.0), Point(25.0,0.0), Point(30.0,0.0)))),CoG),
              DefuzzifyBlock(comeAgain,List(0.0, 30.0),
                List((heckNo,List(Point(0.0,0.0), Point(5.0,1.0), Point(10.0,0.0))),
                  (maybe,List(Point(10.0,0.0), Point(15.0,1.0), Point(20.0,0.0))),
                  (definitely,List(Point(20.0,0.0), Point(25.0,0.0), Point(30.0,0.0)))),CoG)),
            List(
              RuleBlock(No1,MIN,Some(MIN),MAX,
                List(
                  Rule(1,
                    Clause(None,None,None,None,Some(ListBuffer(
                      Clause(None,None,None,None,Some(ListBuffer(
                        Clause(None,Some(service),None,Some(poor),Some(ListBuffer()),false))),false),
                      Clause(Some(OR),None,None,None,Some(ListBuffer(
                        Clause(None,None,None,None,Some(ListBuffer(
                          Clause(None,Some(food),None,Some(rancid),Some(ListBuffer()),false))),false))),false))),false),
                    Clause(None,None,None,None,Some(ListBuffer(
                      Clause(Some(Imp),Some(tip),None,Some(cheap),Some(ListBuffer()),false))),false),None),
                  Rule(2,
                    Clause(None,None,None,None,Some(ListBuffer(
                      Clause(None,None,None,None,Some(ListBuffer(
                        Clause(None,Some(service),None,Some(good),Some(ListBuffer()),false))),false))),false),
                    Clause(None,None,None,None,Some(ListBuffer(
                      Clause(Some(Imp),Some(tip),None,Some(average),Some(ListBuffer()),false))),false),None),
                  Rule(3,
                    Clause(None,None,None,None,Some(ListBuffer(
                      Clause(None,None,None,None,Some(ListBuffer(
                        Clause(None,Some(service),None,Some(excellent),Some(ListBuffer()),false))),false),
                      Clause(Some(AND),None,None,None,Some(ListBuffer(
                        Clause(None,None,None,None,Some(ListBuffer(
                          Clause(None,Some(food),None,Some(delicious),Some(ListBuffer()),false))),false))),false))),false),
                    Clause(None,None,None,None,Some(ListBuffer(
                      Clause(Some(Imp),Some(tip),None,Some(generous),Some(ListBuffer()),false))),false),None))))))
         */

    /*

        println("Input Declarations: " + DeclBlockTest.inDecls.keySet)
        println("Output Declarations: " + DeclBlockTest.outDecls.keySet)
        DeclBlockTest.dumpSemanticResults
    */
    true should === (true)
  }
  /*
    "Function Block" should "dump input and output decls" in {
      println( DeclBlockTest runFuncBlock)

      println("Input Declarations: " + DeclBlockTest.inDecls.keySet)
      println("Output Declarations: " + DeclBlockTest.outDecls.keySet)
      DeclBlockTest.dumpSemanticResults
      true should === (true)
    }

    "Fuzzification Block" should "get fuzzify Temp @ Values: (0, 25, 50, 75, 100, and 1000)" in {
      val testValues = List[Double](0, 25, 50, 75, 1000)
      DeclBlockTest.runFuncBlock
      val f = DeclBlockTest.funcBlockDefs.head._2.fuzzifyBlock.head.fuzzifierMap
      testValues.foreach(v => println("Hot(" + v + "): " + f("HOT")(v)))
      true should === (true)
    }


    "Rule Decl Block" should "parse rules and evaluate them" in {
      DeclBlockTest.runFuncBlock
      val f = DeclBlockTest.funcBlockDefs.head._2.eval()
      println("Sum: " + f)
      true should === (true)
    }
  */
}
