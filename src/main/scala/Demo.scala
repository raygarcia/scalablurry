/**
  * Created by Raymond Garcia, Ph.D. (ray@fathomdynamics.com) on 10/12/2015.
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


import ch.qos.logback.classic.{Level, Logger}
import com.fathomdynamics.fcl.FclParser

import scala.io.Source

object TheApp extends FclParser {

  def main(args: Array[String]) {
    val fclFile = Source.fromFile("examples/scalablurry/tipper.fcl")
    val fclFileStr = fclFile.mkString
    println("------ Processing the tipper.fcl demo... ------")
    println(fclFileStr)
    println("-----------------------------------------------")
    val compileOutput = parseAll(funcBlocks, stripLineComments(stripBlockComments(fclFileStr)))

    println("""Here's the parsed output in terms of parser "primitives" """)
    println(compileOutput.toString)
    println("-------------------------------------------------------------")
    fclFile.close()
    // THE OUTPUT IS A NESTED MAP:
    // There are multiple function blocks,
    // there are multiple rule blocks for every function block,
    // and finally, there can be multiple outputs for each rule block.  Hence,
    // -- Map[Func Block Name, Map[Rule Block Name, Map[Output, numerical Val]]]
    val out = scala.collection.mutable.Map[String,Map[String, Map[String, Double]]]();
    val in = (0.0 to 5.0 by 0.5).map(v => List(v,v))

    // fb._1 is the name of the function block
    // fb._2 is the functionBlock object
    // we know this is a two function block FCL file
    val fbLeft = funcBlockDefs.head;
    val fbRight = funcBlockDefs.last
    fbLeft._2.plot; fbRight._2.plot
    val sb = new StringBuilder
    for (i <-in){
      // x._1 = ruleblock name and
      // x._2 = Map[OutputName, OutputValue]
      val t0 = System.nanoTime()
      val x1 = fbLeft._2.eval(i)
      val t1 = System.nanoTime()
      val d1 = t1 - t0
      print(i +" of " + in.size + " Elapsed time: " + (t1 - t0) + "ns,  ")

      val tb1 = System.nanoTime()
      val x2 = fbRight._2.eval(i)
      val tb2 = System.nanoTime()
      val d2 = tb2 - tb1

      // there is only one rule bluck so use the head
      // there is only one output so use the head

      sb.append(  i + "\t" + x1.head._1 + "\t" + x1.head._2.head._1 + " (in " + d1 + " ns) = " +  x1.head._2.head._2 + "\t||   " + x2.head._1 + "    " +
        x2.head._2.head._1 + " (in " + d2 + " ns) = " +  x2.head._2.head._2 + "\n")
    }
    println("\n\n \tTriangular MFs (" + fbLeft._1 + ") \t|| \t\t\tGaussian MFs (" + fbRight._1 + ")")
    println("------------------------------------------------------------------------")
    println("  Rule Bloc\t |\t Output Name \t|Value      ||  Rule Block | Output Name |     Value")
    println(sb.toString())
  }
}
