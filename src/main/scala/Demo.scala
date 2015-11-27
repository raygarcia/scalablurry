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

import scala.collection.mutable.ListBuffer
import scala.io.Source

object TheApp extends FclParser {

  def main(args: Array[String]) {
    val fclFile = Source.fromFile("tipper.fcl").mkString
    println("------ Processing the tipper.fcl demo... ------")
    println(fclFile)
    println("-----------------------------------------------")
    val compileOutput = parseAll(funcBlocks, stripLineComments(stripBlockComments(fclFile)))
    println(compileOutput.toString)
    val out = ListBuffer[Map[String, Double]]();
    val in = (0.0 to 5.0 by 0.5).map(v => List(v,v))
    funcBlockDefs.foreach(fb => {
      fb._2.plot
      for (i <-in){
        val x = fb._2.eval(i)
        out ++= (x).map(o => o._2)
      }
    })
    (in zip out).foreach(pair => println("in: " + pair._1 + ", out: " + pair._2))
  }
}
