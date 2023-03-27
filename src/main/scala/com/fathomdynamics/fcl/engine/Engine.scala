package com.fathomdynamics.fcl.engine

import com.fathomdynamics.fcl.FclParser

import scala.io.Source

/**
 * Created by Raymond Garcia, Ph.D. (ray@fathomdynamics.com) on 10/10/2015.
 The MIT License (MIT)

Copyright (c) 2015-2023 Raymond Garcia

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
trait FclEngine extends FclParser{
 // def run(input:Stream)
// val compileOutput = parseAll(funcBlock, stripLineComments(stripBlockComments(fclFile)))
 def compile(fileName: String) ={
  val fclFile = Source.fromFile(fileName)
  val fclFileStr = fclFile.mkString
  val compileOutput = parseAll(funcBlocks, stripLineComments(stripBlockComments(fclFileStr)))

  fclFile.close()
  // THE OUTPUT IS A NESTED MAP called funcBlockDefs
  // There can be multiple function blocks,
  // there can be multiple rule blocks for every function block,
  // and finally, there can be multiple outputs for each rule block.  Hence,
  // -- Map[Func Block Name, Map[Rule Block Name, Map[Output, numerical Val]]]

  // fb._1 is the name of the function block
  // fb._2 is the functionBlock object
   funcBlockDefs
 }
}
case class Engine() extends FclEngine{

}
