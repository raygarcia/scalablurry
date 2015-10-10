package com.fathomdynamics.fcl.fuzzification

import com.fathomdynamics.fcl.util.{Validators, Utils}

/**
 * Created by Raymond Garcia, Ph.D. (ray@fathomdynamics.com) on 9/13/2015.
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

trait Fuzzification extends Utils with Validators{
 case class FuzzifyBlock(inputName : String, memberFuncs : List[(String, List[Point])]){
  checkInDecls(inputName)

  val fuzzifierMap = memberFuncs.map(func =>(func._1 -> getFuzzifier(func._2))).toMap

  def getFuzzifier(funcPoints: List[Point]) = (inVal:Double) =>{
   // generate a list of points of all doubles
   val intervals = funcPoints.map(boundary=> {
    val List(xPos:Double, yPos:Double) = List(boundary.x, boundary.y).map(compo => {
     compo match {
      case numeric: Double => numeric
      case func: (() => Any) => func().asInstanceOf[Double]
     }});
    (xPos, yPos)
   }).sliding(2).toList

   val List(leftX:Double, leftY:Double, rightX:Double, rightY:Double) = List(intervals.head.head._1, intervals.head.head._2, intervals.last.last._1,intervals.last.last._2)

   if (inVal <= leftX) leftY
   else if (inVal >= rightX) rightY
   // only a single segment (tuple2, tuple2) should exist here is the membership function is properly defined
   else {
    val segment = intervals.filter (x=>x.head._1 <= inVal && x.last._1 > inVal).flatten

    segment.head._2 + ((segment.last._2 - segment.head._2)/(segment.last._1 - segment.head._1))*(inVal-segment.head._1)
   }
  }
 }

}
