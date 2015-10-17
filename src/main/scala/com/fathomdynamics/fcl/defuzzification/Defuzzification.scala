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

package com.fathomdynamics.fcl.defuzzification

import com.fathomdynamics.fcl.util.{Validators, Utils}

trait Defuzzification extends Utils with Validators{
  /* cog - Centre of Gravity (Note 1)
  * - Centre of Gravity is equivalent to Centroid of Area
  * */
  val defuzRanges = Map[String, Point]()

/*
  def cog

  /* COGS Centre of Gravity for Singletons */
  def cogs

  /*COA Centre of Area (Notes 2 and 3)
  * - Centre of Area is equvalent to Bisector of Area
  * - COA is not applicable if singletons are used.
*/
  def coa


  /*LM Left Most Maximum*/
  def lm

  /*RM Right Most Maximum*/
  def rm
*/

  /*
DEFUZZIFY variable_name
  RANGE(min..max);
  TERM term_name:= membership_function;
  defuzzification_method;
  default_value;
END_DEFUZZIFY
 */

  case class DefuzzifyBlock(name: String, range: List[Double], mixDecls: List[Tuple2[String, Any]], defuzMethod: String){

    val membershipFunctions = Map[String, (Double)=>Double]()

    mixDecls.foreach(x =>{ x._2 match{
      //this will either be a singleton Tuple2[String, Double] or membership func Tuple2[String, List[Point]]
      // adapting to type erasure with an explicit downcast since there are only two cases

      case singletonFuncVal : Double => {membershipFunctions + x._1 -> getSingletonFunc(singletonFuncVal); println("Singleton")}
      case membershipFuncPoints : Any  => {membershipFunctions + x._1 -> getFuzzifier(membershipFuncPoints.asInstanceOf[List[Point]]); println("Regular membership function")}
    }})

    def getSingletonFunc(funcPoint: Double) = (inVal:Double) =>{if (inVal == funcPoint) 1 else 0}
  }
}
