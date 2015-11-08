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

  case class DefuzzifyBlock(outputName: String, range: List[Double],
                            mixDecls: List[Tuple2[String, Any]], defuzMethod: String){
    val INTEGRATION_STEPS = 5000

    val membershipFunctions = Map[String, (Double)=>Double]()

    mixDecls.foreach(x =>{ x._2 match{
      //this will either be a singleton Tuple2[String, Double] or membership func Tuple2[String, List[Point]]
      // adapting to type erasure with an explicit downcast since there are only two cases

      case singletonFuncVal : Double => {membershipFunctions + x._1 -> getSingletonFunc(singletonFuncVal); println("Singleton")}
      case membershipFuncPoints : Any  => {membershipFunctions + x._1 -> getFuzzifier(membershipFuncPoints.asInstanceOf[List[Point]]); println("Regular membership function")}
    }})

    def getSingletonFunc(funcPoint: Double) = (inVal:Double) =>{if (inVal == funcPoint) 1 else 0}

    /*
        CoG - Centre of Gravity (note 1)
        CoGS - Centre of Gravity for Singletons
        CoA - Centre of Area (notes 2 and 3)
        LM - Left Most Maximum (note 4)
        RM - Right Most Maximum (note 4)
     */

    val defuzzify:((Double)=>Double) => Double = {
      defuzMethod match{
        case "CoG" => CoG
/*
        case "CoGS" =>
        case "CoA" =>
        case "LM" =>
        case "RM" =>
*/
      }
    }
    def CoG(func:(Double)=>Double):Double = {
      NumericalIntegration.integrate(func, range.head, range.last, INTEGRATION_STEPS, NumericalIntegration.simpson)
    }

    object NumericalIntegration {
      def leftRect(f:Double=>Double, a:Double, b:Double)=f(a)
      def midRect(f:Double=>Double, a:Double, b:Double)=f((a+b)/2)
      def rightRect(f:Double=>Double, a:Double, b:Double)=f(b)
      def trapezoid(f:Double=>Double, a:Double, b:Double)=(f(a)+f(b))/2
      def simpson(f:Double=>Double, a:Double, b:Double)=(f(a)+4*f((a+b)/2)+f(b))/6;

      def fn1(x:Double)=x*x*x
      def fn2(x:Double)=1/x
      def fn3(x:Double)=x

      type Method = (Double=>Double, Double, Double) => Double
      def integrate(f:Double=>Double, a:Double, b:Double, steps:Double, m:Method)={
        val delta:Double=(b-a)/steps
        delta*(a until b by delta).foldLeft(0.0)((s,x) => s+m(f, x, x+delta))
      }

      def print(f:Double=>Double, a:Double, b:Double, steps:Double)={
        println("rectangular left   : %f".format(integrate(f, a, b, steps, leftRect)))
        println("rectangular middle : %f".format(integrate(f, a, b, steps, midRect)))
        println("rectangular right  : %f".format(integrate(f, a, b, steps, rightRect)))
        println("trapezoid          : %f".format(integrate(f, a, b, steps, trapezoid)))
        println("simpson            : %f".format(integrate(f, a, b, steps, simpson)))
      }

      def main(args: Array[String]): Unit = {
        print(fn1, 0, 1, 100)
        println("------")
        print(fn2, 1, 100, 1000)
        println("------")
        print(fn3, 0, 5000, 5000000)
        println("------")
        print(fn3, 0, 6000, 6000000)
      }
    }
    /*
f(x) = x^3, where x is [0,1], with 100 approximations. The exact result is 1/4, or 0.25.
f(x) = 1/x, where x is [1,100], with 1,000 approximations. The exact result is the natural log of 100, or about 4.605170
f(x) = x, where x is [0,5000], with 5,000,000 approximations. The exact result is 12,500,000.
f(x) = x, where x is [0,6000], with 6,000,000 approximations. The exact result is 18,000,000.

rectangular left   : 0,245025
rectangular middle : 0,249988
rectangular right  : 0,255025
trapezoid          : 0,250025
simpson            : 0,250000
------
rectangular left   : 4,654991
rectangular middle : 4,604763
rectangular right  : 4,556981
trapezoid          : 4,605986
simpson            : 4,605170
------
rectangular left   : 12499997,500729
rectangular middle : 12500000,000729
rectangular right  : 12500002,500729
trapezoid          : 12500000,000729
simpson            : 12500000,000729
------
rectangular left   : 17999997,001390
rectangular middle : 18000000,001391
rectangular right  : 18000003,001390
trapezoid          : 18000000,001391
simpson            : 18000000,001391
     */
  }

}

