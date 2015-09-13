package com.fathomdynamics.fcl.defuzzification

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
trait Defuzzification {
  /* cog - Centre of Gravity (Note 1)
  * - Centre of Gravity is equivalent to Centroid of Area
  * */
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

}
