package com.fathomdynamics.fcl.fuzzification

import com.fathomdynamics.fcl.GlobalConfig
import com.fathomdynamics.fcl.util.{Validators, Utils}
import com.typesafe.scalalogging.Logger
import org.jfree.data.xy.{XYSeriesCollection, XYSeries}
import org.slf4j.LoggerFactory

import scalax.chart.module.ChartFactories._ //XYLineChart

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
  val logger = Logger(LoggerFactory.getLogger("Fuzzification"))

  case class FuzzifyBlock(inputName : String, memberFuncs : List[(String, List[Point])]){
    checkInDecls(inputName)
    val fuzzifierMap = memberFuncs.map(func =>(func._1 -> getFuzzifier(func._2))).toMap
    val fuzzyRanges = memberFuncs.map(func => (func._1 -> List(func._2.head.x.asInstanceOf[Double], func._2.last.x.asInstanceOf[Double]))).toMap

    logger.debug("fuzzyRanges: " + fuzzyRanges.toString())

    def plot = {
      val range = fuzzyRanges.map(func => func._2).flatten.map(x=>(x,x)).reduceLeft ( (x,y) => (x._1 min y._1,x._2 max y._2) )
      val dataset = new XYSeriesCollection()
      fuzzifierMap.map(func => {

        val inc = (range._2 - range._1)/GlobalConfig.PlotConfig.plotPoints
        val series = new XYSeries(func._1)
        for (i <- range._1 to range._2 by inc) yield series.add(i,func._2(i))
        series
      }).foreach(ds => dataset.addSeries(ds))
      chartIt(dataset)
    }
    def chartIt(dataSet:XYSeriesCollection) = {
      val chart = XYAreaChart(dataSet,title = inputName).toFrame()
      chart.pack();
      chart.visible = true;
    }
  }

}
