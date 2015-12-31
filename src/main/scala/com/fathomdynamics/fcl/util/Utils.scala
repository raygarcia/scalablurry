package com.fathomdynamics.fcl.util

import javax.swing.{JPanel, JFrame}

import com.fathomdynamics.fcl.GlobalConfig
import com.fathomdynamics.fcl.engine.FunctionBlockElements
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.immutable.NumericRange
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.swing.{GridPanel, Panel}
import scala.util.{Failure, Success, Try}
import com.quantifind.charts.highcharts._
import com.quantifind.charts.highcharts.Highchart._
import scalax.chart.api._
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

trait Utils {
  implicit def bool2int(b:Boolean) = if (b) 1 else 0
  implicit def int2bool(i:Short) = if (i == 1) true else false
  val perfLogger = Logger(LoggerFactory.getLogger("Performance"))

  val inputStrm = Map[String,List[Any]]()
  // def simplePlot(label:String, range:List[Double],func: (Double) => Double){}
  def simplePlot(label:String, range:List[Double],func: (Double) => Double){
    // range is either [min max] or [min max inc]
    // If inc is not provided, use the plotPoints set in config
    val scope = range.size match {
      case 2 => {
        val inc = (range.last - range.head)/GlobalConfig.PlotConfig.plotPoints
        range.head to range.last by inc
      }
      case 3 => range.head to range(1) by range.last
    }

    val data = for (i <- scope) yield (i,func(i))
    val chart = XYLineChart(data,title = label).toFrame()
    //Create and set up the window.
    //Display the window.
    chart.pack();
    chart.visible = true;

  }

  case class Point(xPos:Any, yPos: Any){
    def x()(implicit fbd : FunctionBlockElements#FuncBlockDef) = xPos match {
      case xVal:Double => xVal.asInstanceOf[Double]
      case inputVar:String => ()=>{fbd.inputs.get(inputVar)}
    }
    def y()(implicit fbd : FunctionBlockElements#FuncBlockDef) = yPos match {
      case yVal:Double => yVal.asInstanceOf[Double]
      case inputVar:String => ()=>{fbd.inputs.get(inputVar)}
    }
  }
  def getFuzzifier(funcPoints: List[Point])(implicit fbd : FunctionBlockElements#FuncBlockDef):(Double)=>Double = (inVal:Double) =>{
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
    // only a single segment (tuple2, tuple2) should exist here if the membership function is properly defined
    else {
      val segment = intervals.filter (x=>x.head._1 <= inVal && x.last._1 > inVal).flatten

      segment.head._2 + ((segment.last._2 - segment.head._2)/(segment.last._1 - segment.head._1))*(inVal-segment.head._1)
    }
  }

  def readTextFile(filename: String): Try[List[String]] = {
    Try(Source.fromFile(filename).getLines.toList)
  }

  def input(inputFile:String)={
    readTextFile(inputFile) match {
      case Success(lines) => lines
      case Failure(f) => println(f)
    }
  }

  def gaussianListPoints(mean:Double, sigma:Double) =
  {
    val pList = ListBuffer[Point]()
    val gRange = sigma*GlobalConfig.EmfConfig.Gaussian.sdCount
    val lBound:Double = mean - gRange
    val uBound:Double = mean + gRange
    val norm = 1 / (sigma * Math.sqrt(2 * Math.PI));
    val  is = 1 / sigma;
    val i2s2 = 0.5 * is * is;

    (lBound to uBound by (uBound - lBound)/GlobalConfig.EmfConfig.estimationPoints).foreach{x =>
      val xMinusMean = x - mean;
      pList += Point(x, norm * Math.exp(-xMinusMean * xMinusMean * i2s2));
    }
    pList.toList
  }
  def genBellListPoints(divisor:Double, exponent:Double, center:Double) =
  {
    val pList = ListBuffer[Point]()
    val limitPrecision = GlobalConfig.EmfConfig.GeneralizedBell.lPrecision
    val root = divisor*Math.pow(1/limitPrecision-1, 1/(2*exponent))
    val lBound:Double = -root + center
    val uBound:Double = root + center

    (lBound to uBound by (uBound - lBound)/GlobalConfig.EmfConfig.estimationPoints).foreach{x =>
      pList += Point(x, 1/(1 + Math.pow((x - center)/divisor, 2*exponent)));
    }

    pList.toList
  }
  def sigmoidalListPoints(gain:Double, center:Double) =
  {
    val pList = ListBuffer[Point]()
    val limitPrecision = GlobalConfig.EmfConfig.Sigmoidal.lPrecision
    val yMin = limitPrecision
    val yMax = 1 - limitPrecision
    val lBound:Double = center - (Math.log(1/yMin -1)/gain)
    val uBound:Double = center - (Math.log(1/yMax -1)/gain)

    (lBound to uBound by (uBound - lBound)/GlobalConfig.EmfConfig.estimationPoints).foreach{x =>
      pList += Point(x, 1/(1 + Math.exp(-gain*(x - center))));
    }

    pList.toList
  }
}
