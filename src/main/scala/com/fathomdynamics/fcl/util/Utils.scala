package com.fathomdynamics.fcl.util

import javax.swing.{JPanel, JFrame}

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
  val inputStrm = Map[String,List[Any]]()
  def simplePlot(label:String, range:List[Double],func: (Double) => Double){}

/*  def simplePlot(label:String, range:List[Double],func: (Double) => Double){
    val scope = (range.head to range.last by 1.0)
/*
    for (i <- scope) yield {
      println("(" + i + ", " + func(i) + ")")
    }
*/
    val data = for (i <- scope) yield (i,func(i))
    val chart = XYLineChart(data,title = label).toFrame()

    //Schedule a job for the event-dispatching thread:
    //creating and showing this application's GUI.
    new Thread{
      override def run: Unit ={
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
          def run ={
            //Create and set up the window.
            //Display the window.
            chart.pack();
            chart.visible = true;
          }
        })
      }
    }.start();
  }
*/
    case class Point(xPos:Any, yPos: Any){
    val x = xPos match {
      case xVal:Double => xVal
      case inputVar:String => ()=>{inputStrm.get(inputVar)}
    }
    val y = yPos match {
      case yVal:Double => yVal
      case inputVar:String => ()=>{inputStrm.get(inputVar)}
    }
  }
  def getFuzzifier(funcPoints: List[Point]):(Double)=>Double = (inVal:Double) =>{
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

  def readTextFile(filename: String): Try[List[String]] = {
    Try(Source.fromFile(filename).getLines.toList)
  }

  def input(inputFile:String)={
    readTextFile(inputFile) match {
      case Success(lines) => lines
      case Failure(f) => println(f)
    }
  }

}
