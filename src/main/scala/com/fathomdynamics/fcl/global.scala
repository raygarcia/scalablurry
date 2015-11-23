package com.fathomdynamics.fcl

import com.typesafe.config.ConfigFactory
;

/**
  * Created by Raymond Garcia, Ph.D. (ray@fathomdynamics.com) on 11/22/15.
  * The MIT License (MIT)
  * <p>
  * Copyright (c) 2015 Raymond Garcia
  * <p>
  * Permission is hereby granted, free of charge, to any person obtaining a copy
  * of this software and associated documentation files (the "Software"), to deal
  * in the Software without restriction, including without limitation the rights
  * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  * copies of the Software, and to permit persons to whom the Software is
  * furnished to do so, subject to the following conditions:
  * <p>
  * The above copyright notice and this permission notice shall be included in all
  * copies or substantial portions of the Software.
  * <p>
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  * SOFTWARE.
  */

object GlobalConfig {
  private val config =  ConfigFactory.load().getConfig("parser")

  // custom membership functions
  object EmfConfig {
    private val emfConfig = config.getConfig("emfMappings")
    val estimationPoints = emfConfig.getInt("numOfEstimationPoints")
    val triangular = emfConfig.getString("Triangular") // triangular min mid max
    val trapetzoidal = emfConfig.getString("Trapetzoidal") // # Trapetzoidal: trapetzoidal min midLow midHigh max
    object Gaussian{
      private val gausConfig = emfConfig.getConfig("Gaussian")
      val token = gausConfig.getString("token")
      val sdCount = gausConfig.getDouble("sdCount")
    }  // # Gaussian: gaussian mean stdev
    val generalizedBell = emfConfig.getString("Generalized_Bell") //# Generalized bell: generalizedBell a b mean
    val sigmoidal = emfConfig.getString("Sigmoidal") //# Sigmoidal: sigmoidal gain center
  }

  object CommentConfig {
    private val comment = config.getConfig("fclComment")
    lazy val singleLineToken = comment.getString("singleLine")
    lazy val multiLineBeginToken = comment.getString("multiLineBegin")
    lazy val multiLineEndToken = comment.getString("multiLineEnd")
  }
}
