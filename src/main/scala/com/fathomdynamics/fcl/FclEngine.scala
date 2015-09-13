package com.fathomdynamics.fcl

import scala.collection.mutable

import scala.util.parsing.combinator.JavaTokenParsers
/*
FUNCTION_BLOCK
VAR_INPUT
  humidity : REAL;
  color : color_type
END_VAR

VAR_INPUT
    <variable name> REAL; (* RANGE(<variable minimum value> .. <variable maximum value>) *)
END_VAR

FUZZIFY <variable name>
    TERM <term (or set) name> := <points that make up the term> ;
END_FUZZIFY
DEFUZZIFY valve
    METHOD: <defuzzification method>;
END_DEFUZZIFY
RULEBLOCK <ruleblock name>
    <operator>:<algorithm>;
    ACCUM:<accumulation method>;
    RULE <rule number>: IF <condition> THEN <conclusion>;
END_RULEBLOCK

END_FUNCTION_BLOCK
 */
class FclEngine extends JavaTokenParsers {
  // literals
  def eol : Parser[Any] = """[^\r\n]+""".r
  def varType : Parser[Any] = "REAL" | "INT"
  def varName : Parser[Any] = """[^-\s\t]+""".r
  def decl : Parser[Any] = varName~":"~varType~opt(";")
  def varInput : Parser[Any] = "VAR_INPUT"~rep(decl)~"END_VAR"

    println("Hello, world!")
}

