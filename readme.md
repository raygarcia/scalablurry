<head>
    <script type="text/javascript"
            src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
    </script>
</head>

an example MathJax inline rendering $\\( 1/x^{2} \\)$, and here is a block rendering: 
$\\[ \frac{1}{n^{2}} \\]$

# Scalablurry
ScalaBlurry is a scala-implemented fuzzy logic library that also supports the Fuzzy Control Language (IEC 61131-7)

*Current version:* 1.0.0.0

# Table of Contents

1. [Building from Source](#building)
1. [Usage](#usage)
1. [An example](#example)
    1. [Command-line Details](#cli)
    1. [The FCL File](#fcl)
    1. [Results](#results)
1. [ToDo](#todo)
1. [wiki](https://github.com/raygarcia/scalablurry/wiki)

![equation](http://www.sciweavers.org/tex2img.php?eq=1%2Bsin%28mc%5E2%29&bc=White&fc=Black&im=jpg&fs=12&ff=arev&edit=)
![equation](http://latex.codecogs.com/gif.latex?Concentration%3D%5Cfrac%7BTotalTemplate%7D%7BTotalVolume%7D)  

<a name="building"></a>
## How to Clean/Test/Compile/Run from Source
This project uses [Typesafe's Activator](https://www.typesafe.com/activator/docs) so to clean, compile, test, and run use the following commands from the project directory:

### Single command option:
    activator clean
         
### Combined command option:
    activator clean compile test run

### How to create an executable jar (based on the assembly plug-in)
    activator assembly

<a name="usage"></a>
## Usage
#### Running without any arguments will produce the usage message...

``` markdown
Usage (Please don't forget the quotes!):
    java -jar scalablurry.jar ... or activator "run ..."  where "..." = [OPTIONS] <FCL file>

OPTIONS

--inFile <file>         Formatted as a b c; where each ";" ends a row
                        (experiment). These values are appended if input values
                        are specified on the command line.

--demo                  Multi-funcblock Tipper Demo.

-i [a b c; d e f; ...]  n x m Matrix where n = number of experiments and m =
                        number of inputs.

-o <file>               output file to store the results.

PARAMETERS

<FCL file>  Input FCL File
```
<a name="example"></a>
# An Example
#### The following example is of the use of Scalablurry as an executable jar or through source using typesafe's activator

<a name="cli"></a>
## Command-line Details

The command below specifies the following:
  * Input values of 3 sets for a two-input system
  * An input file containing addtional values that will be appended to the values on the command line
  * An output file to write the results to

#### ./activator "run -i [5 5; 1 1; 0 0] --inFile examples/scalablurry/tipperInput -o output  examples/scalablurry/tipper.fcl"

<a name="fcl"></a>
## The FCL File
Take note of the following:

  * There are two function blocks
      - each function block has two rule blocks
  * Several types of membership functions are used:
      - linear piecewise in terms of points
      - triangular
      - guassian
      - generalized bell
  * Multiple accumulation, activation, and logical operators

``` java
FUNCTION_BLOCK tipper	// Block definition (there may be more than one block per file)

VAR_INPUT				// Define input variables
	service : REAL;
	food : REAL;
END_VAR

VAR_OUTPUT				// Define output variable
	tip : REAL;
END_VAR

FUZZIFY service			// Fuzzify input variable 'service': {'poor', 'good' , 'excellent'}
	TERM poor := (0, 1) (4, 0) ;
	TERM good := generalizedBell 1 1 5 //trapetzoidal 1 4 6 9//sigmoidal 1 5 // // // gaussian 5 2 //  // instead of (1, 0) (4,1) (6,1) (9,0);
	TERM excellent := (6, 0) (9, 1);
END_FUZZIFY

FUZZIFY food			// Fuzzify input variable 'food': { 'rancid', 'delicious' }
	TERM rancid := triangular 0 1 3 // instead of (0, 1) (1, 1) (3,0) ;
	TERM delicious := (7,0) (9,1);
END_FUZZIFY

DEFUZZIFY tip			// Defzzzify output variable 'tip' : {'cheap', 'average', 'generous' }
    RANGE := (0 .. 30);
	TERM cheap := triangular 0 5 10 // instead of (0,0) (5,1) (10,0);
	TERM average := triangular 10 15 20 // instead of (10,0) (15,1) (20,0);
	TERM generous := triangular 20 25 30 // instead of (20,0) (25,1) (30,0);
	METHOD : CoG;		// Use 'Center Of Gravity' defuzzification method
	DEFAULT := 0;		// Default value is 0 (if no rule activates defuzzifier)
END_DEFUZZIFY

RULEBLOCK No1
	AND : MIN;			// Use 'min' for 'and' (also implicit use 'max' for 'or' to fulfill DeMorgan's Law)
	ACT : MIN;			// Use 'min' activation method
	ACCU : MAX;			// Use 'max' accumulation method

	RULE 1 : IF service IS poor OR food IS rancid THEN tip IS cheap;
	RULE 2 : IF service IS good THEN tip IS average;
	RULE 3 : IF service IS excellent AND food IS delicious THEN tip IS generous;
END_RULEBLOCK

RULEBLOCK No2
	AND : PROD;
	ACT : PROD;
	ACCU : BSUM;

	RULE 1 : IF service IS poor OR food IS rancid THEN tip IS cheap;
	RULE 2 : IF service IS good THEN tip IS average;
	RULE 3 : IF service IS excellent AND food IS delicious THEN tip IS generous;
END_RULEBLOCK

END_FUNCTION_BLOCK

FUNCTION_BLOCK tipper2	// Block definition (all gaussians)
  VAR_INPUT				// Define input variables
    service : REAL;
    food : REAL;
  END_VAR

  VAR_OUTPUT				// Define output variable
    tip : REAL;
  END_VAR

  FUZZIFY service			// Fuzzify input variable 'service': {'poor', 'good' , 'excellent'}
    TERM poor := (0, 1) (4, 0) ;
    TERM good := gaussian 5 .5  // instead of (1, 0) (4,1) (6,1) (9,0);
    TERM excellent := (6, 0) (9, 1);
  END_FUZZIFY

  FUZZIFY food			// Fuzzify input variable 'food': { 'rancid', 'delicious' }
    TERM rancid := triangular 0 1 3 // instead of (0, 1) (1, 1) (3,0) ;
    TERM delicious := (7,0) (9,1);
  END_FUZZIFY

  DEFUZZIFY tip			// Defzzzify output variable 'tip' : {'cheap', 'average', 'generous' }
      RANGE := (0 .. 30);
    TERM cheap := gaussian 5 .5 // instead of (0,0) (5,1) (10,0);
    TERM average := gaussian 15 .5 // instead of (10,0) (15,1) (20,0);
    TERM generous := gaussian 25 .5 // instead of (20,0) (25,1) (30,0);
    METHOD : CoG;		// Use 'Center Of Gravity' defuzzification method
    DEFAULT := 0;		// Default value is 0 (if no rule activates defuzzifier)
  END_DEFUZZIFY

  RULEBLOCK No1
    AND : MIN;			// Use 'min' for 'and' (also implicit use 'max' for 'or' to fulfill DeMorgan's Law)
    ACT : MIN;			// Use 'min' activation method
    ACCU : MAX;			// Use 'max' accumulation method

    RULE 1 : IF service IS poor OR food IS rancid THEN tip IS cheap;
    RULE 2 : IF service IS good THEN tip IS average;
    RULE 3 : IF service IS excellent AND food IS delicious THEN tip IS generous;
  END_RULEBLOCK

    RULEBLOCK No2
        AND : PROD;
        ACT : PROD;
        ACCU : BSUM;

        RULE 1 : IF service IS poor OR food IS rancid THEN tip IS cheap;
        RULE 2 : IF service IS good THEN tip IS average;
        RULE 3 : IF service IS excellent AND food IS delicious THEN tip IS generous;
    END_RULEBLOCK

END_FUNCTION_BLOCK
```

<a name="results"></a>
## Results
Take note of the following:
* Each function block output type is Map[RuleBlockName, Map[OutputName, OutputValue]]
``` java
tipper(List(5.0, 5.0)) output: Map(No1 -> Map(tip -> 14.99999999999993), No2 -> Map(tip -> 15.000000000000032))
tipper2(List(5.0, 5.0)) output: Map(No1 -> Map(tip -> 14.999999999999485), No2 -> Map(tip -> 14.999999999997973))
tipper(List(1.0, 1.0)) output: Map(No1 -> Map(tip -> 6.048269804027887), No2 -> Map(tip -> 5.5693628269985584))
tipper2(List(1.0, 1.0)) output: Map(No1 -> Map(tip -> 7.1505247499523845), No2 -> Map(tip -> 7.219488841991899))
tipper(List(0.0, 0.0)) output: Map(No1 -> Map(tip -> 5.710078647276946), No2 -> Map(tip -> 5.375150867946634))
tipper2(List(0.0, 0.0)) output: Map(No1 -> Map(tip -> 7.1505247499523845), No2 -> Map(tip -> 7.219488841991899))
tipper(List(0.5, 0.5)) output: Map(No1 -> Map(tip -> 5.861125676579706), No2 -> Map(tip -> 5.482287539807647))
tipper2(List(0.5, 0.5)) output: Map(No1 -> Map(tip -> 7.1505247499523845), No2 -> Map(tip -> 7.224043386684748))
tipper(List(1.1, 1.76)) output: Map(No1 -> Map(tip -> 6.169791894973242), No2 -> Map(tip -> 5.65949568444081))
tipper2(List(1.1, 1.76)) output: Map(No1 -> Map(tip -> 7.1852081152265), No2 -> Map(tip -> 7.227457701915328))
```

***
## Basic FCL compliance exists.  Check the [wiki](https://github.com/raygarcia/scalablurry/wiki) for details.
***
<a name="todo"></a>
## TO DO (Check regularly for updates to this list):
- [x] Function Blocks
- [x] Fuzzification Blocks
- [x] Defuzzification Blocks
    - [x] CoG
    - [ ] LM
    - [ ] RM
    - [ ] CoA
    - [ ] CoGS
- [x] Input & Output Blocks
- [x] Rule Blocks
    - [x] Operator OR/AND Algorithms
        - [x] MAX/MIN
        - [x] ASUM/PROD
        - [X] BSUM/BDIF
    - [X] Activation Methods
        - [x] PROD
        - [X] MIN
    - [X] Accumulation Methods
        - [x] MAX
        - [X] BSUM
        - [ ] NSUM

Basic Membership Functions
- [x] Piecewise Linear based on points

Extended Membership Functions
- [x] Triangle
- [x] Trapetzoidal
- [x] Sigmoid
- [x] Gaussian
- [x] Generalized Bell
- [x] Singleton

Additional Membership Functions
- [ ] Custom Function
- [ ] ZShape,
- [ ] Ramp,
- [ ] Concave,
- [ ] Rectangle,
- [ ] Cosine,
- [ ] GaussianProduct,
- [ ] Spike,
- [ ] PiShape,
- [ ] SigmoidDifference,
- [ ] SigmoidProduct,
- [ ] SShape,
- [ ] Linear

Misc
- [ ] Insertion of '_' between constants
- [ ] Optional use of "Range"
- [ ] finer grained unit testing
- [ ] Shringing the size of the jar file produced due to dependencies