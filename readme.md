# Scalablurry
ScalaBlurry is a scala-implemented fuzzy logic library that also supports the Fuzzy Control Language (IEC 61131-7)

## How to Clean/Test/Compile/Run from Source
This project uses [Typesafe's Activator](https://www.typesafe.com/activator/docs) so to clean, compile, test, and run use the following commands from the project directory:

### Single command option:
    activator clean
         
### Combined command option:
    activator clean compile test run

### How to create an executable jar (based on the assembly plug-in)
    activator assembly
***
## TO DO (Check regularly for updates to this list):
Basic FCL Compliance

- [x] Function Blocks
- [x] Fuzzification Blocks
-[x] Defuzzification Blocks
    -[x] CoG
    -[ ] LM
    -[ ] RM
    -[ ] CoA
    -[ ] CoGS
-[x] Input & Output Blocks
-[x] Rule Blocks
    -[x] Operator OR/AND Algorithms
        -[x] MAX/MIN
        -[x] ASUM/PROD
        -[X] BSUM/BDIF
    -[X] Activation Methods
        -[x] PROD
        -[X] MIN
    -[X] Accumulation Methods
        -[x] MAX
        -[X] BSUM
        -[ ] NSUM

Basic Membership Functions
-[x] Piecewise Linear based on points

Extended Membership Functions
-[x] Triangle
-[x] Trapetzoidal
-[x] Sigmoid
-[x] Gaussian
-[x] Generalized Bell
-[x] Singleton

Additional Membership Functions
-[ ] Custom Function
-[ ] ZShape,
-[ ] Ramp,
-[ ] Concave,
-[ ] Rectangle,
-[ ] Cosine,
-[ ] GaussianProduct,
-[ ] Spike,
-[ ] PiShape,
-[ ] SigmoidDifference,
-[ ] SigmoidProduct,
-[ ] SShape,
-[ ] Linear

Misc
-[ ] Insertion of '_' between constants
-[ ] Optional use of "Range"
-[ ] finer grained unit testing
-[ ] Shringing the size of the jar file produced due to dependencies