# Compatibility with FCL Files used with other Libraries

### One of the biggest confusions with using another fuzzzy logic library happens when FCL files do not compile in another library.  While each library may be compliant with the standard, it is the respective extensions that break compatibility between libraries.

#### For instance, the table below lists some features that fuzzy libraries do differently:

FEC Specififcation Feature                                                             | Scalablurry                         |  jFuzzyLogic (as of 11/15)                |             fuzzylite (as of 11/15)                 |
:------------------------------------------------------------------------------------- | :---------------------------------- | :---------------------------------------- | :-------------------------------------------------- |
**Comments** <br><p>Simply specifying whether or not they are supported is enough. | Configurable (defaults to "//" for single line and the pair "/* ", "*/ " for multi-line comments)  | <u>Single-line</u>: <br> // This is a single-line comment <br><br> <u>Multi-line</u>: <br>  /* this is a <br> multi-line comment */ |              |
**Defuzzification methods** <br><p>The standard identifies several defuzzification methods - specified exactly as: <br> <br>"RM", "LM", CoG", "CoA", "CoGS" | Configurable but supported as specified in the standard. |"COG" is used in that libraries tipper.fcl example (instead of "CoG") |
**Extended membership functions** <br><p>Compliance are in terms of a singleton or a piece-wise linear function that is described in terms of a series of points and extensions are custom functions. |Several "canned" functions are provided AND ALL ARE CONFIGURABLE (*name followed by syntax*):<br> * Triangular: triangular min mid max <br>* Trapetzoidal: trapetzoidal min midLow midHigh max <br>* Gaussian: gaussian mean stdev <br>* Generalized bell: generalizedBell a b mean <br>* Sigmoidal: sigmoidal gain center               | As listed in [1]:<br> * Triangular: trian min mid max <br>* Trapetzoidal: trape min midLow midHigh max <br>* Gauss: gauss mean stdev <br>* Generalized bell: gbell a b mean <br>* Sigmoidal: sigm gain center|              |

#### Because of these distinctions, FCL files used in one library will not necessarily work in another without some modification.  Aside from anything that clearly breaks the standard, Scalablurry will attempt to compile and run FCL files from other specific libraries using a compatibilty mode setting in Scalablurry's configuration.

#### [1] http://jfuzzylogic.sourceforge.net/html/manual.html#membership





<div class="footer">
Created by Raymond Garcia, Ph.D. (ray@fathomdynamics.com) on 10/10/2015.<br>
<p>The MIT License (MIT)<br>
Copyright (c) 2015 Raymond Garcia
</div>
