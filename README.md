
This repository contains the code and data related to the StarSEM2019 paper: "Bayesian Inference Semantics: A Modelling System and A Test Suite", by Bernardy, Blanck, Chatzikyriakidis, Lappin, and Maskharashvili.


## Getting started
 To parse sentences go to folder parser and run:
```
./parsesuite.sh
```
Run the following command ti generate Haskell files/code of the problems:
```
./generate.sh
```

It will generate Haskell files for the examples of the test suite. Those files will be
located in the folder named testsuite.

Run the following command to translate Haskell files into WebPPL programs and run them:
 ```
./runmodel.sh ProblemN.hs
```

where ProblemN is the N-th example of the test suite (subtitute N by a number of an example).
