# Welcome!

This is the home for my solutions for [Advent of Code](https://adventofcode.com) challenge
using [Scala](https://www.scala-lang.org)

### Approach

I'm using functional approach for each solution. Which means:

1. avoiding `null`
1. pure functions
1. immutable values
1. recursion\tail-recursion
1. functional data structures
1. functional control flow (no for-loops, no while-loops)
1. whenever I use `if` there is always `else`

### Tools

I'm using [IntelliJ IDEA](https://www.jetbrains.com/idea/) running scala worksheets

# Repository structure

```
scala/
+--build.sbt                 The main sbt build definition
+--project/                  The rest of the sbt build
+--src/main/                 
   +---/resources
       +---advent2015        Input files for 2015 challenge 
       +---advent2020        Input files for 2020 challenge
   +---/advent2015           Source code for 2015 challenge
   +---/advent2020           Source code for 2020 challenge    
```                    
