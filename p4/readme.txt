    Within our implementation, Caleb and I chose to start this project by looking through the snail syntax specification
and eventually grouping the deffinitions of expressions, classes, methods, and members into different sections to ensure we cover every
acceptable input. While writing the parser we wanted to define the support for all of the characteristics of each individual token type, keyword and operator.
We also used the PLY specification and documentation in order to account for associativity and precedence rules related to objects.
It was this part of the project that gave me and Caled greif when writing the definitions for all of them, this caused confusion between
the expression objects and the difference between them and the expression values. 
    It was only in comparing my own parser output to the snails built in output that 
I was able to debug and eventually completely correct my parser to meet the specification. While we were searching for bugs 
withtin the code for our test cases we found that creating small files that contained classes and functions and other things cotained within 
our definitiions and would alter then in a manner that could still allow them to lex but would cause errors witihn the parser. These problems 
could spur from many different places such as incorreclty setting up an array, naming an array something you cannot and many other things. 
The hardest part of the whole project was understanding how to properly define expressions, I went through multiple different stages of trying to 
define them before I divided them all up which allowed us to define all of the exoressions more easily. After we figured out how to properly define an expression
object and value and identifier object and value, this became clear and we got it to work. Once we figured out how to properly
implement all of these within the definitions in our parser we were able to finally build our complete AST generated from the test files.
