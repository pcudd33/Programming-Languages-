    Within p3 I have created multiple java files which do different tasks in order to create my final class lexer.flex.
In the first file that I created called Tok.java I was created a method that would outline all of the Identifiers,
operators, and characters I can use within the langueage of snail. This class is used in assinging meaning to all of these operators
and Identifiers and some characters later on within the Lexer.flex file. Within my Main.java this class is where the 
actual lexing is taking place and this is what prints out all the tokens and allows us to iterate through all of them 
and allows us to know when we have read in all of our tokens. This file also accounts for some of our error testing. Within the 
Token.java class this initializes all of the values for tokType, column, and line number. This file also create the token type constructor itself
which allows us later within the Lexer.flex to create new tokens. This file also overrides the toString method 
which allows for case insesativity for all of our Identifiers due to converting all of it to lowercase no matter what.

    within The file Lexer.flex this is where we created our tokens witch are what we are printing out in a list that is generated throughout this file.
After we created our token type that is our final return type we created some private variables that are used in order to construst our
string literals correctly. After that I created the valid regex for all of my token types due to them being case insesative. After that this file 
outlines the correct regex for all of the Identifiers, Integer Literals, and whitespace. After that the Lexer creates three different states
which allow for nested comments, different types of comments and string literals these will be called upon later within the code when these cases need to be handled.
Next within the Lexer I am creating all of the accepted operators and characters within snail, the same ones that are all outlined within the Tok class. 
Now I am creating all of the Identifiers and key words that will ne needed in order for this to match with everything that snail can do. 
After all this has happened the list of tokens that have been returned should include all of the tokens that were read in and will terminate leaving you with a fully lexed file that is ready for parseing.

    within our test cases we were thinking about ways to implement things that would be improper witch lead us to come up with a series of 
sucessful buggy test files. The main thinking that lead to these files was opening states and comments without closeing them,
improperly defining Identifiers and or keywords, and incorrectly defining whitespace and characters. All of these problems 
when implemented within snail allowed for use to catch all of the bugs within our test cases. Personally I would say 
the most difficult aspect of this project was trying to think of ways to construct test cases in order to find bugs 
within the buggy implementations of the lexer class. This and handling all of the errors within our own lexer was hard just becuase 
the error messages themselves were giving me a lot of greif.

 