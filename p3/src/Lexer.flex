/*
 * JFlex spec for a snail lexer
 *
 */

 //section one ... user code that is copied verbatim into the top of the generated java file

%%

 //section two ... options and declare regex constants
%class Lexer       //name of the class to produce from this spec
%unicode           //supported character set to be scanned
%line              //turn on line number tracking
%column            //turn on column number tracking
%type Token        //specify the return types of tokens

%{
 //code here gets copied into the top of the Lexer class
    private Token token(Tok type){
        return new Token(type, yyline+1, yycolumn+1);
    }

    private Token token(Tok type, Object value){
        return new Token(type, yyline+1, yycolumn+1, value);
    }

    private int commentCount;
    private String currentString;
    private int stringCol;
%}

//TODO 
//macro/constant regex for different token types
class          = ([c]|[C])([l]|[L])([a]|[A])([s]|[S])([s]|[S])
else           = ([e]|[E])([l]|[L])([s]|[S])([e]|[E])
if             = ([i]|[I])([f]|[F])
isvoid         = ([i]|[I])([s]|[S])([v]|[V])([o]|[O])([i]|[I])([d]|[D])
let            = ([l]|[L])([e]|[E])([t]|[T])
new            = ([n]|[N])([e]|[E])([w]|[W])
while          = ([w]|[W])([h]|[H])([i]|[I])([l]|[L])([e]|[E])
true           = ([t]|[T])([r]|[R])([u]|[U])([e]|[E])
false          = ([f]|[F])([a]|[A])([l]|[L])([s]|[S])([e]|[E])


Identifier     = [\p{XID_Start}][\p{XID_Continue}]*    //This makes sure that all special characters and capital and non 
//capital leters are accepted within the Identifier 
IntegerLiteral = [0-9]+                   
Whitespace     = [ \p{White_Space}]   //or it is [\p{Z}\s]  //TODO make a test file that uses wierd whitespace
//StringLiteral  = ([\"]|[\\])([^\0\r\n])([\"]|[\\])

%state COMMENT1
%state COMMENT2
%state STRING

%%
//section three ... lexical anaysis rules
<YYINITIAL> {
    "/*"        { 
                System.err.println("switching to comment state"); 
                yybegin(COMMENT1); 
                commentCount = 1;
                }

    "//"        { 
                System.err.println("switching to comment state"); 
                yybegin(COMMENT2); 
                }

    "+"        {return token(Tok.PLUS);}
    "-"        {return token(Tok.MINUS);}
    "/"        {return token(Tok.DIVIDE);}
    "*"        {return token(Tok.TIMES);}
    "("        {return token(Tok.LPAREN);}
    ")"        {return token(Tok.RPAREN);}
    ","        {return token(Tok.COMMA);}
    "@"        {return token(Tok.AT);}   
    "=="       {return token(Tok.EQUALS);}      
    "="        {return token(Tok.ASSIGN);} 
    "."        {return token(Tok.DOT);}
    ":"        {return token(Tok.COLON);}
    "{"        {return token(Tok.LBRACE);}
    "}"        {return token(Tok.RBRACE);}
    "["        {return token(Tok.LBRACKET);}
    "]"        {return token(Tok.RBRACKET);}
    "<="       {return token(Tok.LTE);}
    "<"        {return token(Tok.LT);}
    "!"        {return token(Tok.NOT);}
    ";"        {return token(Tok.SEMI);}
    "~"        {return token(Tok.UMINUS);}
    [\"]       {yybegin(STRING); currentString = ""; stringCol = yycolumn + 2;}
    

     //yytext() returns the matched lexeme
     {class}            {return token(Tok.CLASS);}
     {else}             {return token(Tok.ELSE);}
     {if}               {return token(Tok.IF);}
     {isvoid}           {return token(Tok.ISVOID);}
     {let}              {return token(Tok.LET);}
     {new}              {return token(Tok.NEW);}
     {while}            {return token(Tok.WHILE);}
     {true}             {return token(Tok.TRUE);}
     {false}            {return token(Tok.FALSE);}

     {Identifier}       {return token(Tok.IDENT, yytext());}
     
     {IntegerLiteral}   {try{
                                {return token(Tok.INT, Long.parseLong(yytext()));}
                        }catch (NumberFormatException e){
                                throw new Error("ERROR: " + (yyline+1) + ":" + (yycolumn+1) + ": " + "Lexer: integer format not valid" + "'" + yytext() + "'");}
                            }

     //{StringLiteral}    {return token(Tok.STRING, yytext());}
     {Whitespace}       {/*ignore*/}

}

<COMMENT1> {
    "*/"        { 
                commentCount--;
                    if (commentCount == 0){
                    System.err.println("switching to YYINITIAL state"); 
                    yybegin(YYINITIAL); 
                    }
                }

    "/*"        { commentCount++;}

     <<EOF>>    {throw new Error("ERROR: " + (yyline+1) + ":" + (yycolumn+1) + ": " + "Lexer: end of file was within a block comment");}

    [^]         { /* ignore */ }
}

<COMMENT2> {
    \n      {System.err.println("switching to YYINITIAL state"); 
                    yybegin(YYINITIAL);}

    [^\n]    {/* ignore */}
}

<STRING> {
    [\"]        {yybegin(YYINITIAL); return new Token(Tok.STRING, (yyline+1), stringCol, currentString);}
    "\\\""      {currentString += yytext();}
    "\\\\"      {currentString += yytext();}
    [^\0\r\n]   {currentString += yytext();}

    [^]         {throw new Error("ERROR: " + (yyline+1) + ":" + (yycolumn+1) + ": " + "Lexer: invalid character in your string: ");}
}

//catch all pattern for if we don't match throw an error
//format for 
//ERROR: 1:23: Lexer: invalid character: '\'
[^]               {throw new Error("ERROR: " + (yyline+1) + ":" + (yycolumn+1) + ": " + "Lexer: invalid character: " + "'" + yytext() + "'");}
