#201967cc8d6a85b5befe76602ccbb86bd55df57f
# simple recursive decent parser for the SL-LEX format

#Class to represent a token 
class Token:
    
    #constructor mehtod
    def __init__(self, line, col, token, lexeme):
        self.type = token.upper()
        self.value = lexeme
        self.lineno = line
        self.lexpos = col

    #python calls the repr method to print something out
    def __repr__(self):
        return "Token({},{},{},{})".format(self.lineno, self.lexpos, self.type, self.value)
    
# Class to read SL-LEX file
class TokenReader:
    def __init__(self, f):
        """
        f - file-like object containing SL-LEx data
        """

        #create a list of all the lines in the file
        lines = f.readlines()    #takes in and reads every line and returns a list of strings 

        #member variable to track the tokens 
        #essentially makes this variable private
        self.__tokens = []

        # create member variables for line and column postions
        self.lineno = 0
        self.lexpos = 0

        #simple while loop to read the tokens
        i = 0
        while i < len(lines):
            #first we read a line number
            lineno = int(lines[i])
            #then we read in our col number
            colno = int(lines[i+1])
            #now the token type
            #python doesn't strip newlines
            tok_type = lines[i+2].strip()

            if tok_type in ["string", "int", "ident"]:
                lexeme = lines[i+3].replace("\n", "")
                #increment it by 1 due to the lexme hence why it is not four down below 
                i += 1
            else:
                lexeme = tok_type

            self.__tokens.append(Token(lineno, colno, tok_type, lexeme))

            #increment i by 3 bc of the line col and token type
            i += 3
        #debug check for having read everything
        assert(i == len(lines))

        #turn the token list into an iterator to grab the next token 
        self.token_stream = iter(self.__tokens)
        
    #method to grab the next token or None if we have read them all 
    def token(self):
        # get next token
        tmp = next(self.token_stream, None)

        # tmp will be None if we have read all tokens
        if tmp is not None:
            # update info for lexer
            self.lineno = tmp.lineno
            self.lexpos = tmp.lexpos

        # return the token
        return tmp

 #main program to try this out 
if __name__ == "__main__":
    import sys
    tokens = TokenReader(sys.stdin)

    t = tokens.token()
    while t is not None:
        print(t)
        t = tokens.token()