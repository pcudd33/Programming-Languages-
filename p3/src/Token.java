public class Token {


    //category of token
    private Tok tokType;
    //line and column location in the file
    private int line, col;
    //lexeme
    private Object value;

    public Token(Tok tokType, int line, int col){
        this.tokType = tokType;
        this.line = line;
        this.col = col;
    }

    public Token(Tok tokType, int line, int col, Object value){
        this(tokType, line, col);
        this.value = value;
    }

    @Override
    public String toString() {
        String toReturn =  this.line + "\n" + this.col + "\n" + this.tokType.toString().toLowerCase();
        if (value != null){
            toReturn +=  "\n" + value.toString();
        }

        return toReturn ;
    }
}
