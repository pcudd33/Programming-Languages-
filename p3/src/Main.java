import java.io.IOException;
import java.io.InputStreamReader;
import java.sql.SQLOutput;

public class Main {
    public static void main(String[] args) {
        Lexer l = new Lexer(new InputStreamReader(System.in));

        try {
            while (true) {
                Token t = l.yylex();
                if (t == null) {
                    //done reading tokens
                    break;
                } else {
                    System.out.println(t);
                }
            }
        } catch (IOException e){
            System.err.println(e.getLocalizedMessage());
        }catch (Error e) {
            System.out.println(e.getLocalizedMessage());
        }
    }


}