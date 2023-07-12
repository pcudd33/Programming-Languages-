# 201967cc8d6a85b5befe76602ccbb86bd55df57f
 
# program ::= (class)+
# class   ::= 'class' 'ident' (':' 'ident')? '{' (feature)* '}' ';'
 
tokens = (
    'DOT',
    'AT',
    'UMINUS',
    'LBRACKET',
    'TIMES',
    'DIVIDE',
    'PLUS',
    'MINUS',
    'LT',
    'LTE',
    'EQUALS',
    'NOT',
    'ASSIGN',
    'LPAREN',
    'RPAREN',
    'COMMA',
    'IDENT',
    'INT',
    'STRING',
    'CLASS',
    'COLON',
    'ELSE',
    'FALSE',
    'IF',
    'ISVOID',
    'LBRACE',
    'LET',
    'NEW',
    'RBRACE',
    'RBRACKET',
    'SEMI',
    'TRUE',
    'WHILE'
)
 
precedence = (
    ('nonassoc', 'ASSIGN'),
    ('nonassoc', 'NOT'),
    ('nonassoc', 'LT', 'LTE', 'EQUALS'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('left', 'LBRACKET'),
    ('nonassoc', 'UMINUS'),
    ('nonassoc', 'AT'),
    ('nonassoc', 'DOT')
)
 
# program ::= class_list
def p_program(p):
    """
    program ::= class_list
    """
    p[0] = p[1]
 
# classlist ::= class class_list
#           |   class
 
def p_class_list(p):
    """
    class_list ::= class class_list
              |    class
    """
    # Build up the list of classes
    if (len(p) == 2):
        p[0] = [p[1]] # Just a class on it's own
    else:
        p[0] = [p[1]] + p[2] # Class and class list
 
# class ::= 'class' 'ident' (':' 'ident')? '{' (feature)* '}' ';'
def p_class(p):
    """
    class ::= CLASS IDENT LBRACE feature_list RBRACE SEMI
          |   CLASS IDENT COLON IDENT LBRACE feature_list RBRACE SEMI
    """
    p[0] = {
        "class_name": p[2]
    }
 
    if p[2] in ["Array", "Bool", "Int", "IO", "String", "Object"]:
        print("ERROR: " + str(p.lineno(2)) + ":" + str(p.lexpos(2)) + ": Parser: invalid class name")
        exit(0)
    if (len(p) == 7):
        # No inheritance
        features = p[4]
    else:
        # Inheritance
        p[0]["inherits"] = p[4]
        features = p[6]
        if (p[4] in ["Array", "Bool", "Int", "String"]):
            print("ERROR: " + str(p.lineno(3)) + ":" + str(p.lexpos(3)) + ": Parser: invalid inheritance class name")
            exit(0)
     
    # Filter out just the methods and just the members
    # List comprehension in python
    p[0]["members"] = [f for f in features if f["type"] == "member"]
    p[0]["methods"] = [f for f in features if f["type"] == "method"]
 
# feature_list ::= (feature)*
# feature_list ::= empty_list
#              |   nonempty_feature_list
# nonempty_feature_list ::= feature nonempty_feature_list
#                       |   feature
 
def p_feature_list(p):
    """
    feature_list ::= empty_list
                 |   nonempty_feature_list
    """
    p[0] = p[1]
 
def p_nonempty_feature_list(p):
    """
    nonempty_feature_list ::= feature nonempty_feature_list
                          |   feature
    """
    if (len(p) == 2):
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]
 
def p_empty_list(p):
    """
    empty_list ::=
    """
    p[0] = []
 
# Member variables
 
def p_feature_member_noinit(p): # Uninitialized Member Variable
    """
    feature ::= LET IDENT SEMI
    """
    if (p[2] in ["self"]):
        print("ERROR: " + str(p.lineno(2)) + ":" + str(p.lexpos(2)) + ": Parser: cannot name member variable 'self'")
        exit(0)

    p[0] = {
        "type": "member",
        "name": {
            "line": p.lineno(2),
            "col": p.lexpos(2),
            "value": p[2]
        }
    }
    
 
def p_feature_member_init(p): # Initialized Member Variable
    """
    feature ::= LET IDENT ASSIGN expr SEMI
    """
    if (p[2] in ["self"]):
        print("ERROR: " + str(p.lineno(2)) + ":" + str(p.lexpos(2)) + ": Parser: cannot name member variable 'self'")
        exit(0)

    p[0] = {
        "name": {
            "line": p.lineno(2),
            "col": p.lexpos(2),
            "value": p[2]
            },
        "type": "member",
        "init": p[4]
    }
    
 
# Methods
 
def p_feature_method_init(p):
    """
    feature ::= IDENT LPAREN param_array RPAREN method_block SEMI
    """
    p[0] = {
        "name": {
            "line": p.lineno(1), "col": p.lexpos(1), "value": p[1]
        },
        "type": "method",
        "parameters": p[3],
        "body": {
            "line": p.lineno(5),
            "col": p.lexpos(5),
            "value": p[5]
        }
    }
 
 
# Expressions
# expr_list ::= (expr)*
# expr_list ::= empty_list
#           |   nonempty_expr_list
# nonempty_expr_list ::= expr nonempty_expr_list
#                    |   expr
 
def p_dynamic_dispatch_expr(p):
    """
    expr ::= expr DOT IDENT LPAREN arg_array RPAREN
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "dynamic-dispatch",
            "object": p[1],
            "method": {
                "line": p.lineno(3),
                "col": p.lexpos(3),
                "value": p[3]
            },
            "args": p[5]
        }
    }
 
def p_static_dispatch_expr(p):
    """
    expr ::= expr AT IDENT DOT IDENT LPAREN arg_array RPAREN
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "static-dispatch",
            "object":  p[1],
            "class": {
                    "line": p.lineno(3),
                    "col": p.lexpos(3),
                    "value": p[3]
            },
            "method": {
                    "line": p.lineno(5),
                    "col": p.lexpos(5),
                    "value": p[5]
            },
            "args": p[7]
        }
    }
 
def p_negation_expr(p):
    """
    expr ::= UMINUS expr
    """
    p[0] ={
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "negate",
            "body": p[2]
        }
    }
 
def p_expr_array_assign(p):
    """
    expr ::= expr LBRACKET expr RBRACKET ASSIGN expr
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "array-assign",
            "lhs": p[1],
            "index": p[3],
            "rhs": p[6]
        }
    }
 
def p_array_access_expr(p):
    """
    expr ::= expr LBRACKET expr RBRACKET
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "array-access",
            "object": p[1],
            "index": p[3]
        }
    }
 
def p_paren_expr(p):
    """
    expr ::= LPAREN expr RPAREN
    """
    p[0] = p[2]
 
def p_new_array_expr(p):
    """
    expr ::= NEW LBRACKET expr RBRACKET IDENT
    """
    if p[5] not in ["Array"]:
        print("ERROR: " + str(p.lineno(5)) + ":" + str(p.lexpos(5)) + ": Parser: identifier following the brackets for a new-array expression must be 'Array'")
        exit(0)
#TODO lineno was 2 and lexpos was 2
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "new-array",
            "size": p[3] 
        }
    }
    
 
def p_expr_mult_expr(p):
    """
    expr ::= expr TIMES expr
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "times",
            "lhs": p[1],
            "rhs": p[3]
        }
    }
 
def p_expr_div_expr(p):
    """
    expr ::= expr DIVIDE expr
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "divide",
            "lhs": p[1],
            "rhs": p[3]
        }
    }
 
def p_expr_plus_expr(p):
    """
    expr ::= expr PLUS expr
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "plus",
            "lhs": p[1],
            "rhs": p[3]
        }
    }
 
def p_expr_minus_expr(p):
    """
    expr ::= expr MINUS expr
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "minus",
            "lhs": p[1],
            "rhs": p[3]
        }
    }
 
def p_expr_lt_expr(p):
    """
    expr ::= expr LT expr
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "lt",
            "lhs": p[1],
            "rhs": p[3]
        }
    }
 
def p_expr_lte_expr(p):
    """
    expr ::= expr LTE expr
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "lte",
            "lhs": p[1],
            "rhs": p[3]
        }
    }
 
def p_expr_equals_expr(p):
    """
    expr ::= expr EQUALS expr
    """
    p[0] = {
       "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "equals",
            "lhs": p[1],
            "rhs": p[3]
        }
    }
 
def p_not_expr(p):
    """
    expr ::= NOT expr
    """
    p[0] ={
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "not",
            "body": p[2]
        }
    }
 
def p_expr_to_block(p):
    """
    expr ::= block
    """
    p[0] = p[1]
 
def p_expr_assign(p):
    """
    expr ::= IDENT ASSIGN expr
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "assign",
            "lhs": {
                "line": p.lineno(1),
                "col": p.lexpos(1),
                "value": p[1]
            },
            "rhs": p[3]
        }
    }
 
 
def p_expr_list(p):
    """
    expr_list ::= nonempty_expr_list
    """
    p[0] = p[1]
 
def p_nonempty_expr_list(p):
    """
    nonempty_expr_list ::= expr SEMI nonempty_expr_list
                       |   expr SEMI
    """
    if (len(p) == 3):
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]
 
def p_arg_array(p):
    """
    arg_array ::= empty_list
              |   nonempty_arg_array
    """
    p[0] = p[1]
 
def p_nonempty_arg_array(p):
    """
    nonempty_arg_array ::= expr COMMA nonempty_arg_array
                       |   expr
    """
    if (len(p) == 4) :
        p[0] = [p[1]] + p[3]
    else:
        p[0] = [p[1]]
 
def p_param_array(p):
    """
    param_array ::= empty_list
                |   nonempty_param_array
    """
    p[0] = p[1]
 
def p_nonempty_param_array(p):
    """
    nonempty_param_array ::= IDENT COMMA nonempty_param_array
                         |   IDENT
    """
    if (len(p) == 4):
        p[0] = [{
            "line": p.lineno(1),
            "col": p.lexpos(1),
            "value": p[1]
        }] + p[3]
    else:
        p[0] = [{
            "line": p.lineno(1),
            "col": p.lexpos(1),
            "value": p[1]
        }]
 
def p_self_dispatch_expr(p):
    """
    expr ::= IDENT LPAREN arg_array RPAREN
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "self-dispatch",
            "method": {
                    "line": p.lineno(1),
                    "col": p.lexpos(1),
                    "value": p[1]
            },
            "args": p[3]
        }
    }
 
def p_expr_false(p):
    """
    expr ::= FALSE
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "bool",
            "value": False
        }
    }
 
def p_expr_true(p):
    """
    expr ::= TRUE
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "bool",
            "value": True
        }
    }
 
def p_expr_string(p):
    """
    expr ::= STRING
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "string",
            "line": p.lineno(1),
            "col": p.lexpos(1),
            "value": str(p[1])
        }
    }
 
def p_expr_int(p):
    """
    expr ::= INT
    """
    p[0] = {
       "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "number",
            "line": p.lineno(1),
            "col": p.lexpos(1),
            "value": int(p[1])
        }
    }
 
def p_expr_ident(p):
    """
    expr ::= IDENT
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "identifier",
            "value": {
                "line": p.lineno(1),
                "col": p.lexpos(1),
                "value": p[1]
            }
        }
    }
 
def p_expr_isvoid(p):
    """
    expr ::= ISVOID LPAREN expr RPAREN
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "isvoid",
            "body":  p[3]
        }
    }
 
def p_new_expr(p):
    """
    expr ::= NEW IDENT
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "new",
            "class": {
                "line": p.lineno(2),
                "col": p.lexpos(2),
                "value": p[2]
            }
        }
    }
 
def p_let_assign_expr(p):
    """
    expr ::= LET IDENT ASSIGN expr
    """
    if (p[2] in ["self"]):
        print("ERROR: " + str(p.lineno(2)) + ":" + str(p.lexpos(2)) + ": Parser: cannot name member variable 'self'")
        exit(0)

    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "let",
            "lhs": {
                "line": p.lineno(2),
                "col": p.lexpos(2),
                "value":  p[2]
            },
            "rhs": p[4]
        }    
    }
   
 
def p_let_ident(p):
    """
    expr ::= LET IDENT
    """
    if (p[2] in ["self"]):
        print("ERROR: " + str(p.lineno(2)) + ":" + str(p.lexpos(2)) + ": Parser: cannot name member variable 'self'")
        exit(0)

    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "let",
            "lhs": {
                "line": p.lineno(2),
                "col": p.lexpos(2),
                "value": p[2]
            }
        }
    }
    
 
def p_while_expr(p):
    """
    expr ::= WHILE LPAREN expr RPAREN block
    """
    if p[3] is None:
        print("ERROR: " + str(p.lineno(4)) + ":" + str(p.lexpos(4)) + ": Parser: Cannot have an empty conditional")
        exit(0)

    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "while",
            "guard": p[3],
            "body": p[5]
        }
    }
 
def p_if_expr(p):
    """
    expr ::= IF LPAREN expr RPAREN block ELSE block
    """
    if p[3] is None:
        print("ERROR: " + str(p.lineno(4)) + ":" + str(p.lexpos(4)) + ": Parser: Cannot have an empty conditional")
        exit(0)

    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "if",
            "guard": p[3],
            "then": p[5],
            "else":  p[7]
        }
    }

def p_block(p):
    """
    block ::= block_expr
    """
    p[0] = p[1]

def p_block_expr(p):
    """
    block_expr ::= LBRACE expr_list RBRACE
    """
    p[0] = {
        "line": p.lineno(1),
        "col": p.lexpos(1),
        "value": {
            "type": "block",
            "body": p[2]
        }
    }

    

def p_method_block(p):
    """
    method_block ::= LBRACE expr_list RBRACE
    """
    p[0] = {
        "type": "block",
        "body": p[2]
    }

# This function gets called if PLY runs into an error condition (fails to parse)
def p_error(p):
    # p is None if we encounter EOF, otherwise it is the token where the error occurred.
    if p is None:
        print("ERROR: " + str(p.lineno) + ":" + str(p.lexpos) + ": Parser: encountered unexpected EOF")
    else:
        print("ERROR: " + str(p.lineno) + ":" + str(p.lexpos) + ": Parser: syntax error near " + p.value)
    exit(0)
# Main program from example parser

# Main program
if __name__ == "__main__":
    import ply.yacc as yacc
    import sys
    import json
    from token_reader import TokenReader
    # This reads all of our p_ functions and compiles them into a grammar and a parser object we can use
    parser = yacc.yacc()
    lexer = TokenReader(sys.stdin)
    # Parse tokens
    program = parser.parse(lexer = lexer, tracking = True)
    # Print the program
    print(json.dumps(program, indent = 2))
