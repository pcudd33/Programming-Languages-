//139ba267302516bcc9e9e23141d94ac04cd56a91
// Code to represent and read in an AST

// program is a list of class-name -- value pairs
type program = list((string, clas))

// clas is a tuple
// name, inherits, members, methods
and clas = (string, option(string), list(member), list(method))

// member is a tuple: name, init
and member = 
    | NoInitMember(identifier)
    | InitMember(identifier, expression)

// methdo is a tuple: name, list of params, body
and method = (identifier, list(identifier), expression)

and identifier = (string, loc)

// line and column
and loc = (int, int)

and expression = (expr_val, loc)

// variant type
and expr_val = 
    | Internal(string)
    | Assign(identifier, expression)
    | ArrayAssign(expression, expression, expression)
    | DynamicDispatch(expression, identifier, list(expression))
    | StaticDispatch(expression, identifier, identifier, list(expression))
    | SelfDispatch(identifier, list(expression))
    | If(expression, expression, expression)
    | While(expression, expression)
    | Block(list(expression))
    | LetNoInit(identifier)
    | LetInit(identifier, expression)
    | New(identifier)
    | NewArray(expression)
    | IsVoid(expression)
    | MathOp(expression, expression, string)
    | Comp(expression, expression, string)
    | Not(expression)
    | Negate(expression)
    | ArrayAccess(expression, expression)
    | Ident(identifier)
    | Int(Int64.t, loc) 
    | String(string, loc)
    | Bool(bool)
;

// helper methods to convert JSON to AST
open Yojson.Safe.Util;

let json_to_loc = (json) => {
    (
        to_int(member("line", json)), 
        to_int(member("col", json))
    );
}
let json_to_ident = (json) => {
    let loc = json_to_loc(json);
    let value = to_string(member("value", json));
    (value, loc);
};

let rec json_to_expr_val = (json) => {
    let typ = to_string(member("type", json));
    switch(typ) {
        | "assign" => {
            Assign(
                json_to_ident(member("lhs", json)),
                json_to_expression(member("rhs", json))
            )
        }
        | "array-assign" => {
            ArrayAssign(
                json_to_expression(member("lhs", json)),
                json_to_expression(member("index", json)),
                json_to_expression(member("rhs", json))
            )
        }
        | "dynamic-dispatch" => {
            DynamicDispatch(
                json_to_expression(member("object", json)),
                json_to_ident(member("method", json)),
                List.map(json_to_expression, to_list(member("args", json)))
            )
        }
        | "static-dispatch" => {
            StaticDispatch(
                json_to_expression(member("object", json)),
                json_to_ident(member("class", json)),
                json_to_ident(member("method", json)),
                List.map(json_to_expression, to_list(member("args", json)))
            )
        }
        | "self-dispatch" => {
            SelfDispatch(
                json_to_ident(member("method", json)),
                List.map(json_to_expression, to_list(member("args", json)))
            )
        }
        | "if" => {
            If(
                json_to_expression(member("guard", json)),
                json_to_expression(member("then", json)),
                json_to_expression(member("else", json))
            )
        }
        | "while" => {
            While(
                json_to_expression(member("guard", json)),
                json_to_expression(member("body", json)),
            )
        }
        | "block" => {
            Block(
            List.map(json_to_expression, to_list(member("body", json))) 
            )
        }
        | "let" => {
            switch(member("rhs", json)) {
                |`Null => LetNoInit(json_to_ident(member("lhs", json)))
                | _ => {
                    LetInit(
                        json_to_ident(member("lhs", json)),
                        json_to_expression(member("rhs", json))
                    )
                }
            }
        }
        | "new" => {
            New(
                json_to_ident(member("class", json))
            )
        }
        | "new-array" => {
            NewArray(
                json_to_expression(member("size", json))
            )
        }
        | "isvoid" => {
            IsVoid(
                json_to_expression(member("body", json))
            )
        }
        | "minus"
        | "times"
        | "divide"
        | "plus" => {
            MathOp(
                json_to_expression(member("lhs", json)),
                json_to_expression(member("rhs", json)), 
                typ
            )
        }
        | "equals"
        | "lt"
        | "lte" => {
            Comp(
                json_to_expression(member("lhs", json)),
                json_to_expression(member("rhs", json)), 
                typ
            )
        }
        | "not" => {
            Not(
                json_to_expression(member("body", json))
            )
        }
        | "negate" => {
            Negate(
                json_to_expression(member("body", json)))
        }
        | "array-access" => {
            ArrayAccess(
                json_to_expression(member("object", json)),
                json_to_expression(member("index", json)))
        }
        | "identifier" => {
            Ident(json_to_ident(member("value", json)))
        }
        
        | "number" => {
            Int(
              Int64.of_string(Yojson.Safe.to_string(member("value", json))), json_to_loc(json)
            )
        }
        | "string" => {
            String(
                to_string(member("value", json)), json_to_loc(json)
            )
        }
        | "bool" => {
            Bool(
                to_bool(member("value", json))
            )
        }
        | _ => failwith(Printf.sprintf("Unhandled expression type %s\n", typ));
    }
}
and json_to_expression = (json) => {
    let loc = json_to_loc(json);
    let expr_val = json_to_expr_val(member("value", json));
    (expr_val, loc)
};

// transform a member object
let json_to_member = (json) => {
    let member_name = json_to_ident(member("name", json));
    let init = member("init", json);
    // switch to see if we have an initalizer
    switch(init) {
        | `Null => NoInitMember(member_name)
        | _ => InitMember(member_name, json_to_expression(init))
    }
};

// transform a method object
let json_to_method = (json) => {
    let method_name = json_to_ident(member("name", json));
    let params = List.map(json_to_ident, to_list(member("parameters", json)));
    let body = json_to_expression(member("body", json));
    (method_name, params, body);
}
// Enrty to converting JSON
let json_to_prog = (json) : program => {
    // map all list elements to their AST class equivalent
    List.map((json_class) : (string, clas) => {
        let class_name = to_string(member("class_name", json_class));
        let inherits = to_string_option(member("inherits", json_class));
        // map all the members
        let members = List.map(json_to_member, to_list(member("members", json_class)));
        let methods = List.map(json_to_method, to_list(member("methods", json_class)));
        (class_name, (class_name, inherits, members, methods));
    }, to_list(json));
};