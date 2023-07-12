// 139ba267302516bcc9e9e23141d94ac04cd56a91
// All the code we need for evaluating snail programs 
open Ast;

type location = int

type value = 
    | Void
    | Bool(bool)
    | String(int, string)
    | Int(Int64.t)
    | Array(int, Array.t(value))
    | Dyn(string, list((string, location)))

module OrderedLocation = {
    type t = location;
    let compare = compare;
};

// map module for store
module LocationMap = Map.Make(OrderedLocation);
// map module for environment
module StringMap = Map.Make(String);

type environment = StringMap.t(location);
type store = LocationMap.t(value);

// helper variable bindings 
let emptyEnv  : environment = StringMap.empty;
let emptyStore  : store = LocationMap.empty;

// tracking of locations
let loc_val : ref(int) = ref(0);
// function to get a new location
// assumption every location is unique
let newloc = () => {
    let l = loc_val^;
    // incremenet the location value
    incr(loc_val);
    l;
};

let run_time_error = (location : loc, mess : string) => {
    let (line, col) = location;
    Printf.printf("ERROR: %d:%d: Exception: %s\n", line, col, mess);
    exit(0);
};

// function to collect all member cariables of a given tupe 
// so that we can construct a new object
let rec get_members = (prog : program, typ : string) : list(member) => {
    // base case (all built in types have no members)
    // look up the class typ in the AST (is this a base case?)
    switch(List.assoc_opt(typ, prog)) {
        |Some(cls) => {
            // recursive case
            // recuse on our parent
            // then append out memebers to the returned list
            let (_name, inherits, members, _methods) = cls;
            let parent_members = switch(inherits) {
                | Some(parent_typ) => get_members(prog, parent_typ)
                | None => get_members(prog, "Object")
            };
            parent_members @ members;
        }
        | None => {
            // Not found --- check if this is a base class
            switch(typ) {
                | "Array"
                | "Bool"
                | "Int"
                | "IO"
                | "Object"
                | "String" => {
                    // Base case -> return an empty list of members
                    []
                }
                | _ => { 
                    failwith(Printf.sprintf("ERROR: Uknown class %s", typ));
                }
            }
        }
    }
};

// looking up a method given an AST and a types name 
// method is a tuple: name, list of params, body
let rec find_method = (prog : program, typ : string, method : string, loc : loc) : method => {
    // look up in our AST the class type that we are working with 
    switch(List.assoc_opt(typ, prog)) {
        | Some(cls) => {
            // we found the class in the AST
            let (_name, inherits, _members, methods : list(method)) = cls;
            // see if we can find our target method in the method list
            // use List.find_opt to search for the correct method
            switch(List.find_opt((m : method) => {
                // extract the method name from the method tuple
                let (nm, _, _) = m;
                let (name, _) = nm;
                // check to see if the name is the one we are looking for
                name == method;
            }, methods)) {
                | Some(method) => {
                    // we found or target method
                    // return it 
                    method
                }
                | None => {
                    // dont' find it
                    // recurse on our parent class
                    switch(inherits) {
                        | Some(parent) => find_method(prog, parent, method, loc)
                        // inherit from Object by default 
                        | None => find_method(prog, "Object", method, loc)
                    }
                }
            }
        }
        | None => {
            // class was not found in the AST
            // it could be a built in type
            // TODO fill in all of the built in methods for each supported type
            switch(typ) {
                | "Array" => {
                    switch(method) {
                        | "length" => {
                            // return a method tuple for this method
                            // method is tuple: name, list of params, body 
                            // what is the body? the body is a snail expression
                            // but! there is no snail code for our built-in methods
                            (("length", loc), [], (Internal("Array.length"), loc))
                        }
                        | _ => {
                            // unknown method
                            //recuse
                            find_method(prog, "Object", method, loc);
                        }
                    }
                }
                | "Bool"
                | "Int" => {
                    find_method(prog, "Object", method, loc);
                }
                | "IO" => {
                    switch(method) {
                        | "print_string" => {
                            (("print_string", loc), [("s", loc)], (Internal("IO.print_string"), loc))
                        }
                        | "print_int" => {
                            (("print_int", loc), [("s", loc)], (Internal("IO.print_int"), loc))
                        }
                        | "read_string" => {
                            (("read_string", loc), [], (Internal("IO.read_string"), loc))
                        }
                        | "read_int" => {
                            (("read_int", loc), [], (Internal("IO.read_int"), loc))
                        }
                        | _ => find_method(prog, "Object", method, loc);
                    }
                }
                | "String" => {
                    switch(method) {
                        | "concat" => {
                            (("concatt", loc), [("s", loc)], (Internal("String.concat"), loc))
                        }
                        | "length" => {
                            (("length", loc), [], (Internal("String.length"), loc))
                        }
                        | "substr" => {
                            (("substr", loc), [("start", loc), ("length", loc)], (Internal("String.substr"), loc))
                        }
                        | _ => find_method(prog, "Object", method, loc);
                    }
                }
                | "Object" => {
                    switch(method) {
                        | "abort" => {
                            (("abort", loc), [], (Internal("Object.abort"), loc))
                        }
                        | "copy" => {
                            (("copy", loc), [], (Internal("Object.copy"), loc))
                        }
                        | "get_type" => {
                            (("get_type", loc), [], (Internal("Object.get_type"), loc))
                        }
                        | "is_a" => {
                            (("is_a", loc), [("t", loc)], (Internal("Object.is_a"), loc))
                        }
                        | _ => find_method(prog, "Object", method, loc);
                    }
                }
                | _ => {
                    run_time_error(loc, "Unkown class or method declaration.");
                }
            }
        }
    }
};

let rec inherits = (prog : program, class_name : string, target_name : string) : bool => {
    if (class_name == target_name) {
        true;
    } else {
        switch(List.assoc_opt(class_name, prog)) {
            | Some(cls) => {
                let (_name, inherited, _members, _ : list(method)) = cls;
                switch(inherited) {
                            | Some(parent) => {
                                inherits(prog, parent, target_name);
                            }
                            // inherit from Object by default 
                            | None => {
                                target_name == "Object";
                            }
                        }
                    }
            | None => false;
        }
    };
};



// expression evaluation code
let rec evaluate_expression = ((prog : program),
                               (so : value), 
                                (e : StringMap.t(location)), 
                                (s : LocationMap.t(value)),
                              (exp : expression))  : (value, environment, store) => {
    // expression = (expr_val, loc)
    // decompose
    let (expval, expl) = exp;

    // switch on the expval type 
    switch (expval) {
        | ArrayAssign(expr1, expr2, expr3) => { 
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, expr1);
            let (v2, e2, s2) = evaluate_expression(prog, so, e1, s1, expr2);
            switch(v2) {
                | Array(len, cur_array) => {
                    let (v3, e3, s3) = evaluate_expression(prog, so, e2, s2, expr3);
                    switch(v3) {
                        | Int(loc) => {
                            if (0 <= Int64.to_int(loc) && Int64.to_int(loc) <= len) {
                                cur_array[Int64.to_int(loc)] = v1;
                                (v1, e2, s2);
                            } else {
                                let (_, location) = expr3;
                                run_time_error(location, "Specified index must be within the bounds of 0 and the array length.");
                            }
                        }
                        | _ => {
                            let (_, location) = expr3;
                            run_time_error(location, "Must perform array assignments with a positive integer index.");
                        }
                    }
                }
                | _ => { // Error
                    let (_, location) = expr2;
                    run_time_error(location, "Must perform array assignment on an array.");
                }
            }
        }
        | Assign(ident, expr) => { 
            let (iname, _) = ident;
            let lid = newloc();
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, expr);
            let e2 = List.fold_left2( // Update environment 
                (acc, loc, arg) => {
                    StringMap.add(loc, arg, acc);
                }, e1, [iname], [lid] );
            let s2 = List.fold_left2(
                (acc, loc, arg) => {
                    LocationMap.add(loc, arg, acc);
                }, s1, [lid], [v1]);
            (v1, e2, s2);
        }
        | DynamicDispatch(target_exp, method_id, arg_list) => {
            let (method_name, method_loc) = method_id;  // Decompose method identifier
            //print_endline(method_name);
            let ((vn_1, en_1, sn_1), arg_vals) = List.fold_left_map((acc, exp_i) => { // Evaluate expressions to n-1
                let (_, ei, si) = acc;
                let (vx, ex, sx) = evaluate_expression(prog, so, ei, si, exp_i);
                ((vx, ex, sx), vx);
            }, (Void, e, s), arg_list);
            let (v0, enp1, snp1) = evaluate_expression(prog, so, en_1, sn_1, target_exp); // Evaluate target expression
            let (class_name, memb_locs) = switch(v0) { // Switch on type and destructure name and member locations
                | Dyn(class_name, memb_locs) => (class_name, memb_locs)
                | Bool(_) => ("Bool", [])
                | Int(_) => ("Int", [])
                | String(_, _) => ("String", [])
                | Array(_, _) => ("Array", [])
                | Void => {
                    let (_, location) = target_exp;
                    run_time_error(location, "Cannot have dispatch on specified object.");
                }
            }
            let (name, param_list, ebody) = find_method(prog, class_name, method_name, method_loc); // Lookup method, destructure name, params and body
            let locs = List.map((_) => newloc(), param_list);
            let e_partial = List.fold_left( // e_partial for {a1:la1,... am:lam}
                (acc, member) => {
                    let (name, loc) = member;
                    StringMap.add(name, loc, acc);
                }, emptyEnv, memb_locs);
                //print_endline("dd 304");
                //Printf.printf("%d %d\n", List.length(memb_locs), List.length(locs));
            let e_dispatch = List.fold_left2( // e_dispatch connects partial to {x1, lx1, ... xn, lxn}
                (acc, param, loc) => {
                    let (name, _loc) = param;
                    StringMap.add(name, loc, acc);
                }, e_partial, param_list, locs);
               // print_endline("dd 311");
            let snp2 = List.fold_left2( // Update values - store 2
                    (acc, loc, arg) => {
                        LocationMap.add(loc, arg, acc);
                    }, snp1, locs, arg_vals);
                    //print_endline("dd 316");
            let (vret, eret, sret) = evaluate_expression(prog, v0, e_dispatch, snp2, ebody); // Evaluate return expression
            (vret, enp1, sret); // Return
        }
        | StaticDispatch(target_exp, method_id, inherits_id, arg_list) => {
            let (method_name, method_loc) = method_id;
            let (inherits_name, inherits_loc) = inherits_id;
            let ((vn_1, en_1, sn_1), arg_vals) = List.fold_left_map((acc, exp_i) => { // Evaluate expressions to n-1
                let (_, ei, si) = acc;
                let (vx, ex, sx) = evaluate_expression(prog, so, ei, si, exp_i);
                ((vx, ex, sx), vx);
            }, (Void, e, s), arg_list);
            let (v0, en, sn) = evaluate_expression(prog, so, en_1, sn_1, target_exp); // Evaluate current
            let (class_name, memb_locs) = switch(v0) { // Decompose v0
                | Dyn(class_name, memb_locs) => (class_name, memb_locs)
                | Bool(_) => ("Bool", [])
                | Int(_) => ("Int", [])
                | String(_, _) => ("String", [])
                | Array(_, _) => ("Array", [])
                | Void => {
                    let (_, location) = target_exp;
                    run_time_error(location, "Cannot call method on null reference");
                }
            };
            if (!inherits(prog, class_name, inherits_name)) { // Error handling for inheritance
                let (_, location) = target_exp;
                run_time_error(location, "Could not inherit from specified class");
            };
            let (name, param_list, ebody) = find_method(prog, class_name, method_name, method_loc); // Lookup method
            let locs = List.map((_) => newloc(), param_list); // New locations
            let e_partial = List.fold_left( // e_partial for {a1:la1, ... am:lam}
                (acc, member) => {
                    let (name, loc) = member;
                    StringMap.add(name, loc, acc);
                }, emptyEnv, memb_locs);
            let e_dispatch = List.fold_left2( // e_dispatch connects e_partial to {x1:lx1, ... xn:lxn}
                (acc, param, loc) => {
                    let (name, loc) = param;
                    StringMap.add(name, loc, acc);
                }, e_partial, memb_locs, locs);
            let snp2 = List.fold_left2( // Update store (sn + 2)
                    (acc, loc, arg) => {
                        LocationMap.add(loc, arg, acc);
                    }, sn, locs, arg_vals);
            let (vret, eret, sret) = evaluate_expression(prog, v0, e_dispatch, snp2, ebody); // Evaluate return expression
            (vret, en, sret); // Return
        }
        | SelfDispatch(method_id, arg_list) => {
            let (method_name, method_loc) = method_id;  // Decompose method identifier
            let ((vn_1, en_1, sn_1), arg_vals) = List.fold_left_map((acc, exp_i) => { // Evaluate expressions to n-1
                let (_, ei, si) = acc;
                let (vx, ex, sx) = evaluate_expression(prog, so, ei, si, exp_i);
                ((vx, ex, sx), vx);
            }, (Void, e, s), arg_list);
            let (v, en, sn) = (so, en_1, sn_1);
            let (class_name, memb_locs) = switch(so) { // Decompose self object (class, member locs)
                | Dyn(class_name, memb_locs) => (class_name, memb_locs)
                | Bool(_) => ("Bool", [])
                | Int(_) => ("Int", [])
                | String(_, _) => ("String", [])
                | Array(_, _) => ("Array", [])
                | Void => { 
                    let (_, location) = exp;
                    run_time_error(location, "Cannot call method on null reference");
                }
            };
            let (name, param_list, ebody) = find_method(prog, class_name, method_name, method_loc); // Lookup method
            let locs = List.map((_) => newloc(), param_list); // New locations
            let e_partial = List.fold_left( // e_partial for {a1:la1, ..., am:lam}
                (acc, member) => {
                    let (name, loc) = member;
                    StringMap.add(name, loc, acc);
                }, emptyEnv, memb_locs);
                //print_endline("sd 389");
            let e_dispatch = List.fold_left2( // e_dispatch connects e_partial to {x1:lx1, ..., xn:lxn}
                (acc, param, loc) => {
                    let (name, _loc) = param;
                    StringMap.add(name, loc, acc);
                }, e_partial, param_list, locs);
                //print_endline("sd 395");
            let snp1 = List.fold_left2( // Update store (sn + 1) *** Invalid_argument *** 
                    (acc, loc, arg) => {
                        LocationMap.add(loc, arg, acc);
                    }, sn, locs, arg_vals);
                    //print_endline("sd 400");
            let (vret, eret, sret) = evaluate_expression(prog, so, e_dispatch, snp1, ebody); // Evaluate return expression
            (vret, en, sret); // Return
        }
        | If(expr1, expr2, expr3) => {
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, expr1);
            switch(v1) {
                | Bool(true) => { // If - True
                    let (v2, e2, s2) = evaluate_expression(prog, so, e1, s1, expr2);
                    (v2, e2, s2);
                }
                | Bool(false) => { // If - False
                    let (v3, e2, s2) = evaluate_expression(prog, so, e1, s1, expr3);
                    (v3, e2, s2);
                }
                | _ => { // Error
                    let (_, location) = expr1;
                    run_time_error(location, "If statement requires boolean check before evaluating body expressions.");
                }
            }
        }
        | While(expr1, expr2) => {
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, expr1);
            switch(v1) {
                | Bool(true) => {
                    let (v2, e2, s2) = evaluate_expression(prog, so, e1, s1, expr2);
                    let (_, e3, s3) = evaluate_expression(prog, so, e2, s2, exp); 
                    (Void, e3, s3);
                }
                | Bool(false) => { // While - False
                    (Void, e1, s1);
                }
                | _ => { // Error
                    let (_, location) = expr1;
                    run_time_error(location, "While statement requires boolean check before evaluating body expressions.");
                }
            }
        }
        | Block(expr_list) => {
            let (vn, en, sn) = List.fold_left((acc, exp_i) => {
                let (_, ei, si) = acc;
                evaluate_expression(prog, so, ei, si, exp_i);
            }, (Void, e, s), expr_list);
            (vn, e, sn);
        }
        | LetInit(ident, expr) => {
            let (iname, iloc) = ident;
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, expr); // Evaluate expression
            let lid : location = newloc();
            let s2 = List.fold_left2( // Update store
                (acc, loc, arg) => {
                    LocationMap.add(loc, arg, acc);
                }, s1, [lid], [v1]);
            let e2 = List.fold_left2( // Update environment 
                (acc, loc, arg) => {
                    StringMap.add(loc, arg, acc);
                }, e1, [iname], [lid] );
            (v1, e2, s2);
        }
        | LetNoInit(ident) => {
            let (iname, iloc) = ident;
            let lid : location = newloc();
            let e1 = List.fold_left2( // Update environment 
                (acc, loc, arg) => {
                    StringMap.add(loc, arg, acc);
                }, e, [iname], [lid] );
            let s1 = List.fold_left2(
                (acc, loc, arg) => {
                    LocationMap.add(loc, arg, acc);
                }, s, [lid], [Void]);
            (Void, e1, s1);
        }
        | Ident(ident) => { 
            let (iname, iloc) = ident;
            if (iname == "self") {
                (so, e, s);
            } else {
                switch(StringMap.find_opt(iname, e)) {
                    | Some(loc) => {
                        switch(LocationMap.find_opt(loc, s)) {
                            | Some(v) => {
                               (v, e, s)
                            }
                            | None => {
                                (Void,e,s)
                            }
                        };
                    }
                    | None => {
                        (Void,e,s)
                    }
                };
            };
        }
        | New(clas_ident) => {
            // decompose type identifier
            let (t, _location) = clas_ident;
            switch(t) {
                | "Bool" => (Bool(false), e, s)
                | "Int" => (Int(0L), e, s) // 0L - Int64-literal
                | "String" => (String(0, ""), e, s)
                | _ => {
                    // get all the class members 
                    let class_members : list(member) = get_members(prog, t);
                    // map each member to create a new location
                    let locs : list((string, location)) = List.map((m : member) => {
                        let name = switch(m) {
                            | InitMember(ident, _) => {
                                let (iname, _) = ident;
                                iname;
                            }
                            | NoInitMember(ident) => {
                                let (iname, _) = ident;
                                iname;
                            }
                        };
                        (name, newloc());
                    }, class_members);

                    let v_1 : value = Dyn(t, locs);
                    // update store (for each location , add to the store Void)
                    let s_1 = List.fold_left((acc, (_, l)) => {
                        // add l => Void
                        LocationMap.add(l, Void, acc);
                    }, s, locs);

                    // create environment for the init
                    let e_1 = List.fold_left2( (acc, member, (_, location)) => {
                        let name = switch(member) {
                            | NoInitMember((n, _l))
                            | InitMember((n, _l), _) => n
                        };
                        StringMap.add(name, location, acc);
                    }, emptyEnv, class_members, locs);

                    // TODO backwards -> need to reverse 
                    let block_init = List.fold_left((acc, member) => {
                        switch(member) {
                            | NoInitMember(_) => acc
                            | InitMember(i, init_expr) => {
                                // build up a block for this assignment 
                                let blk : expression = (Block([init_expr]), expl);
                                // make an assignment 
                                let assn : expression = (
                                    Assign(i, blk),
                                    expl
                                );
                                // prepend
                                [assn, ...acc];
                            }
                        }
                    }, [], class_members);
                    let blk : expression = (Block(List.rev(block_init)), expl);
                    let (v_2, e_2, s_2) = evaluate_expression(prog, v_1, e_1, s_1, blk);
                    (v_1, e_1, s_2);
                }
            }
        }
        | Internal(internal_name) => {
            switch(internal_name) {
                // TODO we need to implement all of the built in methods here 
                | "Array.length" => {
                    // implememt Array.length functionallity
                    // access our self-object
                    switch(so) {
                        | Array(sz, arr) => {
                            (Int(Int64.of_int(sz)), e, s);
                        }
                        | _ => { // Error
                            let (_, location) = exp;
                            run_time_error(location, "Tried to get array length for a non-array object.");
                        }
                    }
                }
                | "Object.abort" => { // Abort
                    Printf.printf("abort\n%!");
                    exit(0);
                }
                | "Object.copy" => { // Copy
                    switch(so) {
                        | Int(i) => {
                            let copy = i;
                            (Int(copy), e, s);
                        }
                        | String(n, i) => {
                            let copy = i;
                            (String(n, copy), e, s);
                        }
                        | Bool(i) => {
                            let copy = i;
                            (Bool(copy), e, s);
                        }
                        | Array(_, _) => {
                            let (_, location) = exp;
                            run_time_error(location, "Tried to copy the contents of an Array.");
                        }
                        | _ => {
                            let (_, location) = exp;
                            run_time_error(location, "Tried to copy the contents of something other than an Object.");
                        }
                    }
                }
                | "Object.get_type" => { // Get type
                    switch(so) {
                        | Int(i) => {
                            let typ = "Int";
                            (String(3, typ), e, s);
                        }
                        | String(n, i) => {
                            let typ = "String";
                            (String(6, typ), e, s);
                        }
                        | Bool(i) => {
                            let typ = "Bool";
                            (String(4, typ), e, s);
                        }
                        | Array(n, i) => {
                            let typ = "Array";
                            (String(5, typ), e, s);
                        }
                        | Dyn(n, _) => {
                            (String(String.length(n), n), e, s);
                        }
                        | _ => {
                            let (_, location) = exp;
                            run_time_error(location, "Cannot call get_type on specified object.");
                        }
                    }
                }
                | "IO.print_string" => {
                    switch(StringMap.find_opt("s", e)) {
                        | Some(loc) => {
                            switch(LocationMap.find_opt(loc, s)) {
                                | Some(v) => {
                                    switch(v) {
                                        | String(n, i) => {
                                            let str = Str.regexp_string("\\n");
                                            let ret = Str.global_replace(str, "\n", i);
                                            let str2 = Str.regexp_string("\\t");
                                            let ret2 = Str.global_replace(str2, "\t", ret);
                                            Printf.printf("%s%!", ret2);
                                            (so, e, s);
                                        }
                                        | _ => {
                                        let (_, location) = exp;
                                        run_time_error(location, "Cannot call print_string on specified object.");
                                        }
                                    }
                                }
                                | None => {
                                    let (_, location) = exp;
                                    run_time_error(location, "Cannot call print_string on specified object.");
                                }
                            }
                        }
                        | None => {
                            let (_, location) = exp;
                            run_time_error(location, "Cannot call print_string on specified object.");
                        }
                    }
                }
                | "IO.print_int" => {
                    switch(StringMap.find_opt("s", e)) {
                        | Some(loc) => {
                            switch(LocationMap.find_opt(loc, s)) {
                                | Some(v) => {
                                    switch(v) {
                                        | Int(i) => {
                                            Printf.printf("%s%!", Int64.to_string(i));
                                            (so, e, s);
                                        }
                                        | _ => {
                                        let (_, location) = exp;
                                        run_time_error(location, "Cannot call print_int on specified object.");
                                        }
                                    }
                                }
                                | None => {
                                    let (_, location) = exp;
                                    run_time_error(location, "Cannot call print_int on specified object.");
                                }
                            }
                        }
                        | None => {
                            let (_, location) = exp;
                            run_time_error(location, "Cannot call print_int on specified object.");
                        }
                    }
                }
                | "Object.is_a" => {
                    switch(StringMap.find_opt("t", e)) {
                        | Some (loc) => {
                            switch(LocationMap.find_opt(loc, s)) {
                                | Some(v) => {
                                    switch(v) {
                                        | String(n, i) => {
                                            let (class_name, memb_locs) = switch(so) { // Decompose so
                                                | Dyn(class_name, memb_locs) => (class_name, memb_locs)
                                                | Bool(_) => ("Bool", [])
                                                | Int(_) => ("Int", [])
                                                | String(_, _) => ("String", [])
                                                | Array(_, _) => ("Array", [])
                                                | Void => {
                                                    let (_, location) = exp;
                                                    run_time_error(location, "Cannot call method on null reference");
                                                }
                                            };
                                            let vret = inherits(prog, class_name, i);
                                            (Bool(vret), e, s);
                                        }
                                        | _ => {
                                            let (_, location) = exp;
                                            run_time_error(location, "is_a parameter must be a single string representing the class name.");
                                        }
                                    }
                                }
                                | None => {
                                    let (_, location) = exp;
                                    run_time_error(location, "Cannot call is_a on specified object.");
                                }
                            }
                        }
                        | None => {
                            let (_, location) = exp;
                            run_time_error(location, "Cannot call is_a on specified object.");
                        }
                    }
                }
                | "IO.read_string" => { 
                    let str = read_line();
                    if (str == "") {
                        let (_, location) = exp;
                        run_time_error(location, "No input before EOF.");
                    };
                    let len = String.length(str);
                    (String(len + 1, str ++ "\\0"), e, s);
                }
                | "IO.read_int" => {
                    let str = read_line();
                    if (str == "") {
                        (Int(Int64.of_int(0)), e, s);
                    } else {
                        let st = Str.regexp("[1-9]+");
                        if (Str.string_match(st, str, 0)) {
                            if ((-2.0 ** 63.0) <= float_of_string(str) && float_of_string(str) <= ((2.0 ** 63.0)) -. 1.0) {
                                (Int(Int64.of_string(str)), e, s);
                            } else {
                                (Int(Int64.of_int(0)), e, s);
                            }
                        } else {
                            (Int(Int64.of_int(0)), e, s);
                        }
                    }
                }
                | "String.concat" => { 
                    switch(StringMap.find_opt("s", e)) {
                        | Some(loc) => {
                            switch(LocationMap.find_opt(loc, s)) {
                                | Some(v) => {
                                    switch(v) {
                                        | String(ni, i) => {
                                            switch(so) {
                                                | String(nj, j) => {
                                                    let vret = j ++ i;
                                                    (String(ni+nj, vret), e, s);
                                                }
                                                | _ => {
                                                    let (_, location) = exp;
                                                    run_time_error(location, "Cannot call .concat on specified object.");
                                                }
                                            }
                                        }
                                        | _ => {
                                            let (_, location) = exp;
                                            run_time_error(location, "Cannot concatenate specified object to String.");
                                        }
                                    }
                                }
                                | None => {
                                    let (_, location) = exp;
                                    run_time_error(location, "Cannot call String.concat on specified object.");
                                }
                            }
                        }
                        | None => {
                            let (_, location) = exp;
                            run_time_error(location, "Cannot call String.concat on specified object.");
                        }
                    }
                }
                | "String.substr" => {
                    let start = Int64.to_int(switch(StringMap.find_opt("start", e)) {
                       | None => {
                           let (_, location) = exp;
                           run_time_error(location, "Cannot call String.substr on specified object.");
                       }
                       | Some(loc1) => {
                           switch(LocationMap.find_opt(loc1, s)) {
                               | None => {
                                   let (_, location) = exp;
                                   run_time_error(location, "Cannot call String.substr on specified object.");
                               }
                               | Some(v) => {
                                   switch(v) {
                                       | Int(i) => {
                                           i;
                                       }
                                       | _ => {
                                            let (_, location) = exp;
                                            run_time_error(location, "Cannot call String.substr on specified object.");
                                       }
                                   }
                               }
                           }
                       }
                   });

                   let len = Int64.to_int(switch(StringMap.find_opt("length", e)) {
                       | None => {
                            let (_, location) = exp;
                            run_time_error(location, "Cannot call String.substr on specified object.");
                       }
                       | Some(loc) => {
                           switch(LocationMap.find_opt(loc, s)) {
                               | None => {
                                   let (_, location) = exp;
                                   run_time_error(location, "Cannot call String.substr on specified object.");
                               }
                               | Some(v) => {
                                   switch(v) {
                                       | Int(i) => {
                                            i;
                                       }
                                       | _ => {
                                            let (_, location) = exp;
                                            run_time_error(location, "Cannot call String.substr on specified object.");
                                       }
                                   }
                               }
                           }
                       }
                    });

                    let so_str = switch(so) {
                        | String(_len, string) => {
                           string;
                        }
                        | _ => {
                            let (_, location) = exp;
                            run_time_error(location, "Cannot call String.substr on specified object.");
                        }
                    };
                   
                    let olen = String.length(so_str);
                    if (start + len > olen) {
                        let (_, location) = exp;
                        run_time_error(location, "Invalid indices specified for String.substr");
                    };

                    let retstr = String.sub(so_str, start, len);
                    (String(String.length(retstr), retstr), e, s);
                }
                | "String.length" => {
                    switch(so) {
                        | String(n, i) => {
                            (Int(Int64.of_int(n)), e, s);
                        }
                        | _ => {
                            let (_, location) = exp;
                            run_time_error(location, "Cannot call length on specified object.");
                        }
                    }
                }
                | _ => {
                    failwith(Printf.sprintf("ERROR: Unimplemented internal method %s", internal_name))
                }
            }
        }
        | NewArray(expr) => { 
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, expr);
            switch(v1) {
                | Int(i) => {
                    if (0 <= Int64.to_int(i)) {
                        let v = Array(Int64.to_int(i), Array.make(Int64.to_int(i), Void));
                        (v, e, s);
                    } else {
                        let (_, location) = expr;
                        run_time_error(location, "Cannot have negative length array.");
                    }
                }
                | _ => {
                    let (_, location) = expr;
                    run_time_error(location, "Creation of a new array object requires an integer specification for the size.");
                }
            }
        }
        | IsVoid(expr) => { 
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, expr);
            if (v1 == Void) {
                (Bool(true), e1, s1);
            }else{
                (Bool(false), e1, s1);
            };
        }
        | MathOp(expr1, expr2, str) => {
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, expr1);
            let (v2, e2, s2) = evaluate_expression(prog, so, e1, s1, expr2);
            switch(v1, v2) {
                | (Int(i), Int(j)) => {
                    switch(str) {
                        | "plus" => {
                            let vret = Int64.add(i, j);
                            (Int(vret), e2, s2);
                        }
                        | "minus" => {
                            let vret = Int64.sub(i, j);
                            (Int(vret), e2, s2);
                        }
                        | "times" => {
                            let vret = Int64.mul(i, j);
                            (Int(vret), e2, s2);
                        }
                        | "divide" => {
                            if (j == Int64.of_int(0)) {
                                let (_, location) = expr1;
                                run_time_error(location, "Cannot divide by zero.");
                            }
                            let vret = Int64.div(i, j);
                            (Int(vret), e2, s2);
                        }
                        | _ => { // Error
                            let (_, location) = expr1;
                            run_time_error(location, "Syntax between integer values does not match any known operator.");
                        }
                    }
                }
                | (_, Int(j)) => {
                    let (_, location) = expr1;
                    run_time_error(location, "Can only perform mathematical operations on integers.")
                }
                | (Int(i), _) => {
                    let (_, location) = expr2;
                    run_time_error(location, "Can only perform mathematical operations on integers.")
                }
                | _ => {
                    let (_, location) = expr1;
                    run_time_error(location, "Can only perform mathematical operations on integers.")
                }
            }
        }
        | Comp(expr1, expr2, str) => { // *** ERROR HANDLING ? ***
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, expr1);
            let (v2, e2, s2) = evaluate_expression(prog, so, e1, s1, expr2);
            switch(str) {
                | "lte" => {
                    switch (v1, v2) {
                        | (Int(i), Int(j)) => {
                            let comp = Int64.compare(i, j);
                            if (comp > 0) { // i-j > 0 means i > j
                                (Bool(false), e2, s2);
                            } else { // i <= j, i-j <= 0
                                (Bool(true), e2, s2);
                            }
                        }
                        | (Bool(i), Bool(j)) => {
                            let comp = Bool.compare(i, j);
                            if (comp > 0) { // true - false > 0
                                (Bool(false), e2, s2);
                            } else { // false - true <= 0
                                (Bool(true), e2, s2);
                            }
                        }
                        | (String(iloc, i), String(jloc, j)) => {
                            let comp = String.compare(i, j);
                            if (comp > 0) { // i - j > 0, j <= i
                                (Bool(false), e2, s2);
                            } else { // i - j <= 0, i <= j
                                (Bool(true), e2, s2);
                            }
                        }
                        | _ => {
                            let ret = compare(v1, v2);
                            if (ret <= 0) {
                                (Bool(true), e2, s2);
                            } else {
                                (Bool(false), e2, s2);
                            }
                        }
                    }
                }
                | "lt" => {
                    switch (v1, v2) {
                        | (Int(i), Int(j)) => {
                            let comp = Int64.compare(i, j);
                            if (comp < 0) { // i-j < 0 means i < j
                                (Bool(true), e2, s2);
                            } else { // comp >= 0, i-j >= 0
                                (Bool(false), e2, s2);
                            }
                        }
                        | (Bool(i), Bool(j)) => {
                            let comp = Bool.compare(i, j);
                            if (comp < 0) { // true - false > 0
                                (Bool(true), e2, s2);
                            } else { // false - true <= 0
                                (Bool(false), e2, s2);
                            }
                        }
                        | (String(iloc, i), String(jloc, j)) => {
                            let comp = String.compare(i, j);
                            if (comp < 0) { // i - j <= 0, j >= i
                                (Bool(true), e2, s2);
                            } else { // i - j > 0, i > j
                                (Bool(false), e2, s2);
                            }
                        }
                        | _ => {
                            let ret = compare(v1, v2);
                            if (ret < 0) {
                                (Bool(true), e2, s2);
                            } else {
                                (Bool(false), e2, s2);
                            }
                        }
                    }
                }
                | "equals" => {
                    switch (v1, v2) {
                        | (Int(i), Int(j)) => {
                            let comp = Int64.compare(i, j);
                            if (comp == 0) { // i-j = 0
                                (Bool(true), e2, s2);
                            } else { 
                                (Bool(false), e2, s2);
                            }
                        }
                        | (Bool(i), Bool(j)) => {
                            let comp = Bool.compare(i, j);
                            if (comp == 0) { // true - true / false - false = 0
                                (Bool(true), e2, s2);
                            } else { 
                                (Bool(false), e2, s2);
                            }
                        }
                        | (String(iloc, i), String(jloc, j)) => {
                            let comp = String.compare(i, j);
                            if (comp == 0) { // i == j
                                (Bool(true), e2, s2);
                            } else { 
                                (Bool(false), e2, s2);
                            }
                        }
                        | _ => {
                            let ret = compare(v1, v2);
                            if (ret == 0) {
                                (Bool(true), e2, s2);
                            } else {
                                (Bool(false), e2, s2);
                            }
                        }
                    }
                }
                | _ => {
                    let (_, location) = expr1;
                    run_time_error(location, "Cannot compare objects; specified operator not implemented.");
                }
            }
        }
        | Not(expr) => {  
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, expr);
            switch(v1) {
                | Bool(true) => { // !True = False
                    (Bool(false), e1, s1);
                }
                | Bool(false) => { // !False = True
                    (Bool(true), e1, s1);
                }
                | _ => { // Error (?)
                    let (_, location) = expr;
                    run_time_error(location, "Not requires an object to negate.");
                }
            }
        }
        | Negate(expr) => { 
            let (v1, e1, s1) = evaluate_expression(prog, so, e, s, expr);
            switch(v1) {
                | Int(int) => {
                    (Int(Int64.neg(int)), e1, s1)
                }
                | _ => {
                    let (_, location) = expr;
                    run_time_error(location, "Negation requires an integer to negate.");
                }
            }
        }
        | Int(i, loc) => {
            (Int(i), e, s)
        }
        | String(str, loc) => {
            (String(String.length(str), str), e, s)
        }
        | Bool(bool) => {
            (Bool(bool), e, s)
        }
        | _ => {
            let (_, location) = exp;
            run_time_error(location, "Unhandled expression type.");
        }
    };
};