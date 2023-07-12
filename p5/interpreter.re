//139ba267302516bcc9e9e23141d94ac04cd56a91
// Main filer for this project
open Ast;
open Opsems;

// check that there are exactly 2 command line arguments (first prog, second is the ast file)
if (Array.length(Sys.argv) != 2) {
    Printf.printf("Usage: interpreter.exe sl-ast-file\n");
    exit(1);
};

// grab just the file name of the AST
let filename = Sys.argv[1];

// open the file as JSON
let sl_ast = Yojson.Safe.from_file(filename);

let program : program = json_to_prog(sl_ast);

// build expression to start program evaluation 
// expression = (expr_val, loc)
// (new Main).main()
// DynDispatch(New("Main"), "main", [])
let nowhere = (0,0)
let main = (DynamicDispatch(
    (New(("Main", nowhere)), nowhere),
    ("main", nowhere),
    []
) , nowhere);

// evaluate the main expression
let e = emptyEnv;
let s = emptyStore;
let so = Void;

// evalutate the expression
let (v, e', s') = evaluate_expression(program, so, e, s, main)