//38a1108d19f54a1fab115779487fbbcea3ae1843
/*
 * P1b Tree representation
 * Kevin Angstadt
 * St. Lawrence University
 *
 * recall you can do: open P1btree;
 */
 
type tree('a) =
| EmptyTree
| TreeNode('a, tree('a), tree('a));

/* accessor functions */
let root_data = (t: tree('a)) : 'a => {
    switch(t) {
        | EmptyTree => failwith("accessing empty tree")
        | TreeNode(d, _, _) => d
    }
};
let left_child = (t: tree('a)) : tree('a) => {
    switch(t) {
        | EmptyTree => EmptyTree
        | TreeNode(_, l, _) => l
    }
};
let right_child = (t: tree('a)) : tree('a) => {
    switch(t) {
        | EmptyTree => EmptyTree
        | TreeNode(_, _, r) => r
    }
};

/*
 * draw a tree on standard out.
 * t : the tree
 * repr : function for converting tree data to string
 */
let draw = ((t: tree('a)), (repr: 'a => string)) : unit => {
    let rec indent = ((t: tree('a)), (prefix: string)) => {
        switch (right_child(t)) {
            | EmptyTree => ()
            | TreeNode(_,_,_) as subt => indent(subt, (prefix ++ "  "))
        };
        Printf.printf("%s--%s\n", prefix, repr(root_data(t)));
        switch (left_child(t)) {
            | EmptyTree => ()
            | TreeNode(_,_,_) as subt => indent(subt, (prefix ++ "  "));
        }
    };
    indent(t, "");
};

/* let's define an int_tree */
type int_tree = tree(int);

/* draw an int_tree */
let draw_int_tree = (t: int_tree) : unit => {
    draw(t, (i => Printf.sprintf("%i", i)))
};