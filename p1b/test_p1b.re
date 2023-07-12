//38a1108d19f54a1fab115779487fbbcea3ae1843

//FIXME ask kevin why this is all messes up and write it down
open P1b;
open P1btree;

//salutations
let names : list(string) = ["mark", "sarah", "kevin", "john", "sally"];
List.iter((element) => Printf.printf("%s    ", element), salutations(names));



//dot_product
let test_list1 : list(int) = [1, 3, 5, 7, 9];
let test_list2 : list(int) = [2, 4, 6, 8 , 10];

switch(dot_product(test_list1, test_list2)){
    |Some(product) => {
        Printf.printf("the number you wanted is: %d\n", product);
    }
    |None => ();
};


//count
let count_test : list(int) = [6,1,1,1,1,1,1,6];

Printf.printf("%d\n", count(count_test, 1));




//int_tree_map
let test_tree : int_tree = TreeNode(1, 
                                TreeNode(2,
                                     TreeNode(4, EmptyTree, EmptyTree), 
                                     TreeNode(5, EmptyTree, EmptyTree)),
                                TreeNode(3, EmptyTree, EmptyTree));

let mapped_tree : int_tree = int_tree_map(e => e *2, test_tree);

draw_int_tree(mapped_tree);

pre_order(test_tree);