//38a1108d19f54a1fab115779487fbbcea3ae1843

//we want to take in a list of strings which will be created in p1b_test.re
//we take that list and print salutations on each iteration of the values that are in list
let rec salutations = (l: list(string)) : list(string) => {
  let tmp = [];
  switch(l){
      |[] => tmp;
      |[hd,...tl] =>{
           tmp @ [("Salutations, " ++ hd)] @ salutations(tl);
      }
  }
};


//this list_length function is here so we can compare the two lists and make sure they
//have the same amount of elements in each
let rec list_length = (l: list('a)) : int => {
 switch(l){
   |[] => 0;
   |[_hd, ...tl] => 1 + list_length(tl);
 };
};

//make two lists of integers of the same length within the test file 
//we take the product of both integers at the same index within l1 & l2 and then store the product within 
//option(int)
let dot_product =  ((l1: list(int)), (l2: list(int))) : option(int) => {
    let total : int = 0; 

  let rec dot_helper = ((l1: list(int)), (l2: list(int)), (total: int)) : option(int) => {
    switch(l1, l2){
    |([hd1,...tl1], [hd2,...tl2]) when list_length(l1) == list_length(l2) => {
      let product = (hd1) * (hd2);
      dot_helper(tl1, tl2, total + product);
    }
    |([],[]) when l1 == l2 => Some(total);
    |(_, _) => None;
    };
  };
  dot_helper(l1, l2, total);
};

//tis code takes in a list of any type as long as all elements are the same
//and the count_helper function takes in all of the parameters that count does but also a counter 
//the switch statement recursivley checks the head to see if it matches the element we are searching for
//and then through the other switch cases adds all occurences of said element to the counter
let count =  ((l: list ('a)), (e: 'a)) : int => {
  let counter = 0;
  let rec count_helper = ((l: list('a)), (e: 'a), (counter : int)) : int => {
    switch(l){
      |[] => counter;
      |[hd,...tl] when hd == e =>  
        let counter = counter + 1;
        count_helper(tl, e, counter);
      |[_,...tl] => 
        count_helper(tl, e, counter);
    };
  };
  count_helper(l, e, counter);
};

open P1btree;

//read in the root node then recursivley go left down the tree until you cannot
//then go up one to the parent of the farthest left tree then recursivley go right
let pre_order = (t: int_tree) : list(int) => {
  let rec pre_order_helper = (t : int_tree) : list(int) => {
    //ask if i should just pass in two int_trees or if that is fine
    switch(t){
      //FIXME as about the order of the traversal and the syntax for that
      |EmptyTree => [];
      |TreeNode(curr, left, right) => {
        [curr, ...pre_order_helper(left)] @ pre_order_helper(right);
      };
    };
  };
  pre_order_helper(t);
};

 
let int_tree_map = ((f: int => int), (t: int_tree)) : int_tree => {
  let rec tree_map_helper = (t : int_tree) : int_tree => {
    switch(t){
      |EmptyTree => EmptyTree;
      |TreeNode(curr, left, right) => {
        TreeNode(f(curr), tree_map_helper(left), tree_map_helper(right));
      }
    };
  };
  tree_map_helper(t);
};