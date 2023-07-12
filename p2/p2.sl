// 38a1108d19f54a1fab115779487fbbcea3ae1843

// Given a list of dependent tasks, this function returns either a valid order in which to perform them (Topological DFS) or the word cycle. 
class Main : IO {
    let g = new Graph;
    let sorted = new List;
    let cyclic = false;
    let cyclicAns = (new Cons).init("cycle", new List);

    // Return a list of the visited nodes in order of visitation using DFS algorithm.
    dfs() {
        // Loop through all unvisited vertices
        let current = selectNode();
        while (!(current == "")) { // While the current line is not empty
            if (cyclic == true) {
                current = "";
            }else{
            visit(current); // Recursive call
            current = selectNode(); // Select unvisited (non-permanently marked) nodee{
            };
        };

        //checks to see if there is a cycle and if so prints cycle 
        //if there is no cycle it retruns the sorted list
        if (cyclic == true){
            cyclicAns;
        }else{
            sorted; // Return the sorted list
        };
        
    }; //dfs

    visit(n) { // Recursive visit for DFS algorithm
        let node = g.getNode(n);
        if (!(cyclic == true )){
            if (node.isVisited()) { // Permanent Mark, do nothing
                false;
            } else { // Continue checking node
                if (node.isTemp()) { // Temporary Mark, cycle detected
                    cyclic = true;
                } else { // No cycle detected, continue
                    node.temp(); // Mark current node with temporary mark
                    let neighbors = node.getNeighbors(); // Identify the neighbors of current node
                    let i = 0;
                    while (i < neighbors.size()) { // Loop through neighbors, recurse on each unvisited node
                        let current = neighbors.get(i);
                        visit(current);
                        if (cyclic == true){
                            i = neighbors.size();
                        }else{
                            i = i + 1; // Iterate i
                        };
                    };

                node.remtemp(); // Remove temporary mark from current node
                node.visit(); // Permanently mark current node
                sorted = sorted.cons(n); // Add current node to head of sorted list
                };
            };    

        }else{
            //if there is a cycle return false and do nothing 
            false;
        };

    }; //visit

    // Pick a node to start from and return it's name.
    selectNode() {
        let i = 0;
        let n;
        let found = false;
        // Iterate through the nodes, select starting point that is not permanently marked
        while (i < g.size()) { 
            n = g.getNodeByIdx(i);
            if (!(n.isVisited())) {
                found = true;
                i = g.size();
            } else {
                i = i + 1;
            };
        };
        if (found) {
            n.getName();
        } else {
            "";
        };

    }; //selectNode

    main() {
        // Loop to read the data
        let done = false;
        while (!done) {
            let dest = read_string(); // Task name
            let src = read_string(); // Name of the task it depends on
            
            // Check second variable to see if it is empty
            if (src.length() == 0) { // If there are no more tasks
                done = true; // We're done reading the input
            } else { // Otherewise, construct nodes within the graph
                // src -> dest
                g.addNode(src);
                g.addNode(dest);
                g.getNode(src).addNeighbor(dest); // Associate the task name with the task it depends on
            };
        };

        // Sorting Reverse Alphabetical
        g.revsort();

        // Perform DFS
        let lst = dfs();
        lst.print();

    }; //main

}; //Main

// Represent a full graph
class Graph : IO {
    let nodes = new ArrayList;
    addNode(name) {
        if (isvoid(getNode(name))) { // Node is not already in graph.
            // Add node and return size.
            nodes.add((new Node).setName(name));
        } else { // Node is already in graph.
            // Return the existing size
            nodes.size();

        };

    }; //addNode

    // Get node via index.
    getNodeByIdx(idx) {
        nodes.get(idx);
    };

    revsort() { // Reverse (ASCII-alphabetical) order sort the nodes of a graph
        nodes.revsort();
        let i = 0;
        while (i < nodes.size()) { // Loop through the neighbors and sort them in forward ASCII-alphabetical order
            let current = nodes.get(i);
            current.sortNeighbors();
            i = i + 1;
        };

    }; //revsort

    size() { nodes.size(); };

    // Get a node that matches by name.
    getNode(nodeName) {
        let i = 0;
        let n;
        let found = false;
        while (i < nodes.size()) {
            n = nodes.get(i);
            if (n.getName() == nodeName) {
                i = nodes.size(); // Break out of the loop
                found = true;
            } else {
                // Increment i
                i = i + 1;
            };
        };
        // Return the node (Node found or node not found).
        if (found) {
            n;
        } else {
            // Return a void value.
            let foo;
            foo;

        };

    }; //getNode

    // Output the graph
    print() {
        let i = 0;
        while (i < nodes.size()) {
            let n = nodes.get(i);
            print_string(n.getName().concat(": "));
            n.getNeighbors().print();
            print_string("\n");
            i = i + 1;
        };

    }; //print

}; //graph

// Store a node in the graph
class Node {
    let name = "";
    let visited = false; // Permanent Mark
    let temp = false; // Temporary Mark
    let neighbors = new ArrayList;

    // Return the name of the node.
    getName() { name; };

    // Set the name of a node and return self object
    setName(theName) { 
        name = theName;
        self;
    };

    visit() { visited = true; };   // Permanent marking
    isVisited() { visited; };      // Check permanent marking status
    temp() { temp = true; };       // Temporary marking
    remtemp() { temp = false; };   // Remove temporary marking
    isTemp() { temp; };            // Check temporary marking status

    // Return the list of neighbor nodes.
    getNeighbors() { neighbors; };

    // Add a neighbor.
    addNeighbor(nodeName) {
        neighbors.add(nodeName);
    };

    // Sort neighbors of current node (forward ASCII-alphabetical order)
    sortNeighbors() {
        neighbors.sort();
    };

}; //Node

// While snail has arrays, there's nothing with dynamic sizing, so let's make a rudimentary ArrayList/Vector
// Classes can appear in any order, so it's possible to place this *after* the Main definition
class ArrayList : IO {
    // We will initially make room for 10 elements
    let data = new[10] Array;
    // The starting size is 0
    let size = 0;

    // Add to the end of the ArrayList
    add(el) {
        // See if we need to resize
        check_resize();

        // Insert the value at the end
        data[size] = el;

        // Increment the size
        // Incidentally, this is also the return value
        size = size + 1;

    }; //add

    // Return size 
    size() { size; };

    // Get a particular index.
    get(i) {
        if (i < 0) {
            // Index too low
            abort();
        } else {
            if (size <= i) {
                // Index too high
                abort();
            } else {
                // Index just right
                data[i];
            };
        };

    }; //get

    // If the ArrayList fills up, we double the number of elements
    check_resize() {
        if (data.length() == size) {
            // The array is full
            
            // Store a temporary copy of the data
            let tmp = data;

            // Make a new array that is twice the length
            data = new[size * 2] Array;

            // Loop through the data and copy it over
            let i = 0;
            while (i < size) {
                // Copy the value from tmp to data
                data[i] = tmp[i];
                // Increment i
                i = i + 1;
            }; 
        } else {
            // The array does not need to be resized
            false;
        };

    }; //check_resize

    // Simple bubble sorting algorithm that works in reverse order 
    revsort() {
        let i = 0;
        while (i < size) {
            let j = size - 1;

            // Bubble sort by bubbling large values to the left
            while (i < j) {
                // If the value to the left is smaller than the current, swap the two values
                if (data[j-1].getName() < data[j].getName()) { // Sorting ASCII chars (reverse order)
                    let tmp = data[j-1];
                    data[j-1] = data[j];
                    data[j] = tmp;
                } else {
                    // Do nothing, current arrangement is correctly ordered
                    false;
                };
                // Decrement j
                j = j - 1;
            }; // while_j

            // Increment i
            i = i + 1;

        }; //while

    }; //revsort

    // Simple bubble sorting algorithm that works in forward order 
    sort() {
        let i = 0;
        while (i < size) {
            let j = size - 1;

            // Bubble sort by bubbling large values to the left
            while (i < j) {
                // If the value to the left is smaller than the current, swap the two values
                if (data[j] < data[j-1]) { // Sorting ASCII chars (forward order)
                    let temp = data[j-1];
                    data[j-1] = data[j];
                    data[j] = temp;
                } else {
                    // Do nothing, current arrangement is correctly ordered
                    false;
                };
                // Decrement j
                j = j - 1;

            };  // while 

            // Increment i
            i = i + 1;
        }; // outer while 

    }; //sort

    // Output the list
    print() {
        let i = 0;
        while (i < size) {
            // Get the item, concatenate a newline, and print
            print_string(data[i].concat(", "));
            i = i + 1;
        };

    }; //print

}; //ArrayList

// Empty list implementation
class List : IO {
    // cons returns a list with the new element at the beginning of the list (the head) and self as the tail (rest).
    cons(hd) {
        (new Cons).init(hd, self);
    };

    // Add to end
    append(el) {
        // Same as adding to the beginning
        cons(el);
    };

    // Illegal for empty list
    head() { abort(); };
    tail() { abort(); };

    // Is empty?
    isEmpty() { true; };

    // Insertion sorting into an empty list, just returns the singleton list
    insert(s) {
        cons(s);
    };

    // print() does nothing for an empty list
    // IO returns self, so let's be consistent
    print() {
        self;
    };
};

// Cons is where all the cool stuff happens (Populating empty/non-empty lists)
// A Cons object is a non-empty list 
class Cons : List {
    let xcar; // xcar is the contents of the head 
    let xcdr; // xcdr is the tail 

    // Add to the end
    append(el) {
        (new Cons).init(xcar, xcdr.append(el));
    };

    // Take off and remove the first element
    tail() { xcdr; };
    head() { xcar; };

    // Is empty?
    isEmpty() { false; };

    // init will populate data into the Cons object and return itself
    init(hd, tl) {
        xcar = hd;
        xcdr = tl;
        self;
    };

    // insert does an insertion sort (using a reverse comparison)
    insert(s) {
        if (!(s < xcar)) { // Reverse order of comparison
            // s is bigger than the current head, so add it to the beginning
            (new Cons).init(s, self);
        } else {
            // Otherwise, pass it down the list to insert
            (new Cons).init(xcar, xcdr.insert(s));
        };
    };

    // Output the list recursively
    print() {
        print_string(xcar.concat("\n"));
        xcdr.print();
    };
};


//int mod = 7 % 2;
//print_int(mod);
