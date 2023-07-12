/*
 * This file presents an example of array operations in snail.  It defines 
 * a deque-like data structure that supports addFirst/addLast, reverse, sort,
 * and print operations.
 *
 * The Main class puts all of this code through the following silly 
 * test exercise:
 *
 *  1. Prompt for a number N
 *  2. Generate a list of number 0..N-1
 *  3. Reverse the list 
 *  4. Sort the list 
 *  5. print the sorted list 
 */

class Deque : IO {
    let data = new[10] Array;
    let size = 0;

    // returns the size of the updated deque
    addFirst(el) {
        // first check if we have enough space        
        if (size == data.length()) {
            resize();
        } else {
            false; // do nothing
        };

        // next, shift all of the data
        let i = size;
        while (0 < i) {
            data[i] = data[i-1];
            i = i - 1;
        };

        // set the first element to el
        data[0] = el;

        // increment size
        size = size + 1;
    };

    // returns the size of the updated deque
    addLast(el) {
        // first check if we have enough space        
        if (size == data.length()) {
            resize();
        } else {
            false; // do nothing
        };

        // add to the end
        data[size] = el;
        size = size + 1;
    };

    reverse() {
        let end = size / 2;
        let i = 0;

        while (i < end) {
            let tmp = data[i];
            data[i] = data[size-i-1];
            data[size-i-1] = tmp;
            i = i + 1;
        };

        // return self 
        self;
    };

    /*
     * perform an adaptive insertion sort 
     */
    sort() {
        // shift min item to data[0]
        let i = size - 1;
        while(0 < i) {
            if(data[i] < data[i-1]) {
                let tmp = data[i];
                data[i] = data[i-1];
                data[i-1] = tmp;
            } else { 0; }; // do nothing
            i = i - 1;
        };

        // now perform insertion sort 
        let i = 2; // start at index 2 
        while (i < size) {
            let v = data[i];
            let j = i;
            while (v < data[j-1]) {
                data[j] = data[j-1];
                j = j - 1;
            };
            i = i + 1;
            data[j] = v;
        };

        // return self 
        self;
    };

    // print the list 
    print() {
        let i = 0;
        while (i < size) {
            print_int(data[i]);
            print_string(" ");
            i = i + 1;
        };
        print_string("\n");
    };

    // helper resize function (doubles size)
    resize() {
        let tmp = data;
        data = new[size * 2] Array;

        let i = 0;
        while(i < size) {
            data[i] = tmp[i];
            i = i + 1;
        };
    };

};

class Main : IO {
    // map integer argument n into list 0..n-1
    iota(n) {
        let d = new Deque;
        let i = 0;
        while (i < n) {
            d.addLast(i);
            i = i + 1;
        };
        d;
    };

    main() {
        print_string("How many numbers to sort? ");
        iota(read_int()).reverse().sort().print();
    };
};