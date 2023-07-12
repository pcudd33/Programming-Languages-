//38a1108d19f54a1fab115779487fbbcea3ae1843

//open the p1a module

open P1a;


//c_to_f
Printf.printf("What is the temperature that you want to convert: ");
let c = float_of_string(read_line());

Printf.printf("the temperature in F is:%f\n ", c_to_f(c));


//split tip
//let split_tip = ((price: float), (n: int)) : option(float) => {
Printf.printf("How much money was the total bill: ");
let price = float_of_string(read_line());
Printf.printf("how many people are splitting the bill: ");
let people = int_of_string(read_line());

switch(split_tip(price, people)){
    |Some(value) => 
    Printf.printf("Your bill after being split is: ");
    print_float(value);
    print_string("\n");
    | None => ();
}


//let triangle_area = ((s1: float), (s2: float), (s3: float)) : option(float) => {

Printf.printf("what float would you like S1 equal to: ");
let s1 = float_of_string(read_line());

Printf.printf("what float would you like S2 equal to: ");
let s2 = float_of_string(read_line());


Printf.printf("what float would you like S3 equal to: ");
let s3 = float_of_string(read_line());

switch(triangle_area(s1,s2,s3)){
    |Some(area) =>
    print_string("your triangle area is: ")
    print_float(area)
    print_string("\n");
    |None => ();
}

let  f = (x : int) : int => {
    x * 2 -1;
}

Printf.printf("What number would you like this function to be applied to: ");
let arg = int_of_string(read_line());

Printf.printf("how many times would you like for the function to run: ");
let n = int_of_string(read_line());

Printf.printf("after the function was applied this is your number: %d\n", repeat(f,arg,n));
