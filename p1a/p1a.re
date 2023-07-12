//38a1108d19f54a1fab115779487fbbcea3ae1843

//c_to_f
//multiply by 1.8 and add 32
let c_to_f = (temp: float) : float => {
    ((temp *. 1.8) +. 32.); 
};

//split tip
//inside of a switch statement you use when 
//make a case for if the number is negative
//price is the amount the meal cost and n is the number of ways it needs to be split  
let split_tip = ((price: float), (n: int)) : option(float) => {
  switch (price, n){
      |(price, n) when (price > 0.) && (n > 1) => Some((price +. (price *. 0.2))/. float_of_int(n));
      |_ => None;
  };
};

//triangle area
let triangle_area = ((s1: float), (s2: float), (s3: float)) : option(float) => {
    let s = (s1 +. s2 +. s3)/.2.;
    let area = sqrt(s*.(s-.s1)*.(s-.s2)*.(s-.s3));
 switch(s1, s2, s3){
     |(s1, s2, s3) when (s1 +. s2 > s3) && (s1 +. s3 > s2) && (s3 +. s2 > s1) => Some(area);
     |_ => None;
 };
};

//repeat
let rec repeat = ((f: 'a => 'a), (arg: 'a), (n: int)) : 'a => {
  //let arg = int_of_string(read_line());
  //let f = arg + (arg + 1);
  switch(n){
    |0 => arg;
    |_ => repeat(f, f(arg), n-1);
  }
};

//list length
let rec list_length = (l: list('a)) : int => {
 switch(l){
   |[] => 0;
   |[_hd, ...tl] => 1 + list_length(tl);
 }
};