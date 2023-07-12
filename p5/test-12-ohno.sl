// 139ba267302516bcc9e9e23141d94ac04cd56a91
// Adapted from Wes Weimer's "hairy-scary"

// oh

class Foo : Bazz {
    let a = if (self.is_a("Bar")) { self; } else { 
        if (self.is_a("Razz")) { new Bar; } else {
            if (self.is_a("Foo")) { new Razz; } else {
                abort();
            };
        };
    };
    
    let b = a.doh() + g.doh() + doh() + printh();

    doh() {
        let i = h;
        h = h + 2;
        i;
    };
};

class Bar : Razz {
    let c = doh();
    let d = printh();
};

class Razz : Foo {
    let e = if ( self.is_a("Bar")) { self; } else {
        if (self.is_a("Razz")) {
            new Bar;
        } else {
            abort();
        };
    };

    let f = a@Bazz.doh() + g.doh() + e.doh() + doh() + printh();
};

class Bazz : IO {
    let h = 1;
    let g = if (self.is_a("Bar")) { self; } else {
        if (self.is_a("Razz")) { new Bar; } else {
            if (self.is_a("Foo")) { new Razz; } else {
                if (self.is_a("Bazz")) { new Foo; } else {
                    abort();
                };
            };
        };
    };

    let i = printh();

    printh() {
        print_int(h);
        0;
    };

    doh() {
        let i = h;
        h = h + 1;
        i;
    };
};

// no
class Main : IO {
    let a = new Bazz;
    let b = new Foo;
    let c = new Razz;
    let d = new Bar;

    main() {
        print_string("\n");
        "do nothing";
    };
};