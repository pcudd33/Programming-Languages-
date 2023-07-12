// 201967cc8d6a85b5befe76602ccbb86bd55df57f
// All valid tokens
class Main : IO {
    let a = new[10] Array;
    let w = new Other;
    let x = 10 * (5 + 34) - 15 / 3;
    let y = 2;
    let z = false;
    func(x, y) {
        while (y < x) {
            x = x / (1 / 5);
            y = y * 10;
        };
        x;
    };

    disfunc(x, y) {
        if (y <= x) {
            x = 5 - 4;
            z = true;
        } else {
            x = 3 / 3;
            z = false;
        };
        z;
    };

    main() {
        print_string((new String).get_type().substr(0, 1));
        print_string(self.get_type().substr(3, 1));
        print_string(self.get_type().substr(1, 2));
        print_string((isvoid(self)).get_type().substr(3, 1));
        print_string("\n");
        g.addValue("hello!");
    };
};

class Other : IO {
    let values = new List;
    addValue(name) {
        if (isvoid(getValue(name))) {
            values.add(name);
        } else {
            values.size();
        };
    };
};