// 139ba267302516bcc9e9e23141d94ac04cd56a91


class A2I {

    /*
     * c2i converts a 1-character string to an integer.  Aborts if the string 
     * is not "0" through "9"
     */
    c2i(char) {
        if (char == "0") {
            0;
        } else {
            if (char == "1") {
                1;
            } else {
                if (char == "2") {
                    2;
                } else {
                    if (char == "3") {
                        3;
                    } else {
                        if (char == "4") {
                            4;
                        } else {
                            if (char == "5") {
                                5;
                            } else {
                                if (char == "6") {
                                    6;
                                } else {
                                    if (char == "7") {
                                        7;
                                    } else {
                                        if (char == "8") {
                                            8;
                                        } else {
                                            if (char == "9") {
                                                9;
                                            } else {
                                                abort();
                                            };
                                        };
                                    };
                                };
                            };
                        };
                    };
                };
            };
        };
    };

    /*
     * convert an Int to a String
     */
    i2c(i) {
        if (i == 0) {
            "0";
        } else {
            if (i == 1) {
                "1";
            } else {
                if (i == 2) {
                    "2";
                } else {
                    if (i == 3) {
                        "3";
                    } else {
                        if (i == 4) {
                            "4";
                        } else {
                            if (i == 5) {
                                "5";
                            } else {
                                if (i == 6) {
                                    "6";
                                } else {
                                    if (i == 7) {
                                        "7";
                                    } else {
                                        if (i == 8) {
                                            "8";
                                        } else {
                                            if (i == 9) {
                                                "9";
                                            } else {
                                                abort();
                                            };
                                        };
                                    };
                                };
                            };
                        };
                    };
                };
            };
        };
    };


    a2i(s) {
        if (s.length() == 0) { 0; }
        else {
            let fst = s.substr(0,1);
            let rst = s.substr(1, s.length()-1);
            if (fst == "-") {
                ~a2i_helper(rst);
            } else {
                if (s.substr(0,1) == "+") {
                    a2i_helper(rst);
                } else {
                    a2i_helper(s);
                };
            };
        };
    };

   
    a2i_helper(s) {
        let int = 0;
        let j = s.length();
        let i = 0;
        while (i < j) {
            // shift by ten and get the next digit
            int = int * 10 + c2i(s.substr(i, 1));
            i = i + 1;
        };
        int;
    };

    /*
     * i2a converts an integer to a string. Positive and negative numbers 
     * are handled correctly.
     */
    i2a(i) {
        if (i == 0) { "0"; } 
        else {
            if (0 < i) {
                i2a_helper(i);
            } else {
                "-".concat(i2a_helper(i * ~1));
            };
        };
    };

   
    i2a_helper(i) {
        if (i == 0) { ""; }
        else {
            let next = i / 10;
            // convert the last digit and prepend the rest of the number
            i2a_helper(next).concat(i2c(i - next * 10));
        };
    };

};

class Main : IO {
    // test a string (a) and int (b)
    test_pair(a, b) {
        a = (new A2I).a2i(a);
        b = (new A2I).i2a(b);
        print_int(a);
        print_string(" == ");
        print_string(b);
        print_string("\n");
    };

    main() {
        test_pair("123456790", 1234567890);
        test_pair("+123456790", 1234567890);
        test_pair("-123456790", ~1234567890);
        test_pair("0", 0);
    };
};