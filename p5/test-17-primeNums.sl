// 139ba267302516bcc9e9e23141d94ac04cd56a91

class Main : IO {
    // out is our "output".  Its values are the primes.
    let out = {
        print_string("2 is trivially prime.\n");
        2;
    };

    // testee is a number to be tested for primeness
    let testee = out;

    // divisor is a number which may factor testee
    let divisor = 0;

    // stop is an arbitrary value limiting testee
    let stop = 500;

    // m supplants the main method
    let m = while(true) {
        testee = testee + 1;
        divisor = 2;

        while (
            if (testee < divisor * divisor) {
                false; // can stop is divisor > sqrt(testee)
            } else {
                if (testee - divisor*(testee/divisor) == 0) {
                    false; // can stop if divisor divides testee
                } else {
                    true;
                };
            } // no semi-colon because this isn't a block of code
        ) {
            divisor = divisor + 1;
        };

        // which reason did we stop?
        if (testee < divisor * divisor) {
            // testee has no factors less than sqrt(testee)
            out = testee;  // can think of out itself as the output
            print_int(out);
            print_string(" is prime.\n");
        } else {
            // the loop halted on testee/divisor == 0, testee isn't prime.
            0;  // do nothing;
        };

        if (stop <= testee) {
            // "halt" is like a SIGTERM
            "halt".abort();
        } else {
            "continue";
        };
    };
};