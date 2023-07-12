JAVA_HOME := $(shell java -XshowSettings:properties -version 2>&1 | grep 'java.home' | cut -d "=" -f 2 | xargs)

CLASS = NativeOTP
LIB := $(shell if [[ "$OSTYPE" == "linux-gnu"* ]]; then echo "libotp.so"; else echo "libotp.dylib"; fi)

all: $(CLASS).class $(LIB)

run: all
	$(JAVA_HOME)/bin/java -cp . -Djava.library.path="$(pwd)" NativeOTP

$(CLASS).class: $(CLASS).java
	$(JAVA_HOME)/bin/javac -h . $(CLASS).java

$(LIB): $(CLASS).c $(CLASS).class
	gcc -I $(JAVA_HOME)/include -I $(JAVA_HOME)/include/darwin -arch x86_64 -O3 -fPIC -shared -o $(LIB) $(CLASS).c

clean:
	rm -rf *.so *.dylib *.class $(CLASS).h 

.phony: all clean run
