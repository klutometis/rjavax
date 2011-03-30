export R_HOME := $(shell R RHOME)
export LD_LIBRARY_PATH := $(R_HOME)/lib:$(shell find -L $(JAVA_HOME) -name libjvm.so -printf %h)
export JAVA_LIBRARY_PATH := $(R_HOME)/library/rJava/jri
# Alternatively, <http://blog.jgc.org/2007/06/escaping-comma-and-space-in-gnu-make.html>
export CLASSPATH := .:$(shell echo *.jar | tr ' ' ':')
FRAGMENTS := test-java-function.R \
	TestInterfaceProxy.java \
	TrivialFunction.java \
	RInterfaceProxy.java \
	Mogrifier.java \
	test-mogrifier.R

%.class : %.java
	javac -classpath $(CLASSPATH) $<

test-mogrifier: RInterfaceProxy.class Mogrifier.class test-mogrifier.R 
	./test-mogrifier.R

test-interface-proxy: TestInterfaceProxy.class
	java -enableassertions \
		-Djava.library.path=$(JAVA_LIBRARY_PATH) \
		-classpath $(CLASSPATH) \
		$(<:.class=)

test-java-function: test-java-function.R TrivialFunction.class
	./test-java-function.R

$(FRAGMENTS): TODO
	org-tangle TODO
