export R_HOME := $(shell R RHOME)
export LD_LIBRARY_PATH := $(R_HOME)/lib:$(shell find -L $(JAVA_HOME) -name libjvm.so -printf %h)
export JAVA_LIBRARY_PATH := $(R_HOME)/library/rJava/jri
CLASSPATH := .:JRI.jar:commons-lang3-3.0-beta.jar
FRAGMENTS := test-java-function.R \
	TestInterfaceProxy.java \
	TrivialFunction.java

.PHONY: test-interface-proxy test-java-function

test-interface-proxy: TestInterfaceProxy.java
	javac -classpath $(CLASSPATH) TestInterfaceProxy.java && \
	java -enableassertions \
		-Djava.library.path=$(JAVA_LIBRARY_PATH) \
		-classpath $(CLASSPATH) \
		TestInterfaceProxy

TrivialFunction.class: TrivialFunction.java
	javac TrivialFunction.java

test-java-function: test-java-function.R TrivialFunction.class
	./test-java-function.R

$(FRAGMENTS): TODO
	org-tangle TODO
