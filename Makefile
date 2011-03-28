export R_HOME := $(shell R RHOME)
export LD_LIBRARY_PATH := $(R_HOME)/lib
JAVA_LIBRARY_PATH := $(R_HOME)/library/rJava/jri
CLASSPATH := .:JRI.jar:commons-lang3-3.0-beta.jar

.PHONY: test-interface-proxy

test-interface-proxy: TestInterfaceProxy.java
	javac -classpath $(CLASSPATH) TestInterfaceProxy.java && \
	java -enableassertions \
		-Djava.library.path=$(JAVA_LIBRARY_PATH) \
		-classpath $(CLASSPATH) \
		TestInterfaceProxy

TestInterfaceProxy.java: TODO
	org-tangle TODO
