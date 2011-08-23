SRC = RInterfaceProxy.java
CLASS = RInterfaceProxy.class
CLASSPATH = '../inst/java/*'
DIST = ../inst/java/rJavax.jar

$(CLASS): $(SRC)
	$(JAVAC) -classpath $(CLASSPATH) $<

all: $(DIST)

$(DIST): RInterfaceProxy.class
	$(JAR) cvf $@ *.class

clean:
	rm -vf *.class
