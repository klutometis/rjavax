# Alternatively, <http://blog.jgc.org/2007/06/escaping-comma-and-space-in-gnu-make.html>
CLASSPATH := .:$(shell echo ../inst/java/*.jar | tr ' ' ':')
CLASSES := $(patsubst %.java,%.class,$(wildcard *.java))
JAR := ../inst/java/rJavax.jar

.PHONY: clean

%.class : %.java
	javac -classpath $(CLASSPATH) $<

all: $(JAR)

$(JAR): $(CLASSES)
	jar cvf $@ *.class			# Glob here for anonyous subclasses.

clean:
	rm -vf *.class
