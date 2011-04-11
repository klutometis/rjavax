CLASSES := $(patsubst %.java,%.class,$(wildcard *.java))

%.class : %.java
	javac $<

all: $(CLASSES)
