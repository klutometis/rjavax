## This is how we'd normally handle adding paths to the class-loader,
## but the test-environment doesn't seem to invoke .onLoad().
## options(morePaths='.')
library(rJavax)

system('make')

.jpackage('rJavax', morePaths='.')
.jengine(TRUE)

File <- setJavaRefClass('java.io.File')
## Calling a Java method (the File-constructor) with a native R type
## (character):
## stopifnot(File$new('/tmp')$getPath() == '/tmp')

## java.lang.Object is automagically defined as the parent of
## java.io.file.
Object <- getJavaRefClass('java.lang.Object')
o1 <- Object$new()
o2 <- Object$new()
## Calling a Java method with a javaRefClass (extraction of the ref
## happens behind-the-scenes):
stopifnot(o1$equals(o1))
stopifnot(!o1$equals(o2))

Comparable <-
  setJavaImplementation('Comparable',
                        implements='java.lang.Comparable',
                        methods=c(compareTo=function(object) as.integer(0)))

OverriddenComparable <-
  setJavaImplementation('OverriddenComparable',
                        implements='java.lang.Comparable',
                        methods=c(compareTo=function(object) as.integer(0),
                          hashCode=function() -1L))

comparable <- Comparable$new()
comparable$compareTo(Object$new())
## These should (and do) yield the same result:
comparable$hashCode()
comparable$ref$hashCode()

overriddenComparable <- OverriddenComparable$new()
overriddenComparable$hashCode()
## overriddenComparable$ref$compareTo(Object$new())
