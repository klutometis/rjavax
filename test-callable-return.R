#!/usr/local/bin/R --vanilla --slave -f

library(rJava)

.jinit(classpath='inst/java/JRIEngine.jar:inst/java/JRI.jar:inst/java/REngine.jar:inst/java/rJavax.jar:src')
.jengine(TRUE)

dollarsToJava <- function(refClass, method)
  toJava(do.call(`$`, list(refClass, method)))

testClass <- setRefClass('testClass',
                         ## We can refer to .self even in Java!
                         methods=list(test=function() .self))

J('TestCallableReturn')$test(toJava(dollarsToJava),
                             toJava(testClass$new()),
                             'test')
