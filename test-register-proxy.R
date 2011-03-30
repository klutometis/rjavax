#!/usr/local/bin/R --vanilla --slave -f

library(rJava)
.jinit(classpath=Sys.getenv('CLASSPATH'),
       parameters=sprintf('-Djava.library.path=%s',
         Sys.getenv('JAVA_LIBRARY_PATH')))
.jengine(TRUE)

to.hashmap <- function(implementations) {
  hashmap <- new(J('java.util.HashMap'))
  ## Really need a Foreach here, since we're not using the return
  ## value.
  implementations <- c(toString=function() "RInterfaceProxy",
                       implementations)
  Map(function(name, implementation)
      hashmap$put(name, toJava(implementation)),
      names(implementations),
      implementations)
  hashmap
}

proxy <- new(J('RInterfaceProxy'),
  'TrivialInterface',
  to.hashmap(list(method=function() cat('hello world'))))

stopifnot(capture.output(proxy$newInstance()$method()) ==
          "hello world")
