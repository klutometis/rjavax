#!/usr/local/bin/R --vanilla --slave -f

library(rJava)
.jinit(classpath=Sys.getenv('CLASSPATH'),
       parameters=sprintf('-Djava.library.path=%s',
         Sys.getenv('JAVA_LIBRARY_PATH')))
.jengine(TRUE)

merge.lists <- function(...) {
  list = do.call(c, list(...))
  list[unique(names(do.call(c, list)))]
}

to.hashmap <- function(implementations) {
  hashmap <- new(J('java.util.HashMap'))

  ## Some things that Java expects of Objects.
  default.implementations <- list(toString=function()
                                  "RInterfaceProxy")

  implementations <-
    merge.lists(implementations,
                default.implementations)
  
  ## Really need a Foreach here, since we're not using the return
  ## value.
  Map(function(name, implementation)
      hashmap$put(name, toJava(implementation)),
      names(implementations),
      implementations)
  hashmap
}

proxy <- new(J('RInterfaceProxy'),
  'Mogrifier',
  to.hashmap(list(mogrify=function(string)
                  paste(string, 'mogrified', sep='-'),
                  tetradicSum=function(w, x, y, z)
                  sum(w, x, y, z))))

mogrifier <- proxy$newInstance()

stopifnot(mogrifier$mogrify('totally') == 'totally-mogrified')

stopifnot(mogrifier$tetradicSum(new(J('java.lang.Integer'), '1'),
                                new(J('java.lang.Integer'), '2'),
                                new(J('java.lang.Integer'), '3'),
                                new(J('java.lang.Integer'), '4'))
          == 10)

stopifnot(mogrifier$tetradicSum(1, 2, 3, 4)
          == 10)
