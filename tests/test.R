library(rJavax)

system('make')

.jpackage('rJavax', morePaths='.')
.jengine(TRUE)

interface <-
  interfaceProxy('TestInterface',
                 list(mogrify=function(string)
                      paste(string, 'mogrified', sep='-'),
                      tetradicSum=function(w, x, y, z)
                      sum(w, x, y, z),
                      multiplyInts=function(x, y)
                      as.integer(x * y),
                      stack=function() {
                        stack <- new(J('java/util/Stack'))
                        stack$push("hello, stack")
                        stack
                      }))

stopifnot(interface$mogrify('totally') == 'totally-mogrified')

stopifnot(interface$tetradicSum(1, 2, 3, 4) == 10)

stopifnot(interface$multiplyInts(as.integer(2), as.integer(4)) == 8)

stopifnot(interface$stack()$pop() == "hello, stack")
