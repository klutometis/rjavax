#!/usr/local/bin/R --vanilla --slave -f

options(morePaths='tests')

library(rJavax)

setJavaInterfaceImplementation <- function(implementation)
  setRefClass('jobjInterfaceRef',
              contains=implementation$className,
              fields='proxy',
              methods=list(initialize=function(interface,
                             implementation,
                             ...) {
                implementations <-
                  structure(Map(function(name) {
                    get(name, implementation$def@refMethods)
                  },
                                implementation$methods()),
                            names=implementation$methods())
                proxy <<-
                  interfaceProxy(interface, implementations)
                .self
              }))

parent <- setRefClass('parentharro',
                      methods=list(mogrify=function(string) 'harro!'))

testInterfaceImplementation <-
  setRefClass('testInterfaceImplementation',
              contains=parent,
              methods=list(mogrify=function(string) {
                print(callSuper('yes!'))
                paste(string, 'mogrified', sep='-')
              },
                tetradicSum=function(w, x, y, z)
                sum(w, x, y, z),
                multiplyInts=function(x, y)
                as.integer(x * y)))

testInterface <-
  setJavaInterfaceImplementation(testInterfaceImplementation)

ti <- testInterface$new('TestInterface',
                        testInterfaceImplementation)

stopifnot(ti$mogrify('test') == ti$proxy$mogrify('test'))
