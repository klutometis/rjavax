#!/usr/local/bin/R --vanilla --slave -f

source('ascend-class-hierarchy.R')

dollarsToJava <- function(object, method)
  toJava(do.call(`$`, list(object, method)))

interfaceProxy <- function(interface, implementation)
  new(J('RInterfaceProxy'),
      interface,
      toJava(dollarsToJava),
      toJava(implementation))$newInstance()

### FIXME: we need to check that a method is provided for each interface method
setJavaImplementation <- function(...,
                                  methods = list(),
                                  contains = character(),
                                  implements = character(),
                                  extends = "java.lang.Object")
{
  for (implement in implements)
    setJavaRefClass(implement)

  if (!identical(extends, "java.lang.Object"))
    stop("Currently, it is only possible to extend 'java.lang.Object'")
  
  delegateMethods <-
    Map(function(method) method$getName(),
        as.list(J('java.lang.Class')$forName(extends)$getMethods()))

  delegateMethods <-
    sapply(as.character(unlist(delegateMethods)), function(delegateMethod)
           eval(substitute(function(...) {
             `$`(delegate, delegateMethod)(...)
           }, list(delegateMethod=delegateMethod))))

  initialize <- methods$initialize
  methods$initialize <- NULL

  delegateMethods[names(methods)] <- methods
  
  setRefClass(...,
              contains=c(contains, extends, implements),
              fields=list(delegate='jobjRef'),
              methods=c(delegateMethods,
                initialize=eval(substitute(function(...) {
                  delegate <<- new(J(extends))
                  ref <<- interfaceProxy(implements, .self)
                  if (hasInitialize)
                    initialize(...)
                  else .self
                }, list(implements = implements,
                        initialize = initialize,
                        hasInitialize = !is.null(initialize),
                        extends = extends)))))
}

Comparable <-
  setJavaImplementation('Comparable',
                        implements='java.lang.Comparable',
                        methods=c(compareTo=function(object) as.integer(0)))

OverriddenComparable <-
  setJavaImplementation('OverriddenComparable',
                        implements='java.lang.Comparable',
                        methods=c(compareTo=function(object) as.integer(0),
                          hashCode=function() -1))

comparable <- Comparable$new()
comparable$compareTo(Object$new())
## These should (and do) yield the same result:
comparable$hashCode()
comparable$ref$hashCode()

overriddenComparable <- OverriddenComparable$new()
overriddenComparable$hashCode()
## overriddenComparable$ref$compareTo(Object$new())
