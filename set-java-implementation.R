#!/usr/local/bin/R --vanilla --slave -f

source('ascend-class-hierarchy.R')

### FIXME: why can this not just be `$`(refClass, method)
dollarsToJava <- function(refClass, method)
  toJava(do.call(`$`, list(refClass, method)))

interfaceProxy <- function(interface, implementation)
  new(J('RInterfaceProxy'),
      interface,
      toJava(dollarsToJava),
      toJava(implementation))$newInstance()

### FIXME: we cannot extend anything else but java.lang.Object for now
### Do we even want the argument? If we do want it, we need to check
### it, and it should probably be the last argument.
### FIXME: we need to check that a method is provided for each interface method
setJavaImplementation <- function(...,
                                  methods=NULL,
                                  contains=NULL,
                                  extends='java.lang.Object',
                                  implements) {
  for (implement in implements)
    setJavaRefClass(implement)

  delegate <- getJavaRefClass(extends)$new()

  delegateMethods <-
    Map(function(method) method$getName(),
        as.list(J('java.lang.Class')$forName(extends)$getMethods()))

  delegateMethods <-
    structure(Map(function(delegateMethod)
                  eval(substitute(function(...) {
                    ## again, shouldn't `$`(delegate, delegateMethod)
                    ## work?
                    do.call(`$`, list(delegate, delegateMethod))(...)
                  },
                                  list(delegate=delegate,
                                       delegateMethod=delegateMethod))),
                  delegateMethods),
              names=delegateMethods)

  setRefClass(...,
              contains=c(contains,
                extends,
                implements),
              fields=c(delegate='jobjRef'),
              methods=c(delegateMethods,
                methods,
                initialize=eval(substitute(function(...) {
                  ## What is this for? Don't we need to instantiate
                  ## the delegate?
                  assign('implements', implements, .self)
                  callSuper(...)
                  .self
                },
                  list(implements=implements,
                       delegateMethods=delegateMethods)))))
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
## Wow: hashCode() and ref$hashCode() yield different results!
comparable$hashCode()
comparable$ref$hashCode()

overriddenComparable <- OverriddenComparable$new()
overriddenComparable$hashCode()
## overriddenComparable$ref$compareTo(Object$new())
