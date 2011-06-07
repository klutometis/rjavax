#!/usr/local/bin/R --vanilla --slave -f

source('ascend-class-hierarchy.R')

dollarsToJava <- function(object, method)
  toJava(do.call(`$`, list(object, method)))

interfaceProxy <- function(interface, implementation)
  new(J('RInterfaceProxy'),
      interface,
      toJava(dollarsToJava),
      toJava(implementation))$newInstance()

### FIXME: We are establishing a reference cycle that will leak memory.
### The Java object owns the R object, and the R object owns the Java object.

### FIXME: we need to check that a method is provided for each interface method
setJavaImplementation <- function(Class,
                                  fields = list(),
                                  contains = character(),
                                  methods = list(),
                                  implements = character(),
                                  extends = "java.lang.Object",
                                  where = topenv(parent.frame()),
                                  ...)
{
  for (implement in implements)
    setJavaRefClass(implement)

  if (!identical(extends, "java.lang.Object"))
    stop("Currently, it is only possible to extend 'java.lang.Object'")

  delegateMethods <-
    Map(function(method) method$getName(),
        as.list(J('java.lang.Class')$forName(extends)$getDeclaredMethods()))

  delegateMethods <-
    sapply(as.character(unlist(delegateMethods)), function(delegateMethod)
           eval(substitute(function(...) {
             `$`(delegate, delegateMethod)(...)
           }, list(delegateMethod=delegateMethod))))

  ## Some methods we need to override

  ## getClass
  interfaces <- lapply(implements, J('java.lang.Class')$forName)
  delegateMethods$getClass <- eval(substitute(function() {
    J("java.lang.reflect.Proxy")$getProxyClass(loader, interfaces)
  }, list(loader = interfaces[[1]]$getClassLoader(),
          interfaces = .jarray(interfaces, "java.lang.Class"))))

  ## toString
  delegateMethods$toString <- eval(substitute(function() {
    paste("R object of class '", name, "', implementing ",
          paste("'", implements, "'", collapse = ", ", sep = ""), sep = "")
  }, list(name = Class, implements = implements)))
  
  ## clone
  delegateMethods$clone <- function() copy()$ref

  initialize <- methods$initialize
  methods$initialize <- NULL

  delegateMethods[names(methods)] <- methods

  fields$delegate <- "jobjRef"
  
  setRefClass(Class,
              fields = fields,
              contains = c(contains, extends, implements),
              methods = c(delegateMethods,
                initialize = eval(substitute(function(...) {
                  delegate <<- new(J(extends))
                  ref <<- interfaceProxy(implements, .self)
                  if (hasInitialize)
                    initialize(...)
                  else .self
                }, list(implements = implements,
                        initialize = initialize,
                        hasInitialize = !is.null(initialize),
                        extends = extends))),
                copy = eval(substitute(function(shallow = FALSE) {
                  x <- callSuper(shallow)
                  ## we do not want to inherit these from the original object
                  x$delegate <- new(J(extends))
                  x$ref <- interfaceProxy(implements, x)
                  x
                }, list(implements = implements, extends = extends)))),
              where = where, ...)
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
