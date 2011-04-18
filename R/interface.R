##' Merge named lists, where previous values take precedent in the
##' case of duplicate keys.
##' @param ... lists to be merged
##' @return merged lists
merge.lists <- function(...) {
  list = do.call(c, list(...))
  list[unique(names(do.call(c, list)))]
}

##' Convert a named list of functions to a hashmap, preserving the
##' \code{REXPReference} property.
##' @param implementations named functions to be preserved
##' @return the resultant hashmap
to.hashmap <- function(implementations) {
  hashmap <- new(J('java.util.HashMap'))

  ## Some things that Java expects of Objects.
  default.implementations <-
    list(toString=function() "RInterfaceProxy")

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

##' Implement a given Java interface.
##' @param interface the name of the to-be-implemented Java interface,
##' e.g. \code{'life.eukarya.opisthokonta.animalia'}
##' @param implementations a named list of functions which implement
##' the corresponding interface-method,
##' e.g. \code{list(eat=function(food) { ... })}
##' @return a nullary constructor which, when invoked, instantiates
##' the implementation; e.g. \code{animalium()$eat(prey)}
##' @export
##' @import rJava
interfaceProxy <- function(interface, implementations)
  new(J('RInterfaceProxy'),
      interface,
      to.hashmap(implementations))$newInstance()
