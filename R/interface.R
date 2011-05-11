dollarsToJava <- function(refClass, method)
  toJava(do.call(`$`, list(refClass, method)))

##' Instantiate a Java-based \code{interface}-proxy for the
##' \code{implementation}.
##' @param interface the name of the to-be-implemented Java interface
##' @param implementation an instance of the implementing \code{refClass}
##' @return a suitable \code{RInterfaceProxy}
##' @import rJava
##' @export
interfaceProxy <- function(interface, implementation)
  new(J('RInterfaceProxy'),
      interface,
      toJava(dollarsToJava),
      toJava(implementation))$newInstance()

proxyMethods <- function(class) {
  method.names <- unique(Map(function(method) method$getName(),
                             as.list(J('java.lang.Class')
                                     $forName(class)
                                     $getMethods())))

  proxyObject <- new(J(class)) 

  structure(Map(function(method.name)
                ## Have to force evaluation because `method.name'
                ## and `proxyObject' aren't defined from the
                ## refClass.
                eval(substitute(function(...) {
                  .jrcall(proxyObject, method.name, ...)
                },
                                list(method.name=method.name,
                                     proxyObject=proxyObject))),
                method.names),
            names=method.names)
}

##' Implement the Java-\code{interface} with a \code{refClass}-based
##' \code{implementation}.
##' @param interface the name of the to-be-implemented Java interface
##' @param implementation an instance of the implementing \code{refClass}
##' @param base base Java class from which to inherit function
##' (default \code{java.lang.Object})
##' @return a \code{refClass} implementing the Java \code{interface}
##' @note \code{implementation} takes an instance of the
##' \code{refClass}, not the \code{refClass} itself; allowing one to
##' make use of peculiar initializers.
##' @export
setJavaInterfaceImplementation <- function(interface,
                                           implementation,
                                           base='java.lang.Object')
  ## Should we use a class-name based on the implementation/interface?
  setRefClass('jobjInterfaceRef',
              contains=c(implementation@.xData$.refClassDef@className),
              fields='proxy',
              methods=c(eval(substitute(proxyMethods(base),
                list(base=base))),
                initialize=eval(substitute(function() {
                  proxy <<-
                    interfaceProxy(interface, implementation)
                  .self
                }),
                  list(interface=interface,
                       implementation=implementation))),
              ## Running into "cannot add bindings to a locked
              ## environment" without this; is something fundamentally
              ## wrong with our approach?
              ##
              ## See TODO: we'll pollute the namespace with
              ## dynamically generated names based on e.g.
              where=topenv(parent.frame()))
