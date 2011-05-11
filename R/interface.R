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

##' Implement the Java-\code{interface} with a \code{refClass}-based
##' \code{implementation}.
##' @param interface the name of the to-be-implemented Java interface
##' @param implementation an instance of the implementing \code{refClass}
##' @return a \code{refClass} implementing the Java \code{interface}
##' @note \code{implementation} takes an instance of the
##' \code{refClass}, not the \code{refClass} itself; allowing one to
##' make use of peculiar initializers.
##' @export
setJavaInterfaceImplementation <- function(interface,
                                           implementation)
  ## Should we use a class-name based on the implementation/interface?
  setRefClass('jobjInterfaceRef',
              contains=implementation@.xData$.refClassDef@className,
              fields='proxy',
              methods=list(initialize=eval(substitute(function() {
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
