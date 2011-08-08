##' Get a refClass-based proxy from the given class-name.
##' @param Class the class-name whose proxy to retrieve
##' @param where the environment in which to get it
##' @return the proxy or NULL
##' @export
getJavaRefClass <- getRefClass

##' Create a refClass-based proxy for Java classes.
##' @param className the class to be proxied
##' @param where environment to define the refClass in
##' @return a refClass-based Java-class-proxy.
##' @export
setJavaRefClass <- function(className,
                            where=topenv(parent.frame()))
  tryCatch(getJavaRefClass(className),
           error=function(e) {
             class <- J('java.lang.Class')$forName(className)

             superclass <- class$getSuperclass()
             superclassName <- NULL
             if (!is.null(superclass)) {
               superclassName <- superclass$getName()
               setJavaRefClass(superclassName, where)
             }
             
             interfaces <- Map(function(interface) interface$getName(),
                               as.list(class$getInterfaces()))

             for (interface in interfaces)
               setJavaRefClass(interface, where)

             ## sort the interfaces lexicographically to avoid inconsistencies
             contains <- c(superclassName,
                           sort(as.character(unlist(interfaces))))

             isAbstract <- function(class) {
               J('java.lang.reflect.Modifier')$isAbstract(class$getModifiers())
             }
             ## if an interface or an abstract class, need to contain VIRTUAL
             if (class$isInterface() || isAbstract(class))
               contains <- c(contains, "VIRTUAL")

             notProtected <- function(m) {
               !J('java.lang.reflect.Modifier')$isProtected(m$getModifiers())
             }
             
             declaredMethods <-
               Map(function(method) method$getName(), 
                   Filter(notProtected, as.list(class$getDeclaredMethods())))
             
             methods <- sapply(as.character(declaredMethods), function(method) {
               eval(substitute(function(...) {
                 arguments <- Map(function(argument) {
                   if (is(argument, 'java.lang.Object')) {
                     argument$ref
                   } else
                   argument
                 }, list(...))                 
                 do.call(.jrcall, c(.self$ref, method, arguments))
               }, list(method=method)))
             })

             if (className == "java.lang.Object")
               setRefClass("java.lang.Object",
                           fields = list(ref = 'jobjRef'),
                           methods = c(methods,
                             initialize = function(...) {
                               ref <<- new(J(class(.self)), ...)
                               .self
                             },
                             copy = function(shallow = FALSE) {
                               ## unlike clone(), this preserves any
                               ## fields that may be present in
                               ## an R-specific subclass
                               x <- callSuper(shallow)
                               x$ref <- ref$clone()
                               x
                             }),
                           contains = contains,
                           where = where)
             else setRefClass(className,
                              methods = methods,
                              contains = contains,
                              where = where)
           })
