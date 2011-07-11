getJavaRefClass <- getRefClass

## Should we memoize this somewhere? Requires an expensive call into
## Java.
hasMethod <- function(referent, method)
  .jcall("RJavaTools",
         "Z",
         "hasMethod",
         .jcast(referent, "java/lang/Object" ),
         method)

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
             
             declaredMethods <-
               as.character(unlist(Map(function(method) method$getName(),
                                       as.list(class$getDeclaredMethods()))))
             
             methods <- sapply(declaredMethods, function(method) {
               eval(substitute(function(...) {
                 arguments <- base::Map(function(argument) {
                   if (methods::is(argument, 'java.lang.Object')) {
                     argument$ref
                   } else
                   argument
                 }, base::list(...))
                 
                 ##
                 ## java.lang.Object will not have
                 ## e.g. finalize() under certain
                 ## conditions (namely,
                 ## superclass-initialization when
                 ## creating a generator object).
                 ##
                 ## I notice that finalize() is called
                 ## twice; could it be calling
                 ## finalize() on a methodless zombie?
                 ##
                 ##debug(.self, .self$ref, method, arguments, call.stack=FALSE)
                 if (hasMethod(.self$ref, method))
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
