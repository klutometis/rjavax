#!/usr/local/bin/R --vanilla --slave -f

library(functional)
source('preamble.R')

Delegate <-
  setRefClass('Delegate',
              fields=list(ref='jobjRef',
                implements='character'),
              methods=c(initialize=function(...) {
                ## This bothers me; I realize it's the base-case in
                ## our recursion: but why not allow java.lang.Object
                ## to be the base-case?

                ## Do this.
                class <- class(.self)
                if (!(class == 'Delegate'))
                  if (J('java.lang.Class')$forName(class)$isInterface()) {
                    if (length(.self$implements))
                      ref <<- interfaceProxy(.self$implements, .self)
                  } else
                    ref <<- new(J(class(.self)), ...)
                .self
              })
              )

getJavaRefClass <- getRefClass

## Should we memoize this somewhere? Requires an expensive call into
## Java.
hasMethod <- function(referent, method)
  .jcall("RJavaTools",
         "Z",
         "hasMethod",
         .jcast(referent, "java/lang/Object" ),
         method)

setJavaRefClass <- function(className)
  tryCatch(getJavaRefClass(className),
           error=function(e) {
             class <- J('java.lang.Class')$forName(className)             
             superclass <- class$getSuperclass()
             superclassName <- 
               if (is.null(superclass))
                 ## NULL
                 'Delegate'
               else
                 superclass$getName()
             
             if (!is.null(superclass)) 
               setJavaRefClass(superclassName)
             
             interfaces <- Map(function(interface) interface$getName(),
                               as.list(class$getInterfaces()))

             ## setJavaRefClass on an interface, really? Apparently.
             for (interface in interfaces)
               setJavaRefClass(interface)

             ## DONE: this must also include the interfaces, sorted
             ## lexicographically, iff the class explicitly implements
             ## the interface (not through inheritance).
             contains <- sort(unlist(c(superclassName, interfaces)))

             declaredMethods <-
               Map(function(method) method$getName(),
                   as.list(class$getDeclaredMethods()))

             methods <-
               structure(Map(function(method)
                             eval(substitute(function(...) {
                               arguments <-
                                 Map(function(argument) {
                                   if (inherits(argument, 'java.lang.Object')) {
                                     ## Controversy starts here: do we
                                     ## take the ref or the proxy?
                                     ## Maybe that's why we can't have
                                     ## refs and proxies; in Java
                                     ## they're one and the same.
                                     ##
                                     ## Even if we have an interface
                                     ## with a ref; there's the
                                     ## problem of whether or not to
                                     ## take the object or the ref,
                                     ## isn't there?
                                     ##
                                     ## No; let's take the ref, and
                                     ## delegate the functions (if
                                     ## need be) to a delegate; the
                                     ## delegate can be invisible,
                                     ## can't it?
                                     argument$ref
                                   } else
                                     argument
                                 },
                                     ## Wow, dudes: list(...)
                                     ## segfaults on niladic
                                     ## evaluation; sweet!
                                     c(...))
                               
                               ## Memoize this somewhere? Also: what
                               ## to do when the method doesn't exist:
                               ## aren't there legitimate cases to
                               ## propagate the error?
                               ##
                               ## Anyway, the bizarre thing is that
                               ## e.g. java.lang.Object will not have
                               ## e.g. finalize() under certain
                               ## conditions (namely,
                               ## superclass-initialization when
                               ## creating a generator object).
                               ##
                               ## Why? No idea.
                               ##
                               ## I notice that finalize() is called
                               ## twice; could it be calling
                               ## finalize() on a methodless zombie?
                               ##
                               ## Is it problematic that object doesn't change?
                               ## What if they change the ref on the class? Let's
                               ## make object a thunk.
                               debug(.self,
                                     .self$ref,
                                     method,
                                     arguments,
                                     call.stack=FALSE)
                               if (hasMethod(.self$ref, method))
                                 do.call(.jrcall, c(.self$ref,
                                                    method,
                                                    arguments))
                             },
                                             list(method=method))),
                             declaredMethods),
                         names=declaredMethods)

             setRefClass(className,
                         fields=list(ref='jobjRef'),
                         contains=contains
                         ## methods=c(methods,
                         ##   initialize=eval(substitute(function(...) {
                         ##     debug(className,
                         ##           list(...),
                         ##           call.stack=FALSE)
                         ##     if (J('java.lang.Class')$forName(className)$isInterface())
                         ##       ref <<- interfaceProxy(.self$implements, .self)
                         ##     else
                         ##       ref <<- new(J(className), ...)
                         ##     .self
                         ##   },
                         ##     list(className=className))))
                         )
           })

File <- setJavaRefClass('java.io.File')
## Calling a Java method (the File-constructor) with a native R type
## (character):
stopifnot(File$new('/tmp')$getPath() == '/tmp')

## java.lang.Object is automagically defined as the parent of
## java.io.file.
Object <- getJavaRefClass('java.lang.Object')
o1 <- Object$new()
o2 <- Object$new()
## Calling a Java method with a javaRefClass (extraction of the ref
## happens behind-the-scenes):
stopifnot(o1$equals(o1))
stopifnot(!o1$equals(o2))
