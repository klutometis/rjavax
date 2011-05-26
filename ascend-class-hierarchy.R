#!/usr/local/bin/R --vanilla --slave -f

library(rJava)
source('R/interface.R')

.jinit(classpath=paste(c(Sys.glob('inst/java/*.jar'), 'src', 'tests'),
         collapse=':'))
invisible(.jengine(TRUE))

## Note that these need to specified as e.g.
## ‘options(error=utils::recover)’ in startup files such as
## ‘.Rprofile’.
options(error=dump.frames)

## Thanks, William Dunlap!
## <https://stat.ethz.ch/pipermail/r-devel/2011-May/061098.html>
debug <- function(..., where=parent.frame()) {
  promises <- as.list(substitute(list(...)))[-1]
  ## If we could get e.g. the calling function
  ## str(sys.function(sys.parent(n=3)))
  ##
  ## In other words, I'd like this to look like:
  ## 
  ## Context (function, environment, etc.)
  ##   expression-0 -> value-0
  ##     [value-0 continued ...]
  ##   expression-1 -> value-1
  ##     [value-1 continued ...]
  ##   ...
  ##   expression-n -> value-n
  ##     [value-n continued ...]
  str(structure(Map(function(promise)
                    tryCatch(eval(promise, envir=where),
                             error=function(e) e),
                    promises),
                names=Map(deparse, promises)))
}

Delegate <-
  setRefClass('Delegate',
              fields=list(ref='jobjRef'),
              methods=c(initialize=function(...) {
                ## This bothers me; I realize it's the base-case in
                ## our recursion: but why not allow java.lang.Object
                ## to be the base-case?
                class <- class(.self)
                if (!(class == 'Delegate' ||
                      J('java.lang.Class')$forName(class)$isInterface()))
                  ref <<- new(J(class(.self)), ...)
                .self
              }))

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
                                   if (inherits(argument, 'java.lang.Object'))
                                     argument$ref
                                   else
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
                               if (hasMethod(.self$ref, method))
                                 do.call(.jrcall, c(.self$ref,
                                                    method,
                                                    arguments))
                             },
                                             list(method=method))),
                             declaredMethods),
                         names=declaredMethods)
             setRefClass(className,
                         contains=contains,
                         methods=methods)
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
