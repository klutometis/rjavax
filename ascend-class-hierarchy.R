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
  str(sys.function(sys.parent(n=3)))
  str(structure(Map(function(promise)
                    tryCatch(eval(promise, envir=where),
                             error=function(e) e),
                    promises),
                names=Map(deparse, promises)))
}

Delegate <-
  setRefClass('Delegate',
              fields='object',
              methods=c(initialize=function(class='java.lang.Object', ...) {
                debug(class)
                object <<- new(J(class), ...)
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
             contains <-
               if (is.null(superclass))
                 'Delegate'
                 ## NULL
               else {
                 superclassName <- superclass$getName()
                 setJavaRefClass(superclassName)
                 superclassName
               }
             debug(className, contains)
             declaredMethods <-
               Map(function(method) method$getName(),
                   as.list(class$getDeclaredMethods()))
             methods <-
               structure(Map(function(method)
                             ## TODO: Check for JavaObjects and
                             ## extract the ref; otherwise: pass
                             ## through.
                             eval(substitute(function(...) {
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
                               if (hasMethod(.self$object, method))
                                 .jrcall(.self$ref, method, ...)
                             },
                                             list(method=method))),
                             declaredMethods),
                         names=declaredMethods)
             setRefClass(className,
                         contains=contains,
                         fields='ref',
                         methods=c(methods,
                           initialize=eval(substitute(function(...) {
                              ## ref <<- new(J(className), ...)
                             debug('harro', className, ...)
                             callSuper(className, ...)
                              .self
                           },
                              list(className=className))),
                           NULL)
             )
           })

File <- setJavaRefClass('java.io.File')
File$new()
## stopifnot(File$new('/tmp')$getPath() == '/tmp')

## java.lang.Object is automagically defined as the parent of
## java.io.file.
## Object <- getJavaRefClass('java.lang.Object')
## stopifnot(typeof(Object$new()$hashCode()) == 'integer')
