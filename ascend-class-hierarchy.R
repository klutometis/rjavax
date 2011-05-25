#!/usr/local/bin/R --vanilla --slave -f

library(rJava)
source('R/interface.R')

.jinit(classpath=paste(c(Sys.glob('inst/java/*.jar'), 'src', 'tests'),
         collapse=':'))
invisible(.jengine(TRUE))

## Thanks, William Dunlap!
## <https://stat.ethz.ch/pipermail/r-devel/2011-May/061098.html>
debug <- function(..., where=parent.frame()) {
  promises <- as.list(substitute(list(...)))[-1]
  str(structure(Map(function(promise)
                    tryCatch(eval(promise, envir=where),
                             error=function(e) e),
                    promises),
                names=Map(deparse, promises)))
}

Delegate <-
  setRefClass('Delegate',
              fields='object',
              contains='VIRTUAL',
              methods=c(initialize=function(class='java.lang.Object', ...) {
                debug(class)
                object <<- new(J(class), ...)
                .self
              }))

getJavaRefClass <- getRefClass

setJavaRefClass <- function(className)
  tryCatch(getJavaRefClass(className),
           error=function(e) {
             class <- J('java.lang.Class')$forName(className)             
             superclass <- class$getSuperclass()
             ## if (!is.null(superclass))
             ##   setJavaRefClass(superclass$getName())
             contains <-
               if (is.null(superclass))
                 'Delegate'
               else {
                 superclassName <- superclass$getName()
                 setJavaRefClass(superclassName)
                 superclassName
               }
             ## debug(contains)
             declaredMethods <-
               Map(function(method) method$getName(),
                   as.list(class$getDeclaredMethods()))
             methods <-
               structure(Map(function(method)
                             ## Check for JavaObjects and extract the
                             ## ref; otherwise: pass through.
                             eval(substitute(function(...)
                                             .jrcall(.self$ref, method, ...),
                                             list(method=method))),
                             declaredMethods),
                         names=declaredMethods)
             setRefClass(className,
                         ## contains='Delegate',
                         contains=contains,
                         fields='ref',
                         methods=c(methods,
                            initialize=eval(substitute(function(...) {
                              ## callSuper(className, ...)
                              debug(className)
                              ref <<- new(J(className), ...)
                              .self
                           },
                              list(className=className)))),
                         )
           })

File <- setJavaRefClass('java.io.File')
## file <- File$new('/tmp')
## file$hashCode()
## file$ref$hashCode()
## debug(typeof(File),
##       class(File),
##       mode(File),
##       ls(File),
##       attributes(File),
##       File$className,
##       typeof(file),
##       class(file),
##       attributes(file))

## java.lang.Object is automagically defined as the parent of
## java.io.file.
## Object <- getJavaRefClass('java.lang.Object')
## stopifnot(typeof(Object$new()$hashCode()) == 'integer')
