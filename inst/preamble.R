
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
##
## Have a parameter which specifies how deep to list the call stack,
## negative or non-number (e.g. boolean) being the whole thing?
## Chicken implements it followingly: n deletes the n most recent
## entries; this defaults, currently, to 1 (avoid the actual call to
## debug).
##
## Also: an environment inspecting tool (which default to identity)?
##
## Should we indent the subsequent lines of str to go after the arrow?
## What about long/multiline expressions, though?
##
## Also: output and debug are interwoven; what about storing up the
## debug and outputting it at the end? What about conditions?
##
## Should be able to pass some things to `str' when e.g. `str' elides
## list elements.
##
## Need better condition-handling (just do the message and call
## manually); take a look at the default condition-handling display.
debug <- function(...,
                  environment=TRUE,
                  call.stack=TRUE,
                  where=parent.frame()) {
  ## If we could get e.g. the calling function
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
  if (environment)
    cat(sprintf('%s\n', format(where)))
  if (call.stack) {
    calls <- as.list(eval(sys.calls(), envir=where))
    cat.calls <- function(calls, depth=1) {
      if (length(calls)) {
        cat.calls(calls[-1], depth + 1)
        cat(sprintf('%s: %s\n',
                    depth,
                    paste(deparse(calls[[1]]),
                          collapse='\n  + ')))
      }
    }
    cat.calls(calls[-length(calls)])
  }
  
  promises <- as.list(substitute(list(...)))[-1]
  expressions <- Map(deparse, promises)
  values <- Map(function(promise)
                tryCatch(eval(promise, envir=where),
                         error=function(e) e),
                promises)
  cat.values <- function(expressions, values) {
    if (length(expressions)) {
      cat(sprintf('  %s -> %s',
                  paste(expressions[[1]],
                        collapse='\n  + '),
                  paste(capture.output(str(values[[1]])),
                        collapse='\n   ')),
          '\n')
      cat.values(expressions[-1],
                 values[-1])
    }
  }
  cat.values(expressions, values)
}
