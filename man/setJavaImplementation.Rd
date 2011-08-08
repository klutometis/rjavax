\name{setJavaImplementation}
\alias{setJavaImplementation}
\title{Create a Java-proxy-interface-backed refClass.}
\usage{
  setJavaImplementation(Class, fields = list(), contains =
  character(), methods = list(), implements = character(),
  extends = "java.lang.Object", where =
  topenv(parent.frame()), ...)
}
\arguments{
  \item{Class}{name of the Java interface to be
  implemented}

  \item{fields}{cf. \code{\link{ReferenceClasses}}}

  \item{contains}{cf. \code{\link{ReferenceClasses}}}

  \item{methods}{cf. cf. \code{\link{ReferenceClasses}}}

  \item{implements}{a character vector of Java interfaces
  implemented}

  \item{extends}{which Java base class is extended}

  \item{where}{the environment to define the javaRefClass
  in}

  \item{...}{arguments to \code{\link{setRefClass}}}
}
\value{
  a Java-proxy-interface-backed refClass
}
\description{
  Create a Java-proxy-interface-backed refClass.
}

