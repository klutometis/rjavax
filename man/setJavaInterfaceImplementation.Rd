\name{setJavaInterfaceImplementation}
\alias{setJavaInterfaceImplementation}
\title{Implement the Java-interface with a refClass-based...}
\usage{
  setJavaInterfaceImplementation(interface, implementation,
  base = "java.lang.Object")
}
\arguments{
  \item{interface}{the name of the to-be-implemented Java
  interface}

  \item{implementation}{an instance of the implementing
  \code{refClass}}

  \item{base}{base Java class from which to inherit
  function (default \code{java.lang.Object})}
}
\value{
  a \code{refClass} implementing the Java \code{interface}
}
\description{
  Implement the Java-\code{interface} with a
  \code{refClass}-based \code{implementation}.
}
\note{
  \code{implementation} takes an instance of the
  \code{refClass}, not the \code{refClass} itself; allowing
  one to make use of peculiar initializers.
}

