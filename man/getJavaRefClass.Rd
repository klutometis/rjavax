\name{getJavaRefClass}
\alias{getJavaRefClass}
\title{Get a refClass-based proxy from the given class-name.}
\usage{
  getJavaRefClass(Class, where = topenv(parent.frame()))
}
\arguments{
  \item{Class}{the class-name whose proxy to retrieve}

  \item{where}{the environment in which to get it}
}
\value{
  the proxy or NULL
}
\description{
  Get a refClass-based proxy from the given class-name.
}

