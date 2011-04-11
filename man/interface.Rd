\name{interface}
\alias{interface}
\title{Implement a given Java interface.}
\usage{interface(interface, implementations)}
\description{Implement a given Java interface.}
\value{a nullary constructor which, when invoked, instantiates
the implementation; e.g. \code{animalium()$eat(prey)}}
\arguments{\item{interface}{the name of the to-be-implemented Java interface,
e.g. \code{'life.eukarya.opisthokonta.animalia'}}
\item{implementations}{a named list of functions which implement
the corresponding interface-method,
e.g. \code{list(eat=function(food) { ... })}}}
