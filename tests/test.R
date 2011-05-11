## This is how we'd normally handle adding paths to the class-loader,
## but the test-environment doesn't seem to invoke .onLoad().
## options(morePaths='.')
library(rJavax)

system('make')

.jpackage('rJavax', morePaths='.')
.jengine(TRUE)

testInterfaceImplementation <-
  setRefClass('testInterfaceImplementation',
              methods=list(salute=function(salutandum)
                sprintf('Salve, %s!', salutandum)))

testInterface <-
  setJavaInterfaceImplementation('TestInterfaceProxyInterface',
                                 testInterfaceImplementation$new())

ti <- testInterface$new()

stopifnot(ti$salute('terra') == ti$proxy$salute('terra'))

## Test implicit inheritance from Object.
stopifnot(typeof(ti$hashCode()) == 'integer')
