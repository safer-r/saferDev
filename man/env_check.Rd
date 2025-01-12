% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/env_check.R
\name{env_check}
\alias{env_check}
\title{env_check}
\usage{
env_check(
  pos = 1,
  name = NULL,
  lib_path = NULL,
  safer_check = TRUE,
  error_text = ""
)
}
\arguments{
\item{pos}{Single non nul positive integer indicating the position of the environment checked (argument n of the parent.frame() function). Value 1 means one step above the env_check() local environment (by default). This means that when env_check(pos = 1) is used inside a function A, it checks if the name of any object in the local environment of this function A is also present in above environments, following R Scope, starting by the just above environment. When env_check(pos = 1) is used in the working (Global) environment (named .GlobalEnv), it checks the object names of this .GlobalEnv environment, in the above environments. See the examples below.}

\item{name}{Single character string indicating a string that will be added in the output string, for instance the name of a function inside which env_check() is used.}

\item{lib_path}{Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful to overcome R execution using system with non admin rights for R package installation in the default directories. Ignored if NULL (default): only the pathways specified by .libPaths() are used for package calling. Specify the right path if the function returns a package path error.}

\item{safer_check}{Single logical value. Perform some "safer" checks? If TRUE, checkings are performed before main code running (see https://github.com/safer-r): 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.}

\item{error_text}{Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = "INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>".}
}
\value{
A character string indicating the object names of the tested environment that match object names in the above environments, following the R scope, or NULL if no match.
}
\description{
Verify that object names in the environment defined by the pos parameter are identical or not to object names in the above environments (following R Scope). This can be used to verify that names used for objects inside a function or in the working environment do not override names of objects already present in the above R environments, following the R scope.
}
\examples{
# Examples in the working environment
# creation of the object mean with value 1 in the .GlobalEnv environment, 
# knowing that the mean() function also exists in the environment base, above .GlobalEnv:
mean <- 1 
# creation of the object t.test with value 1 in the .GlobalEnv environment, 
# knowing that the t.test() function also exists in the environment stats, above .GlobalEnv:
t.test <- 1 
search() # current R scope (order of the successive R environments).
utils::find("mean") # where the objects with the name "mean" are present.
utils::find("t.test") # where the objects with the name "mean" are present.
a <- env_check(pos = 1) # test if any object name of the global environment are above environments 
a # output string.
cat(a)
# test if any object of the stats environment (one step above .GlobalEnv) 
# are in upper environments of stats. Returns NULL since no object names of stats are in upper environments:
env_check(pos = 2) 
rm(mean) ; rm (t.test)

# Examples inside a function
# env_check() checks if the object names inside the fun1 function 
# exist in the .GlobalEnv environment and above:
fun1 <- function(){t.test <- 0 ; mean <- 5 ; env_check(pos = 1)} 
fun1()
cat(fun1())
# env_check() checks if the object names inside the environment one step above fun2(), 
# here .GlobalEnv, exist in the upper environments of .GlobalEnv:
fun2 <- function(){sum <- 0 ; env_check(pos = 2)} 
fun2()
# Warning: cat(fun2()) does not return NULL, because the environement tested is not anymore .GlobalEnv but inside cat().
# With the name of the function fun3 indicated in the message:
fun3 <- function(){t.test <- 0 ; mean <- 5 ; env_check(pos = 1, name = "fun3")}
fun3()
# Alternative way:
fun4 <- function(){t.test <- 0 ; mean <- 5 ; env_check(pos = 1, name = as.character(sys.calls()[[length(sys.calls())]]))}
fun4()
# sys.calls() gives the name of the imbricated functions, 
# sys.calls()[[length(sys.calls())]] the name of the function one step above.
fun5 <- function(){fun6 <- function(){print(sys.calls())} ; fun6()}
fun5()
# A way to have the name of the tested environment according to test.pos value:
fun7 <- function(){
    min <- "a"
    fun8 <- function(){
        test.pos <- 1 # value 1 tests the fun8 env, 2 tests the fun7 env.
        range <- "a"
        env_check(pos = test.pos, name = if(length(sys.calls()) >= test.pos){
            as.character(sys.calls()[[length(sys.calls()) + 1 - test.pos]])
        }else{
            search()[(1:length(search()))[test.pos - length(sys.calls())]]
        }) 
    }
    fun8()
}
fun7()
}
\seealso{
\code{\link{exists}}.
}
\author{
Gael Millot <gael.millot@pasteur.fr>

Yushi Han <yushi.han2000@gmail.com>

Haiding Wang <wanghaiding442@gmail.com>
}
