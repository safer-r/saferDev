# Environment Check

Verify that object names in the environment defined by the `pos`
parameter are identical or not to object names in the above environments
(following R Scope). This can be used to verify that names used for
objects inside a function or in the working environment do not override
names of objects already present in the above R environments, following
the R scope.

## Usage

``` r
env_check(
  pos = 1,
  name = NULL,
  safer_check = TRUE,
  lib_path = NULL,
  error_text = ""
)
```

## Arguments

- pos:

  Single non null positive integer indicating the position of the
  environment checked (argument `n` of the
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) function).
  Value `1` means one step above the `env_check()` local environment (by
  default). This means that when `env_check(pos = 1)` is used inside a
  function A, it checks if the name of any object in the local
  environment of this function A is also present in above environments,
  following R Scope, starting by the just above environment. When
  `env_check(pos = 1)` is used in the working (Global) environment
  (named `.GlobalEnv`), it checks the object names of this `.GlobalEnv`
  environment, in the above environments. See the examples below.

- name:

  Single character string of the name of the checked environment (used
  in the output string only, where the name of the ckecked env is
  replaced by the name value). Can be used when `env_check()` is used
  inside a function "A" to specify that the environment of "A" is
  checked.

- safer_check:

  Single logical value. Perform some "safer" checks? If `TRUE`,
  checkings are performed before main code running (see the [safer-r
  project](https://github.com/safer-r)): 1) correct `lib_path` argument
  value 2) required functions and related packages effectively present
  in local R libraries and 3) R classical operators (like `"<-"`) not
  overwritten by another package because of the R scope. Must be set to
  `FALSE` if this function is used inside another "safer" function to
  avoid pointless multiple checkings.

- lib_path:

  Vector of characters specifying the absolute pathways of the
  directories containing the required packages for the function, if not
  in the default directories. Useful when R packages are not installed
  in the default directories because of lack of admin rights. More
  precisely, `lib_path` is passed through the `new` argument of
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) so that the new
  library paths are `unique(c(new, .Library.site, .Library))`. Warning:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) is restored to
  the initial paths, after function execution. Ignored if `NULL`
  (default) or if the `safer_check` argument is `FALSE`: only the
  pathways specified by the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) are used for
  package calling.

- error_text:

  Single character string used to add information in error messages
  returned by the function, notably if the function is inside other
  functions, which is practical for debugging. Example:
  `error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."`.
  If `NULL`, converted into `""`.

## Value

A character string indicating the object names of the tested environment
that match object names in the above environments, following the R
scope, or `NULL` if no match.

## See also

[`exists`](https://rdrr.io/r/base/exists.html).

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferDev/articles/env_check.html
# Examples in the working environment
# creation of the object mean with value 1 in the .GlobalEnv environment, 
# knowing that the mean() function also exists in the environment base, above .GlobalEnv:
mean <- 1 
# creation of the object t.test with value 1 in the .GlobalEnv environment, 
# knowing that the t.test() function also exists in the environment stats, above .GlobalEnv:
t.test <- 1 
search() # current R scope (order of the successive R environments).
#>  [1] ".GlobalEnv"        "package:saferDev"  "package:stats"    
#>  [4] "package:graphics"  "package:grDevices" "package:utils"    
#>  [7] "package:datasets"  "package:methods"   "Autoloads"        
#> [10] "package:base"     
utils::find("mean") # where the objects with the name "mean" are present.
#> [1] "package:base"
utils::find("t.test") # where the objects with the name "mean" are present.
#> [1] "package:stats"
# test if any object name of the global environment are above environments:
a <- saferDev::env_check(pos = 1) 
a # output string.
#> [1] "SOME VARIABLES OF THE CHECKED ENVIRONMENT UNNAMED ARE ALSO PRESENT IN :\npackage:stats: t.test\npackage:base: mean\nSEARCH PATH CHECKED:\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n.GlobalEnv\npackage:saferDev\npackage:stats\npackage:graphics\npackage:grDevices\npackage:utils\npackage:datasets\npackage:methods\nAutoloads\npackage:base\n"
cat(a)
#> SOME VARIABLES OF THE CHECKED ENVIRONMENT UNNAMED ARE ALSO PRESENT IN :
#> package:stats: t.test
#> package:base: mean
#> SEARCH PATH CHECKED:
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> .GlobalEnv
#> package:saferDev
#> package:stats
#> package:graphics
#> package:grDevices
#> package:utils
#> package:datasets
#> package:methods
#> Autoloads
#> package:base
# test if any object of the stats environment (one step above .GlobalEnv) 
# are in upper environments of stats. Returns NULL since no object names of 
# stats are in upper environments:
saferDev::env_check(pos = 2) 
#> [1] "SOME VARIABLES OF THE CHECKED ENVIRONMENT UNNAMED ARE ALSO PRESENT IN :\nwithCallingHandlers(code, message = function (cnd) \n{\n    watcher$capture_plot_and_output()\n    if (on_message$capture) {\n        watcher$push(cnd)\n    }\n    if (on_message$silence) {\n        invokeRestart(\"muffleMessage\")\n    }\n}, warning = function (cnd) \n{\n    if (getOption(\"warn\") >= 2 || getOption(\"warn\") < 0) {\n        return()\n    }\n    watcher$capture_plot_and_output()\n    if (on_warning$capture) {\n        cnd <- sanitize_call(cnd)\n        watcher$push(cnd)\n    }\n    if (on_warning$silence) {\n        invokeRestart(\"muffleWarning\")\n    }\n}, error = function (cnd) \n{\n    watcher$capture_plot_and_output()\n    cnd <- sanitize_call(cnd)\n    watcher$push(cnd)\n    switch(on_error, continue = invokeRestart(\"eval_continue\"), stop = invokeRestart(\"eval_stop\"), error = NULL)\n}): expr\neval(call): enclos envir expr\ndoWithOneRestart(return(expr), restart): expr\nwithOneRestart(expr, restarts[[1]]): expr\nwithRestartList(expr, restarts[-nr]): expr\ndoWithOneRestart(return(expr), restart): expr\nwithOneRestart(withRestartList(expr, restarts[-nr]), restarts[[nr]]): expr\nwithRestartList(expr, restarts): expr\nwithRestarts(with_handlers({\n    for (expr in tle$exprs) {\n        ev <- withVisible(eval(expr, envir))\n        watcher$capture_plot_and_output()\n        watcher$print_value(ev$value, ev$visible, envir)\n    }\n    TRUE\n}, handlers), eval_continue = function() TRUE, eval_stop = function() FALSE): expr\nevaluate::evaluate(code, child_env(env), new_device = TRUE, output_handler = output_handler): enclos envir expr\nwithCallingHandlers(data_reference_topic(topic, pkg, examples_env = examples_env, run_dont_run = run_dont_run), error = function(err) {\n    cli::cli_abort(\"Failed to parse Rd in {.file {topic$file_in}}\", parent = err, call = quote(build_reference()))\n}): expr\nwithCallingHandlers(expr, error = function(cnd) {\n    if (i == 0) {\n    }\n    else {\n        message <- c(i = \"In index: {i}.\")\n        if (!is.null(names) && !is.na(names[[i]]) && names[[i]] != \"\") {\n            name <- names[[i]]\n            message <- c(message, i = \"With name: {name}.\")\n        }\n        else {\n            name <- NULL\n        }\n        cli::cli_abort(message, location = i, name = name, parent = cnd, call = error_call, class = \"purrr_error_indexed\")\n    }\n}): expr\nwith_indexed_errors(i = i, names = names, error_call = .purrr_error_call, call_with_cleanup(map_impl, environment(), .type, .progress, n, names, i)): expr\nwithCallingHandlers(code, purrr_error_indexed = function(err) {\n    cnd_signal(err$parent)\n}): expr\nSEARCH PATH CHECKED:\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n.GlobalEnv\npackage:saferDev\npackage:stats\npackage:graphics\npackage:grDevices\npackage:utils\npackage:datasets\npackage:methods\nAutoloads\npackage:base\n"
rm(mean)
rm(t.test)

# Examples inside a function
# saferDev::env_check() checks if the object names inside the fun1 function 
# exist in the .GlobalEnv environment and above:
fun1 <- function() {
  t.test <- 0
  mean <- 5
  saferDev::env_check(pos = 1)
}
a <- fun1()
cat(a) # Warning: cat(fun1()) creates an additional layer of environment.
#> SOME VARIABLES OF THE CHECKED ENVIRONMENT UNNAMED ARE ALSO PRESENT IN :
#> package:stats: t.test
#> package:base: mean
#> SEARCH PATH CHECKED:
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> .GlobalEnv
#> package:saferDev
#> package:stats
#> package:graphics
#> package:grDevices
#> package:utils
#> package:datasets
#> package:methods
#> Autoloads
#> package:base
# saferDev::env_check() checks if the object names inside the environment one step above fun2(), 
# here .GlobalEnv, exist in the upper environments of .GlobalEnv:
fun2 <- function() {
  sum <- 0
  saferDev::env_check(pos = 2)
}
fun2()
#> NULL
# Warning: cat(fun2()) does not return NULL, because the environment tested is not 
# anymore .GlobalEnv but inside cat().
a <- fun2() 
cat(a) # nothing displayed because fun2() returns NULL
# With the name of the function fun3 indicated in the message:
fun3 <- function() {
  t.test <- 0
  mean <- 5
  saferDev::env_check(pos = 1, name = "fun3")
}
a <- fun3() 
cat(a)
#> SOME VARIABLES OF THE CHECKED ENVIRONMENT fun3 ARE ALSO PRESENT IN :
#> package:stats: t.test
#> package:base: mean
#> SEARCH PATH CHECKED:
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> .GlobalEnv
#> package:saferDev
#> package:stats
#> package:graphics
#> package:grDevices
#> package:utils
#> package:datasets
#> package:methods
#> Autoloads
#> package:base
# Alternative way:
# sys.calls() gives the name of the imbricated functions and 
# sys.calls()[[length(sys.calls())]] the name of the function one step above.
fun4 <- function() {
  t.test <- 0
  mean <- 5
  name <- as.character(sys.calls()[[length(sys.calls())]])
  saferDev::env_check(pos = 1, name = name)
}
a <- fun4() 
cat(a)
#> SOME VARIABLES OF THE CHECKED ENVIRONMENT fun4 ARE ALSO PRESENT IN :
#> package:stats: t.test
#> package:base: mean
#> SEARCH PATH CHECKED:
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> .GlobalEnv
#> package:saferDev
#> package:stats
#> package:graphics
#> package:grDevices
#> package:utils
#> package:datasets
#> package:methods
#> Autoloads
#> package:base
# A way to have the name of the tested environment according to test.pos value:
fun7 <- function() {
  min <- "VALUE"
  fun8 <- function() {
    test.pos <- 1 # value 1 tests the fun8 env, 2 tests the fun7 env.
    range <- "VALUE"
    name <- if(length(sys.calls()) >= test.pos) {
      as.character(sys.calls()[[length(sys.calls()) + 1 - test.pos]])
    } else {
      search()[(1:length(search()))[test.pos - length(sys.calls())]]
    }
    saferDev::env_check(pos = test.pos, name = name) 
  }
  fun8()
}
a <- fun7() 
cat(a)
#> SOME VARIABLES OF THE CHECKED ENVIRONMENT fun8 ARE ALSO PRESENT IN :
#> package:base: range
#> SEARCH PATH CHECKED:
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> <environment>
#> .GlobalEnv
#> package:saferDev
#> package:stats
#> package:graphics
#> package:grDevices
#> package:utils
#> package:datasets
#> package:methods
#> Autoloads
#> package:base
```
