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

  Single logical value. Perform the "safer" checks? If `TRUE`, checkings
  are performed before main code running (see the [safer-r
  project](https://github.com/safer-r)): 1) required functions and
  related packages effectively present in local R libraries and 2) R
  classical operators (like `"<-"`) not overwritten by another package
  because of the R scope. Warning: must be set to `FALSE` if this
  function is used inside another "safer" function to avoid pointless
  multiple checkings.

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
  (default): only the pathways specified by the current
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
# Inside a function: the names t.test and mean, created inside fun1(),
# also exist in the stats and base environments located above:
fun1 <- function(){
    t.test <- 0
    mean <- 5
    saferDev::env_check(pos = 1, name = "fun1")
}
a <- fun1()
base::cat(a)
#> SOME VARIABLES OF THE CHECKED ENVIRONMENT fun1 ARE ALSO PRESENT IN :
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
# NULL returned when no name is also present in the environments above:
fun2 <- function(){
    tempo_name_used_nowhere_else <- 0
    saferDev::env_check(pos = 1, name = "fun2")
}
fun2()
#> NULL
# pos = 2 checks the environment one step above the function (here, the calling environment).
# See the env_check() article of the website for more examples.
```
