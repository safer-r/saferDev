# Check Object Names Against the Calling Frames and the Search Path

Checks whether the object names of an environment are also used in the
environments above it: the frames of the call stack
([`sys.frames()`](https://rdrr.io/r/base/sys.parent.html): the calling
functions, not the environments where the functions are defined), then
the search path ([`search()`](https://rdrr.io/r/base/search.html)).
Useful to detect object names of a function, or of the global
environment, that mask or are masked by other objects.

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

  Single positive integer: position of the checked environment in the
  call stack, counted from `env_check()`, then in the search path. `1`
  (default) checks the environment that calls `env_check()`: the frame
  of a function, or the global environment at top level. `2` checks the
  environment one step further up, etc., up to `package:base`, the last
  environment of the search path.

- name:

  Single character string: name of the checked environment in the
  returned message (e.g., the name of the function calling
  `env_check()`). If `NULL` (default), `"UNNAMED"` for the frame of a
  function, or the name of an environment of the search path (e.g.,
  `".GlobalEnv"`).

- safer_check:

  Single logical value. If `TRUE` (default), checks before running that
  the required packages and functions are installed and that the
  classical R operators (like `<-`) are not masked by another package or
  by a user-defined object of the global environment. These checks take
  time at each call: set `FALSE` in loops and inside other safer-r
  functions, to avoid repeating them.

- lib_path:

  Vector of character strings: paths of existing extra library folders
  containing the required packages (e.g., packages installed without
  admin rights). They are searched after the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) during the call
  (the current libraries keep priority), and
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) is restored on
  exit. If `NULL` (default), only the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) are used.

- error_text:

  Single character string added to the error messages (e.g.,
  `" INSIDE pkg1::fun1()"`) to name the calling functions. `NULL` is
  converted into `""`.

## Value

A character string listing, for each environment above the checked one,
the object names it shares with the checked environment, followed by the
environments checked (frames written `<environment>`). `NULL` if no name
is shared.

## Details

Call `env_check()` directly (e.g., `res <- env_check()`), not inside
another call like `print(env_check())` or `cat(env_check())`, which
would check the frame of [`print()`](https://rdrr.io/r/base/print.html)
or [`cat()`](https://rdrr.io/r/base/cat.html) instead. At the top level
of code run by [`source()`](https://rdrr.io/r/base/source.html) or
knitr, the frames of the functions running the code are also checked.

## See also

[`sys.frames`](https://rdrr.io/r/base/sys.parent.html),
[`search`](https://rdrr.io/r/base/search.html).

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# Warning: the "Run examples" link also checks the frames of the code running the examples.
# See also https://safer-r.github.io/saferDev/articles/env_check.html
# Inside a function: the names t.test and mean, created inside fun1(),
# also exist in the stats and base environments located above:
fun1 <- function(){
    t.test <- 0
    mean <- 5
    saferDev::env_check(pos = 1, name = "fun1", safer_check = TRUE, lib_path = NULL,
        error_text = "")
}
a <- fun1()
base::cat(a, file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE)
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
    saferDev::env_check(pos = 1, name = "fun2", safer_check = TRUE, lib_path = NULL,
        error_text = "")
}
fun2()
#> NULL
# The frames of the calling functions are checked (call stack), here the frame of fun3():
fun3 <- function(){
    min <- 1
    fun4()
}
fun4 <- function(){
    min <- 2
    saferDev::env_check(pos = 1, name = "fun4", safer_check = TRUE, lib_path = NULL,
        error_text = "")
}
a <- fun3()
base::cat(a, file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE)
#> SOME VARIABLES OF THE CHECKED ENVIRONMENT fun4 ARE ALSO PRESENT IN :
#> fun3(): min
#> package:base: min
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
