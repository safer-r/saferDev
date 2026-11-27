# Check That Functions Exist in Installed Packages

Checks that the required functions are present in the installed packages
(e.g., not removed or renamed in another version of the package).

## Usage

``` r
is_function_here(fun, safer_check = TRUE, lib_path = NULL, error_text = "")
```

## Arguments

- fun:

  Vector of character strings: the required functions, each preceded by
  its package and `"::"` or `":::"`, without final `"()"` (e.g.,
  `c("stats::median", "grid::gpar")`). `"pkg::fun"` requires an object
  exported by the package, `"pkg:::fun"` accepts any object of the
  package namespace (internal functions included). The object can be of
  any kind (e.g., `"base::pi"`), except lazy-loaded datasets (e.g.,
  `"datasets::iris"`), which are not found. Operators and other
  non-syntactic names can be written with or without backticks (e.g.,
  `"base::+"`), except the names starting with `":"` (e.g., `:=`, `::`),
  which must be written with backticks (e.g., `` "rlang::`:=`" ``).

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

Invisible `NULL` if all the functions are present, otherwise an error.
The namespaces of the checked packages are loaded (not attached).

## See also

[`is_package_here`](is_package_here.md),
[`exists`](https://rdrr.io/r/base/exists.html),
[`getNamespaceExports`](https://rdrr.io/r/base/ns-reflect.html).

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# See also https://safer-r.github.io/saferDev/articles/is_function_here.html
saferDev::is_function_here(fun = base::c("stats::median", "grid::gpar"), safer_check = TRUE,
    lib_path = NULL, error_text = "")
# Missing function: error
base::try(expr = saferDev::is_function_here(fun = "stats::notgood", safer_check = TRUE,
    lib_path = NULL, error_text = " INSIDE P1::F1"), silent = FALSE,
    outFile = base::getOption(x = "try.outFile", default = base::stderr()))
#> Error : 
#> 
#> ================
#> 
#> ERROR IN saferDev::is_function_here() INSIDE P1::F1
#> 
#> REQUIRED FUNCTION IS MISSING IN THE INSTALLED PACKAGE:
#> stats::notgood
#> 
#> IN ONE OF THESE FOLDERS:
#> /__w/_temp/Library
#> /usr/local/lib/R/site-library
#> /usr/local/lib/R/library
#> 
#> ================
#> 
#> 
# Internal function (not exported): error with "::", no error with ":::"
base::try(expr = saferDev::is_function_here(fun = "stats::Pillai", safer_check = TRUE,
    lib_path = NULL, error_text = ""), silent = FALSE,
    outFile = base::getOption(x = "try.outFile", default = base::stderr()))
#> Error : 
#> 
#> ================
#> 
#> ERROR IN saferDev::is_function_here().
#> 
#> REQUIRED FUNCTION IS NOT EXPORTED BY THE INSTALLED PACKAGE:
#> stats::Pillai
#> 
#> WRITE ":::" INSTEAD OF "::" TO CHECK A NON EXPORTED FUNCTION.
#> 
#> ================
#> 
#> 
saferDev::is_function_here(fun = "stats:::Pillai", safer_check = TRUE, lib_path = NULL,
    error_text = "")
# Package not written: error
base::try(expr = saferDev::is_function_here(fun = "c", safer_check = TRUE, lib_path = NULL,
    error_text = ""), silent = FALSE,
    outFile = base::getOption(x = "try.outFile", default = base::stderr()))
#> Error : 
#> 
#> ================
#> 
#> ERROR IN saferDev::is_function_here().
#> 
#> THE STRING IN fun ARGUMENT MUST BE WRITTEN "package::function" OR "package:::function":
#> c
#> 
#> ================
#> 
#> 
```
