# Check That Packages Are Installed

Checks that the required packages are installed, without loading them.

## Usage

``` r
is_package_here(
  req_package,
  safer_check = TRUE,
  lib_path = NULL,
  error_text = ""
)
```

## Arguments

- req_package:

  Vector of character strings: names of the packages to check (e.g.,
  `c("stats", "utils")`).

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

Invisible `NULL` if all the packages are installed, otherwise an error.

## See also

[`is_function_here`](is_function_here.md),
[`find.package`](https://rdrr.io/r/base/find.package.html).

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# See also https://safer-r.github.io/saferDev/articles/is_package_here.html
saferDev::is_package_here(req_package = base::c("stats", "utils"), safer_check = TRUE,
    lib_path = NULL, error_text = "")
# Missing package: error
base::try(expr = saferDev::is_package_here(req_package = "nopackage", safer_check = TRUE,
    lib_path = NULL, error_text = " INSIDE P1::F1"), silent = FALSE,
    outFile = base::getOption(x = "try.outFile", default = base::stderr()))
#> Error : 
#> 
#> ================
#> 
#> ERROR IN saferDev::is_package_here() INSIDE P1::F1
#> 
#> REQUIRED PACKAGE:
#> nopackage
#> 
#> MUST BE INSTALLED IN ONE OF THESE FOLDERS:
#> /__w/_temp/Library
#> /usr/local/lib/R/site-library
#> /usr/local/lib/R/library
#> 
#> ================
#> 
#> 
# Folder of lib_path that does not exist: error
base::try(expr = saferDev::is_package_here(req_package = "stats", safer_check = TRUE,
    lib_path = "NOTGOOD", error_text = ""), silent = FALSE,
    outFile = base::getOption(x = "try.outFile", default = base::stderr()))
#> Error : 
#> 
#> ================
#> 
#> ERROR IN saferDev::is_package_here().
#> 
#> THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
#> NOTGOOD
#> 
#> ================
#> 
#> 
```
