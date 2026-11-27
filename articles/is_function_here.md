# is_function_here(): more examples

\

``` r

library(saferDev)
```

\

### Datasets

``` r

f <- "stats::median"
```

\

### Datasets info

``` r

f # function name between quotes, preceded by its package.
```

    ## [1] "stats::median"

\

### Argument `fun`

``` r

# Check that the stats::median function is in the installed stats package.
is_function_here(fun = f) 

# Several functions can be checked at once.
is_function_here(fun = c("stats::median", "grid::gpar"))

# "pkg::fun" requires a function exported by the package: error here, because Pillai() is internal.
is_function_here(fun = "stats::Pillai")
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::is_function_here().
    ## 
    ## REQUIRED FUNCTION IS NOT EXPORTED BY THE INSTALLED PACKAGE:
    ## stats::Pillai
    ## 
    ## WRITE ":::" INSTEAD OF "::" TO CHECK A NON EXPORTED FUNCTION.
    ## 
    ## ================

``` r

# "pkg:::fun" accepts the internal functions.
is_function_here(fun = "stats:::Pillai")

# "pkg::fun" also accepts the functions that the package exports from another package: plot() is defined in base and exported by graphics.
is_function_here(fun = "graphics::plot")

# Operators and other non-syntactic names are accepted, with or without backticks, except the names starting with ":" (like := or ::), which must be written with backticks (like "rlang::`:=`").
is_function_here(fun = c("base::+", "base::[", "base::`[`", "base::names<-"))

# Error: missing function.
is_function_here(fun = "stats::notgood")
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::is_function_here().
    ## 
    ## REQUIRED FUNCTION IS MISSING IN THE INSTALLED PACKAGE:
    ## stats::notgood
    ## 
    ## IN ONE OF THESE FOLDERS:
    ## /__w/_temp/Library
    ## /usr/local/lib/R/site-library
    ## /usr/local/lib/R/library
    ## 
    ## ================

``` r

# Error: the package is not written.
is_function_here(fun = "median")
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::is_function_here().
    ## 
    ## THE STRING IN fun ARGUMENT MUST BE WRITTEN "package::function" OR "package:::function":
    ## median
    ## 
    ## ================

\

### Argument `safer_check`

``` r

# Safer checks are performed before running.
is_function_here(fun = f, safer_check = TRUE)
```

\

### Argument `lib_path`

``` r

# The "NOTGOOD" path does not exist.
is_function_here(fun = f, lib_path = "NOTGOOD")
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::is_function_here().
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

``` r

# Only the current .libPaths() are used.
is_function_here(fun = f, lib_path = NULL)
```

\

### Argument `error_text`

``` r

# Add a text in error messages returned by is_function_here().
is_function_here(
    fun = f, 
    lib_path = "NOTGOOD", 
    error_text = " === TEXT ADDED ==="
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::is_function_here() === TEXT ADDED ===
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### All the arguments

``` r

is_function_here(
    fun = f, # function name string (between quotes), with the associated package.
    safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
    lib_path = NULL, # where the required packages are if not in the default directories.
    error_text = "" # add information in error messages returned by the function.
) 
```

\
