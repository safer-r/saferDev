# is_package_here(): more examples

\

``` r

library(saferDev)
```

\

### Datasets

``` r

req_pkg <- "stats"
```

\

### Datasets info

``` r

req_pkg # package name between quotes.
```

    ## [1] "stats"

\

### Argument `req_package`

``` r

# Check that the stats package is installed.
saferDev::is_package_here(
    req_package = req_pkg,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
)

# Several packages can be checked at once.
saferDev::is_package_here(
    req_package = base::c("stats", "utils"),
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
)

# Error: missing package.
saferDev::is_package_here(
    req_package = "nopackage",
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::is_package_here().
    ## 
    ## REQUIRED PACKAGE:
    ## nopackage
    ## 
    ## MUST BE INSTALLED IN ONE OF THESE FOLDERS:
    ## /__w/_temp/Library
    ## /usr/local/lib/R/site-library
    ## /usr/local/lib/R/library
    ## 
    ## ================

\

### Argument `safer_check`

``` r

# Safer checks are performed before running.
saferDev::is_package_here(
    req_package = req_pkg,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
)
```

\

### Argument `lib_path`

``` r

# The folders of lib_path are searched after the current .libPaths(): the stats package is found.
saferDev::is_package_here(
    req_package = req_pkg,
    safer_check = TRUE,
    lib_path = base::tempdir(check = FALSE),
    error_text = ""
)

# Only the current .libPaths() are used.
saferDev::is_package_here(
    req_package = req_pkg,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
)

# The "NOTGOOD" path does not exist.
saferDev::is_package_here(
    req_package = req_pkg,
    safer_check = TRUE,
    lib_path = "NOTGOOD",
    error_text = ""
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::is_package_here().
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Argument `error_text`

``` r

# Add a text in error messages returned by is_package_here().
saferDev::is_package_here(
    req_package = req_pkg,
    safer_check = TRUE,
    lib_path = "NOTGOOD",
    error_text = " === TEXT ADDED ==="
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::is_package_here() === TEXT ADDED ===
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### All the arguments

``` r

saferDev::is_package_here(
    req_package = req_pkg, # package name string (between quotes).
    safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
    lib_path = NULL, # where the required packages are if not in the default directories.
    error_text = "" # add information in error messages returned by the function.
) 
```

\
