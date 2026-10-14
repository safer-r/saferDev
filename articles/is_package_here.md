# is_package_here(): more examples

\

``` r

library(saferDev)
```

\

### Datasets

``` r

req.pkg <- "ggplot2"
```

\

### Datasets info

``` r

req.pkg # package name between quotes.
```

    ## [1] "ggplot2"

\

### Argument `req_package`

``` r

# check if the ggplot2 package is in the default library folder.
is_package_here(req_package = req.pkg) 
```

\

### Argument `safer_check`

``` r

# Safer checkings are performed before main code running.
is_package_here(req_package = req.pkg, safer_check = TRUE)
```

\

### Argument `lib_path`

``` r

# The ggplot2 package is in one of the paths c(".", .libPaths()).
is_package_here(req_package = req.pkg, lib_path = ".")

# The ggplot2 package is in the default path (.libPaths()).
is_package_here(req_package = req.pkg, lib_path = NULL)

# The "NOTGOOD" path does not exists.
is_package_here(req_package = req.pkg, lib_path = "NOTGOOD")
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
is_package_here(
    req_package = req.pkg, 
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

is_package_here(
    req_package = req.pkg, # package name string (between quotes).
    safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
    lib_path = NULL, # where the required packages are if not in the default directories.
    error_text = "" # add information in error messages returned by the function.
) 
```

\`\`\`
