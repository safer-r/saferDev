# is_function_here(): more examples

\

``` r

library(saferDev)
```

\

### Datasets

``` r

f <- "ggplot2::geom_point"
```

\

### Datasets info

``` r

f # function name between quotes.
```

    ## [1] "ggplot2::geom_point"

\

### Argument `fun`

``` r

# check if the ggplot2::geom_point function is in the default library folder.
is_function_here(fun = f) 
```

\

### Argument `safer_check`

``` r

# Safer checkings are performed before main code running.
is_function_here(fun = f, safer_check = TRUE)
```

\

### Argument `lib_path`

``` r

# The "NOTGOOD" path does not exists.
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

# The ggplot2::geom_point function is in the default path (.libPaths()).
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
