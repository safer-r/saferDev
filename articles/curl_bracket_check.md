# curl_bracket_check: more examples

\

``` r

library(saferDev)
```

\

### Datasets

``` r

# Function with all the bodies delimited by curly brackets.
fun1 <- function(x){
    if(x > 1){
        base::return(x)
    }else{
        base::return(0)
    }
    for(i in 1:3){
        base::print(i)
    }
    while(x > 1){
        x <- x - 1
    }
    repeat{
        x <- x + 1
    }
    base::sapply(X = 1:3, FUN = function(y){
        y * 2
    })
}
# Function with some bodies not delimited by curly brackets.
fun2 <- function(x){
    if(x > 1)
        base::return(x)
    else
        base::return(0)
    for(i in 1:3)
        base::print(i)
    while(x > 1)
        x <- x - 1
    repeat
        x <- x + 1
    base::sapply(X = 1:3, FUN = function(y) y * 2)
}
```

\

### Datasets info

``` r

fun1 # Function.
```

    ## function (x) 
    ## {
    ##     if (x > 1) {
    ##         base::return(x)
    ##     }
    ##     else {
    ##         base::return(0)
    ##     }
    ##     for (i in 1:3) {
    ##         base::print(i)
    ##     }
    ##     while (x > 1) {
    ##         x <- x - 1
    ##     }
    ##     repeat {
    ##         x <- x + 1
    ##     }
    ##     base::sapply(X = 1:3, FUN = function(y) {
    ##         y * 2
    ##     })
    ## }

``` r

fun2 # Function.
```

    ## function (x) 
    ## {
    ##     if (x > 1) 
    ##         base::return(x)
    ##     else base::return(0)
    ##     for (i in 1:3) base::print(i)
    ##     while (x > 1) x <- x - 1
    ##     repeat x <- x + 1
    ##     base::sapply(X = 1:3, FUN = function(y) y * 2)
    ## }

\

### Argument `x`

``` r

# Test if all the bodies of fun1 are delimited by curly brackets.
# Here everything is fine.
curl_bracket_check(x = fun1)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::curl_bracket_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

``` r

# Here some bodies are not delimited by curly brackets.
curl_bracket_check(x = fun2)
```

    ## 
    ## 
    ## 
    ## INSIDE fun2(), SOME CURLY BRACKETS {} ARE MISSING AFTER:
    ## 
    ## LINE CONSTRUCT       CODE
    ## 3    if      if (x > 1) 
    ## 5    else        else base::return(0)
    ## 6    for     for (i in 1:3) base::print(i)
    ## 7    while       while (x > 1) x <- x - 1
    ## 8    repeat      repeat x <- x + 1
    ## 9    function        base::sapply(X = 1:3, FUN = function(y) y * 2)

    ## function (x) 
    ## {
    ##     if (x > 1) {
    ##         base::return(x)
    ##     }
    ##     else {
    ##         base::return(0)
    ##     }
    ##     for (i in 1:3) {
    ##         base::print(i)
    ##     }
    ##     while (x > 1) {
    ##         x <- x - 1
    ##     }
    ##     repeat {
    ##         x <- x + 1
    ##     }
    ##     base::sapply(X = 1:3, FUN = function(y) {
    ##         y * 2
    ##     })
    ## }

\

### Argument `safer_check`

``` r

# Safer checkings are performed before main code running.
curl_bracket_check(
    x = fun1, 
    safer_check = TRUE
)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::curl_bracket_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

\

### Argument `lib_path`

``` r

# The "NOTGOOD" path does not exists.
curl_bracket_check(
    x = fun1,  
    lib_path = "NOTGOOD"
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::curl_bracket_check().
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Argument `error_text`

``` r

# Add a text in error messages returned by curl_bracket_check().
curl_bracket_check(
    x = fun1, 
    lib_path = "NOTGOOD", 
    error_text = " === TEXT ADDED ==="
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::curl_bracket_check() === TEXT ADDED ===
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Arguments `export` and `out_path`: return or export the modified function

When curly brackets [`{ }`](https://rdrr.io/r/base/Paren.html) are
missing in the code of the submitted function,
[`curl_bracket_check()`](../reference/curl_bracket_check.md) can return
the corrected function (with the missing
[`{ }`](https://rdrr.io/r/base/Paren.html) added around each reported
construct body), or export its code into a `.R` file named with the
submitted function name and `_safer` added at the end:

``` r

# Returns the modified function, or NULL if nothing has to be modified.
fun2_safer <- curl_bracket_check(
    x = fun2
)
```

    ## 
    ## 
    ## 
    ## INSIDE fun2(), SOME CURLY BRACKETS {} ARE MISSING AFTER:
    ## 
    ## LINE CONSTRUCT       CODE
    ## 3    if      if (x > 1) 
    ## 5    else        else base::return(0)
    ## 6    for     for (i in 1:3) base::print(i)
    ## 7    while       while (x > 1) x <- x - 1
    ## 8    repeat      repeat x <- x + 1
    ## 9    function        base::sapply(X = 1:3, FUN = function(y) y * 2)

``` r

fun2_safer # modified function (or NULL when the code was already clean).
```

    ## function (x) 
    ## {
    ##     if (x > 1) {
    ##         base::return(x)
    ##     }
    ##     else {
    ##         base::return(0)
    ##     }
    ##     for (i in 1:3) {
    ##         base::print(i)
    ##     }
    ##     while (x > 1) {
    ##         x <- x - 1
    ##     }
    ##     repeat {
    ##         x <- x + 1
    ##     }
    ##     base::sapply(X = 1:3, FUN = function(y) {
    ##         y * 2
    ##     })
    ## }

``` r

# The modified function behaves like the submitted one.
fun2(1)
```

    ## [1] 0

``` r

fun2_safer(1)
```

    ## [1] 0

``` r

# The modified function passes the check itself.
curl_bracket_check(x = fun2_safer)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::curl_bracket_check().
    ## INSIDE fun2_safer(), EVERYTHING SEEMS CLEAN.

``` r

# Export the modified function into the fun2_safer.R file, in the "." path.
# Warning: an already existing fun2_safer.R file is never overwritten (an error is returned instead).
curl_bracket_check(
    x = fun2,
    export = TRUE,
    out_path = "."
)
```

    ## 
    ## 
    ## 
    ## INSIDE fun2(), SOME CURLY BRACKETS {} ARE MISSING AFTER:
    ## 
    ## LINE CONSTRUCT       CODE
    ## 3    if      if (x > 1) 
    ## 5    else        else base::return(0)
    ## 6    for     for (i in 1:3) base::print(i)
    ## 7    while       while (x > 1) x <- x - 1
    ## 8    repeat      repeat x <- x + 1
    ## 9    function        base::sapply(X = 1:3, FUN = function(y) y * 2)

    ## MODIFIED CODE EXPORTED IN:
    ## ./fun2_safer.R

    ## NULL

``` r

# The exported file contains an assignment (fun2_safer <- function(...)...), 
# so that base::source()ing it recreates the modified function.
# Warning: comments of the submitted function are lost in the exported file.
```

When `export = TRUE`, the modified function is not returned by the
function but only exported (no file is created when the submitted
function is already compliant). `out_path = "."` (default) means the R
working directory set by the user, and the tested function must be
designated by a function name (not by an inline definition, nor by a
component writing like `obj$fun`), because the name of the exported file
could not be determined reliably otherwise.\

### All the arguments

``` r

curl_bracket_check(
    x = fun1, # function name to test.
    export = FALSE, # return the modified function, or export it into a .R file?
    out_path = ".", # pathway of the folder where to export the modified function file.
    safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
    lib_path = NULL, # where the required packages are if not in the default directories.
    error_text = "" # add information in error messages returned by the function.
)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::curl_bracket_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

\

### Sophisticated example

``` r

# The body of a primitive function cannot be delimited by curly brackets: skipped.
curl_bracket_check(x = base::`+`)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::curl_bracket_check().
    ## INSIDE base::`+`(), EVERYTHING SEEMS CLEAN.

``` r

# The body of base::mean() is UseMethod("mean"): reported.
curl_bracket_check(x = base::mean)
```

    ## 
    ## 
    ## 
    ## INSIDE base::mean(), SOME CURLY BRACKETS {} ARE MISSING AFTER:
    ## 
    ## LINE CONSTRUCT       CODE
    ## 1    function        function (x, ...)

    ## function (x, ...) 
    ## {
    ##     UseMethod("mean")
    ## }
    ## <environment: namespace:base>

\
