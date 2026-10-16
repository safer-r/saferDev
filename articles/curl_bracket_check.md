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

### All the arguments

``` r

curl_bracket_check(
    x = fun1, # function name to test.
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

\
