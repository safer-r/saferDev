# curly_bracket_check(): more examples

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
curly_bracket_check(x = fun1)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::curly_bracket_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

``` r

# Here some bodies are not delimited by curly brackets.
curly_bracket_check(x = fun2)
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
    ##     if (x > 1) 
    ##         { base::return(x) }
    ##     else { base::return(0) }
    ##     for (i in 1:3) { base::print(i) }
    ##     while (x > 1) { x <- x - 1 }
    ##     repeat { x <- x + 1 }
    ##     base::sapply(X = 1:3, FUN = function(y) { y * 2 })
    ## }

\

### Argument `safer_check`

``` r

# Safer checkings are performed before main code running.
curly_bracket_check(
    x = fun1, 
    safer_check = TRUE
)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::curly_bracket_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

\

### Argument `lib_path`

``` r

# The "NOTGOOD" path does not exists.
curly_bracket_check(
    x = fun1,  
    lib_path = "NOTGOOD"
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::curly_bracket_check().
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Argument `error_text`

``` r

# Add a text in error messages returned by curly_bracket_check().
curly_bracket_check(
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
    ## ERROR IN saferDev::curly_bracket_check() === TEXT ADDED ===
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Arguments `export` and `out_path`: return or export the corrected function

``` r

# The corrected function is returned (NULL if nothing has to be corrected).
fun2_safer <- curly_bracket_check(x = fun2)
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

fun2_safer # { } added around each reported body.
```

    ## function (x) 
    ## {
    ##     if (x > 1) 
    ##         { base::return(x) }
    ##     else { base::return(0) }
    ##     for (i in 1:3) { base::print(i) }
    ##     while (x > 1) { x <- x - 1 }
    ##     repeat { x <- x + 1 }
    ##     base::sapply(X = 1:3, FUN = function(y) { y * 2 })
    ## }

``` r

# The corrected function behaves like the submitted one.
fun2(1)
```

    ## [1] 0

``` r

fun2_safer(1)
```

    ## [1] 0

``` r

# And it passes the check.
curly_bracket_check(x = fun2_safer)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::curly_bracket_check().
    ## INSIDE fun2_safer(), EVERYTHING SEEMS CLEAN.

``` r

# Export the corrected function into the fun2_safer.R file, here in a new temporary folder.
# out_path = "." (default) is the R working directory.
# An existing file is never overwritten (an error is returned instead).
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(), fileext = "")
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
curly_bracket_check(
    x = fun2,
    export = TRUE,
    out_path = tempo_dir
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
    ## /tmp/RtmpqmsHj4/dir1a74573340a4/fun2_safer.R

``` r

# base::source() of this file recreates the corrected function, named fun2_safer.
base::readLines(con = base::file.path(tempo_dir, "fun2_safer.R", fsep = "/"))
```

    ##  [1] "fun2_safer <- function (x) "                           
    ##  [2] "{"                                                     
    ##  [3] "    if (x > 1) "                                       
    ##  [4] "        { base::return(x) }"                           
    ##  [5] "    else { base::return(0) }"                          
    ##  [6] "    for (i in 1:3) { base::print(i) }"                 
    ##  [7] "    while (x > 1) { x <- x - 1 }"                      
    ##  [8] "    repeat { x <- x + 1 }"                             
    ##  [9] "    base::sapply(X = 1:3, FUN = function(y) { y * 2 })"
    ## [10] "}"

\

### All the arguments

``` r

curly_bracket_check(
    x = fun1, # function name to test.
    export = FALSE, # write the corrected function into a .R file instead of returning it?
    out_path = ".", # folder where to write the file (used only if export = TRUE).
    safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
    lib_path = NULL, # where the required packages are if not in the default directories.
    error_text = "" # add information in error messages returned by the function.
)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::curly_bracket_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

\

### Sophisticated example

``` r

# The body of a primitive function cannot have curly brackets: skipped.
curly_bracket_check(x = base::`+`)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::curly_bracket_check().
    ## INSIDE base::`+`(), EVERYTHING SEEMS CLEAN.

``` r

# The body of base::mean() is UseMethod("mean"): reported.
curly_bracket_check(x = base::mean)
```

    ## 
    ## 
    ## 
    ## INSIDE base::mean(), SOME CURLY BRACKETS {} ARE MISSING AFTER:
    ## 
    ## LINE CONSTRUCT       CODE
    ## 1    function        function (x, ...)

    ## function (x, ...) 
    ## { UseMethod("mean") }
    ## <environment: namespace:base>

\
