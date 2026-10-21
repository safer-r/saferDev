# true_false_check(): more examples

\

``` r

library(saferDev)
```

\

### Datasets

``` r

# Function with TRUE and FALSE written in full.
fun1 <- function(x){
    if(x == TRUE){
        base::return(base::mean(x = 1:3, na.rm = FALSE))
    }
}
# Function with T and F used as values.
fun2 <- function(x){
    if(x == T){
        base::return(base::mean(x = 1:3, na.rm = F))
    }
}
```

\

### Datasets info

``` r

fun1 # Function.
```

    ## function (x) 
    ## {
    ##     if (x == TRUE) {
    ##         base::return(base::mean(x = 1:3, na.rm = FALSE))
    ##     }
    ## }

``` r

fun2 # Function.
```

    ## function (x) 
    ## {
    ##     if (x == T) {
    ##         base::return(base::mean(x = 1:3, na.rm = F))
    ##     }
    ## }

\

### Argument `x`

``` r

# Test if TRUE and FALSE are written in full in fun1.
# Here everything is fine.
true_false_check(x = fun1)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::true_false_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

    ## NULL

``` r

# Here T and F are used.
true_false_check(x = fun2)
```

    ## 
    ## 
    ## 
    ## INSIDE fun2(), SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE:
    ## 
    ## LINE SYMBOL      CODE
    ## 3    T       if (x == T) {
    ## 4    F       base::return(base::mean(x = 1:3, na.rm = F))
    ## 
    ## MODIFIED FUNCTION RETURNED BY return().

    ## function (x) 
    ## {
    ##     if (x == TRUE) {
    ##         base::return(base::mean(x = 1:3, na.rm = FALSE))
    ##     }
    ## }
    ## <environment: 0x5598da28f690>

``` r

# Error: the functions called in x must first be written with their package (run colons_check() first).
fun3 <- function(x){ if(x == T){ mean(x = 1:3) } }
true_false_check(x = fun3)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::true_false_check().
    ## 
    ## INSIDE fun3(), SOME FUNCTIONS ARE NOT ASSOCIATED TO THEIR PACKAGE.
    ## 
    ## PLEASE, RUN saferDev::colons_check(fun3()) FIRST,
    ## ADD THE MISSING <PACKAGE>::<FUNCTION> (OR <PACKAGE>:::<FUNCTION> FOR FUNCTION STARTING BY A DOT)
    ## AND RERUN saferDev::true_false_check(fun3()).
    ## 
    ## ================

\

### Argument `safer_check`

``` r

# Safer checkings are performed before main code running.
true_false_check(
    x = fun1, 
    safer_check = TRUE
)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::true_false_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

    ## NULL

\

### Argument `lib_path`

``` r

# The "NOTGOOD" path does not exists.
true_false_check(
    x = fun1,  
    lib_path = "NOTGOOD"
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::true_false_check().
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Argument `error_text`

``` r

# Add a text in error messages returned by true_false_check().
true_false_check(
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
    ## ERROR IN saferDev::true_false_check() === TEXT ADDED ===
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Arguments `export` and `out_path`: return or export the corrected function

``` r

# The corrected function is returned (NULL if nothing has to be corrected).
fun2_safer <- true_false_check(x = fun2)
```

    ## 
    ## 
    ## 
    ## INSIDE fun2(), SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE:
    ## 
    ## LINE SYMBOL      CODE
    ## 3    T       if (x == T) {
    ## 4    F       base::return(base::mean(x = 1:3, na.rm = F))
    ## 
    ## MODIFIED FUNCTION RETURNED BY return().

``` r

fun2_safer # T replaced by TRUE, F replaced by FALSE.
```

    ## function (x) 
    ## {
    ##     if (x == TRUE) {
    ##         base::return(base::mean(x = 1:3, na.rm = FALSE))
    ##     }
    ## }
    ## <environment: 0x5598dab65d78>

``` r

# And it passes the check.
true_false_check(x = fun2_safer)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::true_false_check().
    ## INSIDE fun2_safer(), EVERYTHING SEEMS CLEAN.

    ## NULL

``` r

# Export the corrected function into the fun2_safer.R file, here in a new temporary folder.
# out_path = "." (default) is the R working directory.
# An existing file is never overwritten (an error is returned instead).
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(), fileext = "")
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
true_false_check(
    x = fun2,
    export = TRUE,
    out_path = tempo_dir
)
```

    ## 
    ## 
    ## 
    ## INSIDE fun2(), SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE:
    ## 
    ## LINE SYMBOL      CODE
    ## 3    T       if (x == T) {
    ## 4    F       base::return(base::mean(x = 1:3, na.rm = F))

    ## MODIFIED CODE EXPORTED IN:
    ## /tmp/Rtmp3VZt5O/dir1d5a7b6e7bc9/fun2_safer.R

``` r

# base::source() of this file recreates the corrected function, named fun2_safer.
base::readLines(con = base::file.path(tempo_dir, "fun2_safer.R", fsep = "/"))
```

    ## [1] "fun2_safer <- function (x) "                             
    ## [2] "{"                                                       
    ## [3] "    if (x == TRUE) {"                                    
    ## [4] "        base::return(base::mean(x = 1:3, na.rm = FALSE))"
    ## [5] "    }"                                                   
    ## [6] "}"

\

### All the arguments

``` r

true_false_check(
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
    ## AFTER RUNNING saferDev::true_false_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

    ## NULL

\

### Sophisticated example

``` r

# T and F used as names are not reported: they are user-defined objects.
fun4 <- function(x){
    df <- base::data.frame(T = 1:2, stringsAsFactors = FALSE)
    y <- df$T # column named T
    z <- base::list(F = 3) # argument named F
    base::return(y)
}
true_false_check(x = fun4)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::true_false_check().
    ## INSIDE fun4(), EVERYTHING SEEMS CLEAN.

    ## NULL

``` r

# If T is defined in the function, its uses are not reported.
fun5 <- function(x){
    T <- 2
    base::return(x + T)
}
true_false_check(x = fun5)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::true_false_check().
    ## INSIDE fun5(), EVERYTHING SEEMS CLEAN.

    ## NULL

``` r

# Strings and comments are ignored.
fun6 <- function(x){
    base::return("T") # F
}
true_false_check(x = fun6)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::true_false_check().
    ## INSIDE fun6(), EVERYTHING SEEMS CLEAN.

    ## NULL

\
