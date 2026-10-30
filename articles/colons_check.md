# colons_check: more examples

\

``` r

library(saferDev)
```

\

### Datasets

``` r

# Function.
fun1 <- function(){
    base::sum(1, 3)
  }
# Function.
fun2 <- function(){
    sum(1, 3)
  }
```

\

### Datasets info

``` r

fun1 # Function.
```

    ## function () 
    ## {
    ##     base::sum(1, 3)
    ## }

``` r

fun2 # Function.
```

    ## function () 
    ## {
    ##     sum(1, 3)
    ## }

\

### Argument `x`

``` r

# Test if all the functions inside fun1 have their associated package written.
# Here everything is fine.
colons_check(x = fun1)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::colons_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

    ## NULL

``` r

# Here one missing package declaration.
colons_check(x = fun2)
```

    ## 
    ## 
    ## INSIDE fun2(), SOME :: OR ::: ARE MISSING BEFORE BASIC FUNCTIONS:
    ## 
    ## LINE FUN     STRING_BEFORE
    ## 3    sum     

    ## function () 
    ## {
    ##     base::sum(1, 3)
    ## }

\

### Argument `safer_check`

``` r

# Safer checkings are performed before main code running.
colons_check(
    x = fun1, 
    safer_check = TRUE
)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::colons_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

    ## NULL

\

### Argument `lib_path`

``` r

# The "NOTGOOD" path does not exists.
colons_check(
    x = fun1,  
    lib_path = "NOTGOOD"
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::colons_check().
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Argument `error_text`

``` r

# Add a text in error messages returned by colons_check().
colons_check(
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
    ## ERROR IN saferDev::colons_check() === TEXT ADDED ===
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Arguments `export` and `out_path`

``` r

# Since saferDev v3.0.0, the package attributions of the basic functions are added automatically.
# The modified function is returned by return() (NULL if no modification was needed).
res <- colons_check(x = fun2) # res is the modified function: sum() has become base::sum()
```

    ## 
    ## 
    ## INSIDE fun2(), SOME :: OR ::: ARE MISSING BEFORE BASIC FUNCTIONS:
    ## 
    ## LINE FUN     STRING_BEFORE
    ## 3    sum     

``` r

res # print the modified function.
```

    ## function () 
    ## {
    ##     base::sum(1, 3)
    ## }

``` r

res() # the modified function evaluates identically.
```

    ## [1] 4

``` r

# Alternatively, the modified code can be exported in a .R file (export = TRUE).
# The file is named <function name>_safer.R, here fun2_safer.R, and is written in out_path.
# Nothing is written and NULL is returned if no modification was needed.
# An error is returned if the file already exists (never overwritten).
base::file.remove(base::file.path(out_path = base::tempdir(), "fun2_safer.R", fsep = "/")) # remove a potential pre-existing file from a previous run
```

    ## Warning in base::file.remove(base::file.path(out_path = base::tempdir(), :
    ## cannot remove file '/tmp/RtmpiUjuRE/fun2_safer.R', reason 'No such file or
    ## directory'

    ## [1] FALSE

``` r

colons_check(
    x = fun2,
    export = TRUE,
    out_path = base::tempdir() # directory where to write fun2_safer.R (default: the R working directory).
)
```

    ## 
    ## 
    ## INSIDE fun2(), SOME :: OR ::: ARE MISSING BEFORE BASIC FUNCTIONS:
    ## 
    ## LINE FUN     STRING_BEFORE
    ## 3    sum     

    ## MODIFIED CODE EXPORTED IN:
    ## /tmp/RtmpiUjuRE/fun2_safer.R

``` r

base::readLines(con = base::file.path(base::tempdir(), "fun2_safer.R", fsep = "/")) # content of the exported file.
```

    ## [1] "fun2_safer <- function () " "{"                         
    ## [3] "    base::sum(1, 3)"        "}"

\

### All the arguments

``` r

colons_check(
    x = fun1, # function name to test.
    export = FALSE, # export the modified code in a .R file instead of returning it?
    out_path = ".", # folder where to export the modified code (used only if export = TRUE).
    safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
    lib_path = NULL, # where the required packages are if not in the default directories.
    error_text = "" # add information in error messages returned by the function.
)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::colons_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

    ## NULL

\

### Sophisticated example

``` r

source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R")
colons_check(
    x = test
)
```

    ## 
    ## 
    ## INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE BASIC FUNCTIONS:
    ## 
    ## LINE FUN     STRING_BEFORE
    ## 5    gregexpr        matches <- 
    ## 8    regmatches      matched_strings <- 
    ## 11   sum     
    ## 18   sub     result <- 
    ## 19   range       
    ## 22   return      
    ## 
    ## INSIDE test(), INTERNAL FUNCTION DETECTED:
    ## FUN1
    ## 
    ## INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE OTHER FUNCTIONS:
    ## 
    ## LINE FUN     STRING_BEFORE
    ## 16   roc1        base::length(
    ## 20   roc4        tempo.cat <- base::paste0("IAGE\nLENGTHS OF roc00() (", base::ks.test(

    ## Warning: FROM saferDev::colons_check().
    ## 
    ## (1) INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS:
    ## 
    ## LINE FUN     STRING_BEFORE
    ## 16   roc1        base::length(
    ## 20   roc4        tempo.cat <- base::paste0("IAGE\nLENGTHS OF roc00() (", base::ks.test(
    ## 
    ## THE PACKAGE ATTRIBUTIONS OF THESE FUNCTIONS CANNOT BE ADDED AUTOMATICALLY, BECAUSE THE CORRECT PACKAGE CANNOT BE DETERMINED RELIABLY.
    ## PLEASE, MODIFY THE CODE MANUALLY, WITH THE HELP OF THE MESSAGE PRINTED ABOVE.

    ## function (text, pattern) 
    ## {
    ##     FUN1 <- function() {
    ##     }
    ##     matches <- base::gregexpr(pattern = base::paste0(pattern, "\\(#"), 
    ##         text = text)
    ##     dt <- base::c(2:8)
    ##     matched_strings <- base::regmatches(x = text, m = matches)[[1]]
    ##     FUN1()
    ##     tempo4 <- a$regmatches(x = text, m = matches)[[1]]
    ##     base::sum(1:3)
    ##     a$regmatches(x = 1)
    ##     tempo5 <- a$count
    ##     tempo.cat <- base::paste0("INTERNAL ERROR 4 IN ", function.name, 
    ##         " OF THE ", package.name, " PACKAGE\nLENGTHS OF col1 (", 
    ##         base::length(roc1()), "), col2 (", base::length(roc2), 
    ##         "), AND col3 (", base::length(roc3), "), SHOULD BE EQUAL\n")
    ##     result <- base::sub("\\($##", "", matched_strings)
    ##     base::range(1:3)
    ##     tempo.cat <- base::paste0("IAGE\nLENGTHS OF roc00() (", base::ks.test(roc4()), 
    ##         "), and roc0 (", base::length(roc5), ") SHOULL\n")
    ##     base::return(baba)
    ##     base::return(bobo)
    ## }

\

### Returned value with the returned modified function

``` r

# The modified function is returned (NULL if no basic function required a package attribution).
# The returned function carries the environment of the submitted function,
# so that the functions used inside it still resolve the same way.
source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R")
res <- colons_check(x = test)
```

    ## 
    ## 
    ## INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE BASIC FUNCTIONS:
    ## 
    ## LINE FUN     STRING_BEFORE
    ## 5    gregexpr        matches <- 
    ## 8    regmatches      matched_strings <- 
    ## 11   sum     
    ## 18   sub     result <- 
    ## 19   range       
    ## 22   return      
    ## 
    ## INSIDE test(), INTERNAL FUNCTION DETECTED:
    ## FUN1
    ## 
    ## INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE OTHER FUNCTIONS:
    ## 
    ## LINE FUN     STRING_BEFORE
    ## 16   roc1        base::length(
    ## 20   roc4        tempo.cat <- base::paste0("IAGE\nLENGTHS OF roc00() (", base::ks.test(

    ## Warning: FROM saferDev::colons_check().
    ## 
    ## (1) INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS:
    ## 
    ## LINE FUN     STRING_BEFORE
    ## 16   roc1        base::length(
    ## 20   roc4        tempo.cat <- base::paste0("IAGE\nLENGTHS OF roc00() (", base::ks.test(
    ## 
    ## THE PACKAGE ATTRIBUTIONS OF THESE FUNCTIONS CANNOT BE ADDED AUTOMATICALLY, BECAUSE THE CORRECT PACKAGE CANNOT BE DETERMINED RELIABLY.
    ## PLEASE, MODIFY THE CODE MANUALLY, WITH THE HELP OF THE MESSAGE PRINTED ABOVE.

``` r

base::is.null(x = res) # NULL only if no modification was performed.
```

    ## [1] FALSE

``` r

base::is.function(x = res)
```

    ## [1] TRUE

``` r

# Beware: the returned function is the analyzed code, i.e., without the comments
# of the submitted function, because colons_check() does not analyze the comments.
```

\
