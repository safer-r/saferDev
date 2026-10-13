# colons_check: more examples

  

``` r
library(saferDev)
```

  

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

  

### All the arguments

``` r
colons_check(
    x = fun1, # function name to test.
    safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
    lib_path = NULL, # where the required packages are if not in the default directories.
    error_text = "" # add information in error messages returned by the function.
)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::colons_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

  

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

  
