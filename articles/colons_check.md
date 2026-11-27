# colons_check(): more examples

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
# Here everything is fine: NULL is returned invisibly.
saferDev::colons_check(x = fun1, export = FALSE, out_path = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::colons_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

``` r

# Here one missing package prefix: the corrected function is returned.
saferDev::colons_check(x = fun2, export = FALSE, out_path = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
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

# Safer checks are performed before running the main code.
saferDev::colons_check(
    x = fun1,
    export = FALSE,
    out_path = NULL,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::colons_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

\

### Argument `lib_path`

``` r

# The "NOTGOOD" path does not exist.
saferDev::colons_check(
    x = fun1,
    export = FALSE,
    out_path = NULL,
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
saferDev::colons_check(
    x = fun1,
    export = FALSE,
    out_path = NULL,
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
    ## ERROR IN saferDev::colons_check() === TEXT ADDED ===
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Arguments `export` and `out_path`

``` r

# The package prefixes of the basic functions are added automatically.
# The modified function is returned (NULL, invisibly, if nothing has to be modified).
res <- saferDev::colons_check(x = fun2, export = FALSE, out_path = NULL, safer_check = TRUE, lib_path = NULL, error_text = "") # sum() has become base::sum()
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

# Alternatively, the modified function can be written into a .R file (export = TRUE).
# The file is named <function name>_safer.R, here fun2_safer.R, and is written into the out_path folder.
# Nothing is written and NULL is returned if nothing has to be modified.
# An error is returned if out_path is NULL (default) or if the file already exists (never overwritten).
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(check = FALSE), fileext = "") # new temporary folder.
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
saferDev::colons_check(
    x = fun2,
    export = TRUE,
    out_path = tempo_dir, # folder where to write fun2_safer.R.
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
)
```

    ## 
    ## 
    ## INSIDE fun2(), SOME :: OR ::: ARE MISSING BEFORE BASIC FUNCTIONS:
    ## 
    ## LINE FUN     STRING_BEFORE
    ## 3    sum     

    ## MODIFIED CODE EXPORTED IN:
    ## /tmp/RtmptEGmZo/dir14072e4e0633/fun2_safer.R

``` r

base::readLines(con = base::file.path(tempo_dir, "fun2_safer.R", fsep = "/"), n = -1L, ok = TRUE, warn = TRUE, encoding = "unknown", skipNul = FALSE) # content of the written file.
```

    ## [1] "fun2_safer <- function () " "{"                         
    ## [3] "    base::sum(1, 3)"        "}"

\

### All the arguments

``` r

saferDev::colons_check(
    x = fun1, # function to check.
    export = FALSE, # write the modified function into a .R file instead of returning it?
    out_path = NULL, # folder where to write the modified function (required if export = TRUE).
    safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
    lib_path = NULL, # where the required packages are if not in the default directories.
    error_text = "" # add information in error messages returned by the function.
)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::colons_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

\

### Masked functions

``` r

# A basic function name is not modified when the function called when running the checked function
# is another function of the same name (attached package, imported function or function defined by the user).
# Here, the filter() function defined by the user masks stats::filter():
filter <- function(.data, ...){
    "user filter"
}
fun3 <- function(df){
    y <- filter(df) # calls the filter() function of the user, not stats::filter().
    z <- mean(1:3)
    y
}
res <- saferDev::colons_check(x = fun3, export = FALSE, out_path = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
```

    ## 
    ## 
    ## INSIDE fun3(), SOME :: OR ::: ARE MISSING BEFORE BASIC FUNCTIONS:
    ## 
    ## LINE FUN     STRING_BEFORE
    ## 4    mean        z <- 
    ## 
    ## INSIDE fun3(), SOME :: OR ::: ARE MISSING BEFORE OTHER FUNCTIONS:
    ## 
    ## LINE FUN     STRING_BEFORE
    ## 3    filter      y <-

    ## Warning: FROM saferDev::colons_check().
    ## 
    ## (1) INSIDE fun3(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS:
    ## 
    ## LINE FUN     STRING_BEFORE
    ## 3    filter      y <- 
    ## 
    ## AMONG THEM, THESE NAMES OF R BASIC FUNCTIONS ARE MASKED: THE FUNCTION CALLED WHEN RUNNING fun3() IS ANOTHER FUNCTION (ATTACHED PACKAGE, IMPORT OR USER DEFINITION):
    ## filter
    ## 
    ## THE PACKAGE ATTRIBUTIONS OF THESE FUNCTIONS CANNOT BE ADDED AUTOMATICALLY, BECAUSE THE CORRECT PACKAGE CANNOT BE DETERMINED RELIABLY.
    ## PLEASE, MODIFY THE CODE MANUALLY, WITH THE HELP OF THE MESSAGE PRINTED ABOVE.

``` r

res # mean() has become base::mean(), filter() is not modified.
```

    ## function (df) 
    ## {
    ##     y <- filter(df)
    ##     z <- base::mean(1:3)
    ##     y
    ## }

``` r

res(1) # still calls the filter() function of the user.
```

    ## [1] "user filter"

``` r

base::rm(list = "filter", pos = -1, envir = base::environment(fun = NULL), inherits = FALSE)
```

\

### Sophisticated example

``` r

test <- function(
        text,
        pattern
    ){
        # AIM
        # extract all function names
        # ARGUMENTS
        # text: vector of strings
        # pattern: regex to extract function names
        # RETURN
        # A list containing the functions names, each compartment being one of the string of the input vector
        # DEBUGGING
        # text = ini[1] ; pattern = pattern
        FUN1 <- function(){}
        #### Find all matches, including trailing '(' #
        matches <- gregexpr(pattern = base::paste0(pattern, "\\(#"), text = text) # # test
        dt <- base::c(2:8)
        matched_strings <- regmatches(x = text, m = matches)[[1]]
        FUN1()
        # Remove trailing '(' from each match #
        tempo4 <- a$regmatches(x = text, m = matches)[[1]] ; sum(1:3) ; a$regmatches(x = 1)
        tempo5 <- a$count
        tempo_cat <- base::paste0("INTERNAL ERROR 4 IN ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTHS OF col1 (", base::length(roc1()), "), col2 (", base::length(roc2), "), AND col3 (", base::length(roc3), "), SHOULD BE EQUAL\n")
        result <- sub("\\($##", "", matched_strings) ; range(1:3) # sub("\\($##", "", matched_strings)
        tempo_cat <- base::paste0("IAGE\nLENGTHS OF roc00() (", base::ks.test(roc4()), "), and roc0 (", base::length(roc5), ") SHOULL\n")
        return(baba) # base::sub("\\($##", "", matched_strings)
        base::return(bobo) # a$sub("\\($##", "", matched_strings)
    }
saferDev::colons_check(x = test, export = FALSE, out_path = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
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
    ## 20   roc4        tempo_cat <- base::paste0("IAGE\nLENGTHS OF roc00() (", base::ks.test(

    ## Warning: FROM saferDev::colons_check().
    ## 
    ## (1) INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS:
    ## 
    ## LINE FUN     STRING_BEFORE
    ## 16   roc1        base::length(
    ## 20   roc4        tempo_cat <- base::paste0("IAGE\nLENGTHS OF roc00() (", base::ks.test(
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
    ##     tempo_cat <- base::paste0("INTERNAL ERROR 4 IN ", function_name, 
    ##         " OF THE ", package_name, " PACKAGE\nLENGTHS OF col1 (", 
    ##         base::length(roc1()), "), col2 (", base::length(roc2), 
    ##         "), AND col3 (", base::length(roc3), "), SHOULD BE EQUAL\n")
    ##     result <- base::sub("\\($##", "", matched_strings)
    ##     base::range(1:3)
    ##     tempo_cat <- base::paste0("IAGE\nLENGTHS OF roc00() (", base::ks.test(roc4()), 
    ##         "), and roc0 (", base::length(roc5), ") SHOULL\n")
    ##     base::return(baba)
    ##     base::return(bobo)
    ## }

\

### Returned value with the returned modified function

``` r

# The modified function is returned (NULL, invisibly, if no basic function required a package prefix).
# The returned function keeps the environment of the submitted function,
# so that the functions used inside it still resolve the same way.
res <- saferDev::colons_check(x = test, export = FALSE, out_path = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
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
    ## 20   roc4        tempo_cat <- base::paste0("IAGE\nLENGTHS OF roc00() (", base::ks.test(

    ## Warning: FROM saferDev::colons_check().
    ## 
    ## (1) INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS:
    ## 
    ## LINE FUN     STRING_BEFORE
    ## 16   roc1        base::length(
    ## 20   roc4        tempo_cat <- base::paste0("IAGE\nLENGTHS OF roc00() (", base::ks.test(
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

base::identical(x = base::environment(fun = res), y = base::environment(fun = test), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)
```

    ## [1] TRUE

``` r

# The comments and the indentation of the submitted function are kept when it carries its source
# references (e.g., a function defined in the console). Otherwise (e.g., functions of installed
# packages, or functions defined in this article, built by knitr), the deparsed code is used, without comments.
```

\
