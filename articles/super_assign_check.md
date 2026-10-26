# super_assign_check(): more examples

\

``` r

library(saferDev)
```

\

### Datasets

``` r

# Function without superassignment operator.
fun1 <- function(x){ y <- x ; base::return(y) }
# Function with superassignment operators <<- and ->>, including one inside a nested function.
fun2 <- function(x){ y <<- x ; base::lapply(X = 1:2, FUN = function(i){ i ->> z }) ; base::return(y) }
```

\

### Datasets info

``` r

fun1 # Function.
```

    ## function (x) 
    ## {
    ##     y <- x
    ##     base::return(y)
    ## }

``` r

fun2 # Function.
```

    ## function (x) 
    ## {
    ##     y <<- x
    ##     base::lapply(X = 1:2, FUN = function(i) {
    ##         z <<- i
    ##     })
    ##     base::return(y)
    ## }

\

### Argument `x`

``` r

# Test if fun1 uses the superassignment operators <<- or ->>.
# Here everything is fine: NULL is returned invisibly.
super_assign_check(x = fun1)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::super_assign_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

``` r

# Here fun2 uses them: an error is returned, with the lines to rewrite.
super_assign_check(x = fun2)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::super_assign_check().
    ## 
    ## INSIDE fun2(), THE SUPERASSIGNMENT OPERATORS <<- AND ->> ARE NOT ALLOWED IN SAFER-R FUNCTIONS:
    ## WHAT THEY MODIFY DEPENDS ON THE ENVIRONMENTS WHERE THE FUNCTION IS DEFINED AND CALLED, SO THAT IT IS DIFFICULT TO GUESS WHAT IS EXPECTED.
    ## PLEASE, REWRITE THE CODE, FOR INSTANCE BY RETURNING THE VALUE, OR BY USING base::assign() WITH AN EXPLICIT envir ARGUMENT.
    ## 
    ## LINE SYMBOL      CODE
    ## 3    <<-     y <<- x
    ## 5    <<-     z <<- i
    ## 
    ## ================

\

### Why an error and not a corrected function?

``` r

# What <<- modifies depends on the environments where the function is defined and called.
# Here, a y object is created in the global environment:
exists("y", envir = globalenv(), inherits = FALSE)
```

    ## [1] FALSE

``` r

fun2(1)
```

    ## [1] 1

``` r

exists("y", envir = globalenv(), inherits = FALSE)
```

    ## [1] TRUE

``` r

rm(y, z, envir = globalenv())

# Here, the y object of the enclosing function is modified instead:
fun3 <- function(){ y <- 0 ; fun4 <- function(x){ y <<- x } ; fun4(1) ; y }
fun3()
```

    ## [1] 1

``` r

exists("y", envir = globalenv(), inherits = FALSE)
```

    ## [1] FALSE

``` r

# The writer intention cannot be guessed: the code must be rewritten, for instance by
# returning the value, or by using base::assign() with an explicit envir argument.
```

\

### Argument `safer_check`

``` r

# Safer checkings are performed before main code running.
super_assign_check(
    x = fun1, 
    safer_check = TRUE
)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::super_assign_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

\

### Argument `lib_path`

``` r

# The "NOTGOOD" path does not exists.
super_assign_check(
    x = fun1,  
    lib_path = "NOTGOOD"
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::super_assign_check().
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Argument `error_text`

``` r

# Add a text in error messages returned by super_assign_check().
super_assign_check(
    x = fun2, 
    error_text = " === TEXT ADDED ==="
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::super_assign_check() === TEXT ADDED ===
    ## 
    ## INSIDE fun2(), THE SUPERASSIGNMENT OPERATORS <<- AND ->> ARE NOT ALLOWED IN SAFER-R FUNCTIONS:
    ## WHAT THEY MODIFY DEPENDS ON THE ENVIRONMENTS WHERE THE FUNCTION IS DEFINED AND CALLED, SO THAT IT IS DIFFICULT TO GUESS WHAT IS EXPECTED.
    ## PLEASE, REWRITE THE CODE, FOR INSTANCE BY RETURNING THE VALUE, OR BY USING base::assign() WITH AN EXPLICIT envir ARGUMENT.
    ## 
    ## LINE SYMBOL      CODE
    ## 3    <<-     y <<- x
    ## 5    <<-     z <<- i
    ## 
    ## ================

\

### All the arguments

``` r

super_assign_check(
    x = fun1, # function name to test.
    safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
    lib_path = NULL, # where the required packages are if not in the default directories.
    error_text = "" # add information in error messages returned by the function.
)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::super_assign_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

\

### Sophisticated example

``` r

# A <<- written in a string, in a comment or as a backquoted name is not an operator: ignored.
fun5 <- function(x){ y <- "the <<- writing in a string" ; base::`<<-` ; x } # a <<- in a comment
super_assign_check(x = fun5)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::super_assign_check().
    ## INSIDE fun5(), EVERYTHING SEEMS CLEAN.

\
