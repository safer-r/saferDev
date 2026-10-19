# for_loop_nb_check: more examples

\

``` r

library(saferDev)
```

\

### Datasets

``` r

# Function with all the for() sequences written with hard numbers or seq_len()/seq_along() calls.
fun1 <- function(x){
    for(i in 1:3){
        base::print(i)
    }
    for(i in 0:0){ # equal bounds: not reported by for_loop_nb_check() (target of the runtime checking line)
        base::print(i)
    }
    for(i in -3:-1){ # both negative: not reported by for_loop_nb_check()
        base::print(i)
    }
    for(i in 1:0){ # decreasing: not reported by for_loop_nb_check()
        base::print(i)
    }
    for(i in base::seq_along(along.with = x)){ # recommended replacement of 1:length(x)
        base::print(i)
    }
    for(i in base::seq_len(length.out = 3)){ # recommended replacement of 1:n
        base::print(i)
    }
}
# Function with some for() sequences not written with hard numbers.
fun2 <- function(x){
    for(i in 1:length(x)){ # 1:0 when x is empty: 2 loops
        base::print(i)
    }
    for(i in 1:length(x) - 1){ # 0:-1 when x is empty: 2 loops, and only 0 when x has 1 element
        base::print(i)
    }
    for(i in 1:n){ # n a variable: the sequence content is not predictable from the writing
        base::print(i)
    }
    for(i in 1:(3 - 1)){ # parenthesized constant expression: reported (purely syntactic rule)
        base::print(i)
    }
    for(i in 1:3 - 1){ # gives c(-2, -1) at execution
        base::print(i)
    }
    for(i in seq_along(along.with = x) - 1){ # gives c(0, 1) when x has 2 elements
        base::print(i)
    }
    for(i in 1:2.5){ # silently truncated toward 0
        base::print(i)
    }
    for(i in 1:NA){ # error at execution
        base::print(i)
    }
    for(i in 1:Inf){ # runs for a very long time
        base::print(i)
    }
    for(i in 1:TRUE){ # TRUE is not a numeric literal
        base::print(i)
    }
    for(i in x){ # plain object
        base::print(i)
    }
    for(i in base::rev(x = x)){ # non seq_len()/seq_along() call
        base::print(i)
    }
    for(i in base::seq(x = x)){ # 2 loops when x = 0
        base::print(i)
    }
    for(i in base::names(x = x)){ # non seq_len()/seq_along() call
        base::print(i)
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
    ##     for (i in 1:3) {
    ##         base::print(i)
    ##     }
    ##     for (i in 0:0) {
    ##         base::print(i)
    ##     }
    ##     for (i in -3:-1) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:0) {
    ##         base::print(i)
    ##     }
    ##     for (i in base::seq_along(along.with = x)) {
    ##         base::print(i)
    ##     }
    ##     for (i in base::seq_len(length.out = 3)) {
    ##         base::print(i)
    ##     }
    ## }

``` r

fun2 # Function.
```

    ## function (x) 
    ## {
    ##     for (i in 1:length(x)) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:length(x) - 1) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:n) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:(3 - 1)) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:3 - 1) {
    ##         base::print(i)
    ##     }
    ##     for (i in seq_along(along.with = x) - 1) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:2.5) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:NA) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:Inf) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:TRUE) {
    ##         base::print(i)
    ##     }
    ##     for (i in x) {
    ##         base::print(i)
    ##     }
    ##     for (i in base::rev(x = x)) {
    ##         base::print(i)
    ##     }
    ##     for (i in base::seq(x = x)) {
    ##         base::print(i)
    ##     }
    ##     for (i in base::names(x = x)) {
    ##         base::print(i)
    ##     }
    ## }

\

### Argument `x`

``` r

# Test if all the for() sequences of fun1 are written with hard numbers.
# Here everything is fine.
for_loop_nb_check(x = fun1)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::for_loop_nb_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

``` r

# Here some for() sequences are not written with hard numbers.
for_loop_nb_check(x = fun2)
```

    ## 
    ## 
    ## 
    ## INSIDE fun2(), SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:
    ## 
    ## LINE CONSTRUCT       CODE
    ## 3    for     for (i in 1:length(x)) {
    ## 6    for     for (i in 1:length(x) - 1) {
    ## 9    for     for (i in 1:n) {
    ## 12   for     for (i in 1:(3 - 1)) {
    ## 15   for     for (i in 1:3 - 1) {
    ## 18   for     for (i in seq_along(along.with = x) - 1) {
    ## 21   for     for (i in 1:2.5) {
    ## 24   for     for (i in 1:NA) {
    ## 27   for     for (i in 1:Inf) {
    ## 30   for     for (i in 1:TRUE) {
    ## 33   for     for (i in x) {
    ## 36   for     for (i in base::rev(x = x)) {
    ## 39   for     for (i in base::seq(x = x)) {
    ## 42   for     for (i in base::names(x = x)) {

\

### Argument `safer_check`

``` r

# Safer checkings are performed before main code running.
for_loop_nb_check(
    x = fun1, 
    safer_check = TRUE
)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::for_loop_nb_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

\

### Argument `lib_path`

``` r

# The "NOTGOOD" path does not exists.
for_loop_nb_check(
    x = fun1,  
    lib_path = "NOTGOOD"
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::for_loop_nb_check().
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Argument `error_text`

``` r

# Add a text in error messages returned by for_loop_nb_check().
for_loop_nb_check(
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
    ## ERROR IN saferDev::for_loop_nb_check() === TEXT ADDED ===
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### All the arguments

``` r

for_loop_nb_check(
    x = fun1, # function name to test.
    safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
    lib_path = NULL, # where the required packages are if not in the default directories.
    error_text = "" # add information in error messages returned by the function.
)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::for_loop_nb_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

\

### Sophisticated example

``` r
# The for() sequences written with hard numbers are all clean, whatever the bounds (0:0, 1:0 and -3:-1 are deliberately not reported by for_loop_nb_check(): they are the target of a specific checking line added in the function bodies, which warns at execution when the computed sequence is equal, both negative or decreasing).
fun3 <- function(x){ x <- 0 ; for(i in 0:0){ y <- i } ; y }
for_loop_nb_check(x = fun3)

# But the same writing computed from an object is reported, like 1:length(x), which gives c(1, 0) when x is empty (base::seq_along(x = x) should be used instead).
fun4 <- function(x){ x <- character() ; for(i in 1:length(x)){ y <- i } ; y }
for_loop_nb_check(x = fun4)

# Nested for() loops are all checked.
fun5 <- function(x){
    for(i in 1:3){
        for(i2 in 1:length(x)){ # reported
            base::print(i2)
        }
    }
}
for_loop_nb_check(x = fun5)

# A for() loop inside a formal default is also checked.
fun6 <- function(f = function(y) for(i in y){ base::print(i) }) f(1)
for_loop_nb_check(x = fun6)

# Aliased bounds cannot be detected (documented limitation): n <- length(x) ; for(i in 1:n) is reported as a plain variable, but the aliasing itself is invisible.
fun7 <- function(x){ n <- base::length(x = x) ; for(i in 1:n){ y <- i } ; y }
for_loop_nb_check(x = fun7)

# Backquoted constructs cannot be checked (documented limitation).
fun8 <- function(x){ `for`(i in 1:3, base::print(i)) }
try(for_loop_nb_check(x = fun8), silent = FALSE)
```

    ## Error in parse(text = input): <text>:28:30: unexpected 'in'
    ## 27: # Backquoted constructs cannot be checked (documented limitation).
    ## 28: fun8 <- function(x){ `for`(i in
    ##                                  ^

\
