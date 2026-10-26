# saferDev

## Description

Set of functions for the development of R functions and pipelines,
written according to the [safer-R project](https://github.com/safer-r)
specifications.

\

## Content

\

| Function | Description |
|:---|:---|
| **all_args_here()** | Check that all the functions used inside a function are written with all their arguments. For instance: `base::paste0(letters[1:2], collapse = NULL, recycle0 = FALSE)` and not `base::paste0(letters[1:2])`. Return table message of the line with problems and a corrected version of the submitted function. |
| **arg_check()** | Check expected values of arguments of functions: class, type, mode, length (each tested exactly, same values, in the same order), restricted values panel, kind of numeric values in addition to the distinction between ‘integer’ and ‘double’ (proportion only? Inf values authorized? negative values authorized? Integers of type ‘double’?). |
| **colons_check()** | Check that all the functions used inside a function are all referenced by their package attribution. For instance: [`base::mean()`](https://rdrr.io/r/base/mean.html) and not [`mean()`](https://rdrr.io/r/base/mean.html), or `saferDev:::.base_op_check()` and not [`.base_op_check()`](../reference/dot-base_op_check.md). Return table message of the line with problems and a corrected version of the submitted function. |
| **curly_bracket_check()** | Verify that all the bodies of the `if`, `else`, `for`, `while`, `repeat` and `function` are delimited by curly brackets [`{ }`](https://rdrr.io/r/base/Paren.html). Return table message of the line with problems and a corrected version of the submitted function. |
| **for_loop_nb_check()** | Check the sequence expression of every `for` loop of a function. If the number of iterations is not fixed by the writing itself (e.g., `1:length(x)`), a checking line is added before the loop, issuing a warning at execution if the sequence is empty, contains `NA` or infinite values, has a single value, or is decreasing. Return a table-like message of the problematic lines and a corrected version of the submitted function. |
| **lambda_check()** | Verify that the lambda functions written with the shorthand `\(x)` (like `\(x) x * 2`) are all written with the standard `function(x)` writing. Return table message of the line with problems and a corrected version of the submitted function. Warning: the `\(x)` writing is only kept in the code of a function carrying its source references (attribute `srcref`). |
| **env_check()** | Verify that object names in the environment defined by the `pos` argument are identical or not to object names in the above environments (following R Scope). This can be used to verify that names used for objects inside a function or in the working environment do not override names of objects already present in the above R environments, following the R scope. |
| **get_message()** | Return the error, warning or simple (non error non warning) message if ever exist of an instruction written between quotes. |
| **is_function_here()** | Check if required functions are present in installed packages. This controls for instance modifications of function names depending on package versions. |
| **is_package_here()** | Check if required packages are installed locally. |
| **report()** | Print a character string or a data object into a same log file. |
| **super_assign_check()** | Verify that the superassignment operators `<<-` and `->>` are not used in a function. Return an error with a table of the lines with problems if any: these operators are not allowed in safer-r functions, because what they modify depends on the environments where the function is defined and called, so that it is difficult to guess what the writer had in mind (nothing can be corrected automatically). |
| **true_false_check()** | Verify that the `TRUE` and `FALSE` logical constants are always written in full when used as values (argument values, conditions, comparisons, indexing) and never abbreviated as `T` or `F`, which can be overwritten by the user. Return table message of the line with problems and a corrected version of the submitted function. |

\

## Getting started: check and correct a function

Each checking function prints a table-like message of the problematic
lines and returns the corrected function (or `NULL` if nothing has to be
corrected). With `export = TRUE`, the corrected function is written into
a `<function name>_safer.R` file instead. Exception:
[`super_assign_check()`](../reference/super_assign_check.md) returns an
error if any `<<-` or `->>` is detected, because what these operators
modify cannot be guessed from the code.

Recommended order:
[`super_assign_check()`](../reference/super_assign_check.md) (error
only, nothing corrected automatically),
[`lambda_check()`](../reference/lambda_check.md),
[`colons_check()`](../reference/colons_check.md) and
[`all_args_here()`](../reference/all_args_here.md) (these two require
the previous steps), then
[`curly_bracket_check()`](../reference/curly_bracket_check.md),
[`true_false_check()`](../reference/true_false_check.md) and
[`for_loop_nb_check()`](../reference/for_loop_nb_check.md).

``` r

library(saferDev)
# function with missing package attributions, missing arguments, missing curly brackets,
# an abbreviated logical value and a for() sequence not written with hard numbers
fun1 <- function(x, y = T){
    if(x > 1) z <- mean(x)
    for(i in 1:length(y)){
        z <- base::paste0(z, i)
    }
    z
}
fun2 <- colons_check(x = fun1) # base:: added before mean() and length()
```

    ## 
    ## 
    ## INSIDE fun1(), SOME :: OR ::: ARE MISSING BEFORE BASIC FUNCTIONS:
    ## 
    ## LINE FUN     STRING_BEFORE
    ## 4    mean        z <- 
    ## 5    length      for (i in 1:

``` r

fun3 <- all_args_here(x = fun2) # missing arguments added
```

    ## 
    ## INSIDE fun2(), ARGUMENTS ARE MISSING.

    ##   LINE_NB FUN_NAME     FUN_ARGS FUN_POS                               DEF_ARGS
    ## 1       4     mean      mean(x)      12                                 x, ...
    ## 2       5   length    length(y)      19                                      x
    ## 3       6   paste0 paste0(z, i)      12 ..., collapse = NULL, recycle0 = FALSE
    ##    MISSING_ARG_NAMES                      MISSING_ARGS
    ## 1                  x                             x = x
    ## 2                  x                             x = y
    ## 3 collapse, recycle0 collapse = NULL, recycle0 = FALSE
    ##                                            STATUS
    ## 1                                     mean(x = x)
    ## 2                                   length(x = y)
    ## 3 paste0(z, i, collapse = NULL, recycle0 = FALSE)

``` r

fun4 <- curly_bracket_check(x = fun3) # { } added after if()
```

    ## 
    ## 
    ## 
    ## INSIDE fun3(), SOME CURLY BRACKETS {} ARE MISSING AFTER:
    ## 
    ## LINE CONSTRUCT       CODE
    ## 3    if      if (x > 1)

``` r

fun5 <- true_false_check(x = fun4) # T replaced by TRUE
```

    ## 
    ## 
    ## 
    ## INSIDE fun4(), SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE:
    ## 
    ## LINE SYMBOL      CODE
    ## 1    T       function (x, y = T)

``` r

fun5
```

    ## function (x, y = TRUE) 
    ## {
    ##     if (x > 1) {
    ##         z <- base::mean(x = x)
    ##     }
    ##     for (i in 1:base::length(x = y)) {
    ##         z <- base::paste0(z, i, collapse = NULL, recycle0 = FALSE)
    ##     }
    ##     z
    ## }
    ## <environment: 0x559fd49d9a80>

``` r

fun6 <- for_loop_nb_check(x = fun5) # checking line added before the for() loop
```

    ## 
    ## 
    ## 
    ## INSIDE fun5(), SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:
    ## 
    ## LINE CONSTRUCT       CODE
    ## 6    for     for (i in 1:base::length(x = y)) {

``` r

fun6(x = 2, y = NULL) # 1:length(NULL) is c(1, 0): a warning is now returned
```

    ## Warning: FOR SEQUENCE DECREASING

    ## [1] "210"

\

See the [website](https://safer-r.github.io/saferDev/) for one article
with more examples per function.

\
