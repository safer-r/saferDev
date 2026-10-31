# all_args_here(): more examples

\

``` r

library(saferDev)
```

\

### Dataset

``` r

source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test2.R") # function.
source("https://raw.githubusercontent.com/safer-r/.github/refs/heads/main/profile/backbone.R") # function.
```

    ## Warning in file(filename, "r", encoding = encoding): cannot open URL
    ## 'https://raw.githubusercontent.com/safer-r/.github/refs/heads/main/profile/backbone.R':
    ## HTTP status was '404 Not Found'

    ## Error in `file()`:
    ## ! cannot open the connection to 'https://raw.githubusercontent.com/safer-r/.github/refs/heads/main/profile/backbone.R'

``` r

FUN1 <- function(x, y){ # function.
    code_for_col <- base::as.vector(
        x = base::unlist(
            x = base::mapply(
                FUN = function(x, y){
                    base::rep(
                        x = y, 
                        base::length(x = x)
                    )
                }, 
                x = x, 
                y = y, 
                MoreArgs = NULL, 
                SIMPLIFY = TRUE, 
                USE.NAMES = TRUE
            ), 
            recursive = TRUE, 
            use.names = TRUE
        ), 
        mode = "any"
    )
    # The code below is written without carriage return on purpose.
    code_for_col2 <- base::as.vector(x = base::unlist(x = base::mapply(FUN = function(x, y){base::rep(x = y, base::length(x = x))}, x = x, y = y))) 
    middle_bracket <- base::do.call(what = base::c, args = code_for_col)
    middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, quote = FALSE, envir = base::parent.frame())
}
FUN2 <- function(x, y){ # function.
    middle_bracket2 <- base::do.call(
        what = base::c, 
        args = code_for_col, 
        quote = FALSE, 
        envir = base::parent.frame()
    )
}
```

\

### Simple examples

``` r

# Example that returns an error.
all_args_here(mean)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::all_args_here().
    ## 
    ## CANNOT GET THE ARGUMENTS OF A FUNCTION THAT IS NOT ASSOCIATED TO ITS PACKAGE IN LINE 2:
    ## 
    ## UseMethod("mean")
    ## 
    ## PLEASE, RUN saferDev::colons_check(mean) FIRST,
    ## AND RERUN saferDev::all_args_here(mean).
    ## 
    ## ================

``` r

# Example that returns an error.
all_args_here(test2)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::is_function_here() INSIDE saferDev::all_args_here().
    ## 
    ## REQUIRED FUNCTION IS MISSING IN THE INSTALLED PACKAGE:
    ## base::ks.test
    ## 
    ## IN ONE OF THESE FOLDERS:
    ## /__w/_temp/Library
    ## /usr/local/lib/R/site-library
    ## /usr/local/lib/R/library
    ## 
    ## ================

``` r

# Example indicating missing arguments.
all_args_here(
    x = FUN2
)
```

    ## 
    ## INSIDE FUN2(), ARGUMENTS ARE MISSING.

    ##   LINE_NB     FUN_NAME
    ## 1       3      do.call
    ## 2       4 parent.frame
    ##                                                                                     FUN_ARGS
    ## 1 do.call(what = base::c, args = code_for_col,  quote = FALSE, envir = base::parent.frame())
    ## 2                                                                             parent.frame()
    ##   FUN_POS                                          DEF_ARGS MISSING_ARG_NAMES
    ## 1      26 what, args, quote = FALSE, envir = parent.frame()                  
    ## 2      30                                             n = 1                 n
    ##   MISSING_ARGS              STATUS
    ## 1                             GOOD
    ## 2        n = 1 parent.frame(n = 1)

    ## function (x, y) 
    ## {
    ##     middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, 
    ##         quote = FALSE, envir = base::parent.frame(n = 1))
    ## }

\

### Arguments `table_export`, `fun_export`, `out_path`, `df_name` and `overwrite`

``` r

# Print the results into the res.tsv file written in the "." path, 
# and overwrite if res.tsv already exists in ".".
all_args_here(
    x = FUN2, 
    table_export = TRUE, 
    out_path = ".", 
    df_name = "res.tsv", 
    overwrite = TRUE
)
```

    ## 
    ## INSIDE FUN2(), ARGUMENTS ARE MISSING.
    ## RESULT EXPORTED IN:
    ## /__w/saferDev/saferDev/vignettes/articles/res.tsv

    ## function (x, y) 
    ## {
    ##     middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, 
    ##         quote = FALSE, envir = base::parent.frame(n = 1))
    ## }

\

### Argument `fun_export`: return or export the modified function

When arguments are missing in the code of the submitted function,
[`all_args_here()`](../reference/all_args_here.md) can return the
corrected function (with the missing arguments written as proposed in
the `$STATUS` column of the output data frame), or export its code into
a `.R` file named with the submitted function name and `_safer` added at
the end:

``` r

# Returns the modified function, or NULL if nothing has to be modified.
FUN2_safer <- all_args_here(
    x = FUN2
)
```

    ## 
    ## INSIDE FUN2(), ARGUMENTS ARE MISSING.

    ##   LINE_NB     FUN_NAME
    ## 1       3      do.call
    ## 2       4 parent.frame
    ##                                                                                     FUN_ARGS
    ## 1 do.call(what = base::c, args = code_for_col,  quote = FALSE, envir = base::parent.frame())
    ## 2                                                                             parent.frame()
    ##   FUN_POS                                          DEF_ARGS MISSING_ARG_NAMES
    ## 1      26 what, args, quote = FALSE, envir = parent.frame()                  
    ## 2      30                                             n = 1                 n
    ##   MISSING_ARGS              STATUS
    ## 1                             GOOD
    ## 2        n = 1 parent.frame(n = 1)

``` r

FUN2_safer # modified function (or NULL when the code was already clean).
```

    ## function (x, y) 
    ## {
    ##     middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, 
    ##         quote = FALSE, envir = base::parent.frame(n = 1))
    ## }

``` r

# Export the modified function into the FUN2_safer.R file, in the "." path.
all_args_here(
    x = FUN2,
    fun_export = TRUE,
    out_path = ".", 
    overwrite = TRUE
)
```

    ## 
    ## INSIDE FUN2(), ARGUMENTS ARE MISSING.
    ## 
    ## 
    ##   LINE_NB     FUN_NAME
    ## 1       3      do.call
    ## 2       4 parent.frame
    ##                                                                                     FUN_ARGS
    ## 1 do.call(what = base::c, args = code_for_col,  quote = FALSE, envir = base::parent.frame())
    ## 2                                                                             parent.frame()
    ##   FUN_POS                                          DEF_ARGS MISSING_ARG_NAMES
    ## 1      26 what, args, quote = FALSE, envir = parent.frame()                  
    ## 2      30                                             n = 1                 n
    ##   MISSING_ARGS              STATUS
    ## 1                             GOOD
    ## 2        n = 1 parent.frame(n = 1)

    ## MODIFIED CODE EXPORTED IN:
    ## /__w/saferDev/saferDev/vignettes/articles/FUN2_safer.R

When `fun_export = TRUE`, the modified function is not returned by the
function but only exported (the file is not created when no correction
has been applied).\

### Argument `safer_check`

``` r

# Safer checkings are performed before main code running.
all_args_here(
    x = FUN2, 
    safer_check = TRUE
)
```

    ## 
    ## INSIDE FUN2(), ARGUMENTS ARE MISSING.

    ##   LINE_NB     FUN_NAME
    ## 1       3      do.call
    ## 2       4 parent.frame
    ##                                                                                     FUN_ARGS
    ## 1 do.call(what = base::c, args = code_for_col,  quote = FALSE, envir = base::parent.frame())
    ## 2                                                                             parent.frame()
    ##   FUN_POS                                          DEF_ARGS MISSING_ARG_NAMES
    ## 1      26 what, args, quote = FALSE, envir = parent.frame()                  
    ## 2      30                                             n = 1                 n
    ##   MISSING_ARGS              STATUS
    ## 1                             GOOD
    ## 2        n = 1 parent.frame(n = 1)

    ## function (x, y) 
    ## {
    ##     middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, 
    ##         quote = FALSE, envir = base::parent.frame(n = 1))
    ## }

\

### Argument `lib_path`

``` r

# The "NOTGOOD" path does not exists.
all_args_here(
    x = FUN2,  
    lib_path = "NOTGOOD"
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::all_args_here().
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Argument `error_text`

``` r

# Add a text in error messages returned by all_args_here().
all_args_here(
    x = FUN2, 
    lib_path = "NOTGOOD", 
    error_text = " === TEXT ADDED ==="
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::all_args_here() === TEXT ADDED ===
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### All the arguments

``` r

all_args_here(
    x = FUN1, # R function.
    table_export = FALSE, # export the data frame into a .tsv file?
    fun_export = FALSE, # export the modified function into a .R file?
    out_path = ".", # pathway of the folder where to export the data frame or the modified function.
    df_name = "res.tsv", # name of the exported data frame file.
    overwrite = FALSE, # overwrite potential files already existing in out_path?
    safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
    lib_path = NULL, # where the required packages are if not in the default directories.
    error_text = "" # add information in error messages returned by the function.
)
```

    ## 
    ## INSIDE FUN1(), ARGUMENTS ARE MISSING.

    ##    LINE_NB     FUN_NAME
    ## 1        3    as.vector
    ## 2        3       unlist
    ## 3        3       mapply
    ## 4        5          rep
    ## 5        5       length
    ## 6        8    as.vector
    ## 7        8       unlist
    ## 8        8       mapply
    ## 9       10          rep
    ## 10      10       length
    ## 11      12      do.call
    ## 12      13      do.call
    ## 13      14 parent.frame
    ##                                                                                                                                                                                                                               FUN_ARGS
    ## 1  as.vector(x = base::unlist(x = base::mapply(FUN = function(x,  y) { base::rep(x = y, base::length(x = x)) }, x = x, y = y, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE),  recursive = TRUE, use.names = TRUE), mode = "any")
    ## 2                                     unlist(x = base::mapply(FUN = function(x,  y) { base::rep(x = y, base::length(x = x)) }, x = x, y = y, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE),  recursive = TRUE, use.names = TRUE)
    ## 3                                                                                            mapply(FUN = function(x,  y) { base::rep(x = y, base::length(x = x)) }, x = x, y = y, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
    ## 4                                                                                                                                                                                                      rep(x = y, base::length(x = x))
    ## 5                                                                                                                                                                                                                        length(x = x)
    ## 6                                                                                                         as.vector(x = base::unlist(x = base::mapply(FUN = function(x,  y) { base::rep(x = y, base::length(x = x)) }, x = x, y = y)))
    ## 7                                                                                                                              unlist(x = base::mapply(FUN = function(x,  y) { base::rep(x = y, base::length(x = x)) }, x = x, y = y))
    ## 8                                                                                                                                                mapply(FUN = function(x,  y) { base::rep(x = y, base::length(x = x)) }, x = x, y = y)
    ## 9                                                                                                                                                                                                      rep(x = y, base::length(x = x))
    ## 10                                                                                                                                                                                                                       length(x = x)
    ## 11                                                                                                                                                                                        do.call(what = base::c, args = code_for_col)
    ## 12                                                                                                                                          do.call(what = base::c, args = code_for_col,  quote = FALSE, envir = base::parent.frame())
    ## 13                                                                                                                                                                                                                      parent.frame()
    ##    FUN_POS                                                     DEF_ARGS
    ## 1       23                                              x, mode = "any"
    ## 2       43                        x, recursive = TRUE, use.names = TRUE
    ## 3       60 FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE
    ## 4        7                                                       x, ...
    ## 5       24                                                            x
    ## 6       24                                              x, mode = "any"
    ## 7       44                        x, recursive = TRUE, use.names = TRUE
    ## 8       61 FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE
    ## 9        7                                                       x, ...
    ## 10      24                                                            x
    ## 11      25            what, args, quote = FALSE, envir = parent.frame()
    ## 12      26            what, args, quote = FALSE, envir = parent.frame()
    ## 13      30                                                        n = 1
    ##                MISSING_ARG_NAMES
    ## 1                               
    ## 2                               
    ## 3                               
    ## 4                               
    ## 5                               
    ## 6                           mode
    ## 7           recursive, use.names
    ## 8  MoreArgs, SIMPLIFY, USE.NAMES
    ## 9                               
    ## 10                              
    ## 11                  quote, envir
    ## 12                              
    ## 13                             n
    ##                                          MISSING_ARGS
    ## 1                                                    
    ## 2                                                    
    ## 3                                                    
    ## 4                                                    
    ## 5                                                    
    ## 6                                        mode = "any"
    ## 7                  recursive = TRUE, use.names = TRUE
    ## 8  MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE
    ## 9                                                    
    ## 10                                                   
    ## 11                            quote = FALSE, envir = 
    ## 12                                                   
    ## 13                                              n = 1
    ##                                                                                                                                         STATUS
    ## 1                                                                                                                                         GOOD
    ## 2                                                                                                                                         GOOD
    ## 3                                                                                                                                         GOOD
    ## 4                                                                                                                                         GOOD
    ## 5                                                                                                                                         GOOD
    ## 6   as.vector(x = base::unlist(x = base::mapply(FUN = function(x,  y) { base::rep(x = y, base::length(x = x)) }, x = x, y = y)), mode = "any")
    ## 7  unlist(x = base::mapply(FUN = function(x,  y) { base::rep(x = y, base::length(x = x)) }, x = x, y = y), recursive = TRUE, use.names = TRUE)
    ## 8     mapply(x = x, y = y,FUN = function(x,  y) { base::rep(x = y, base::length(x = x)) }, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
    ## 9                                                                                                                                         GOOD
    ## 10                                                                                                                                        GOOD
    ## 11                                                                       do.call(what = base::c, args = code_for_col, quote = FALSE, envir = )
    ## 12                                                                                                                                        GOOD
    ## 13                                                                                                                         parent.frame(n = 1)

    ## ARGUMENTS WITH EMPTY VALUE (DEFAULT VALUE USED BUT CANNOT BE FIXED, OR NO DEFAULT VALUE):
    ## line 12: base::do.call(): envir (DEFAULT: parent.frame())
    ## USE "= ," OR "= )" TO SEEK EACH CASE IN THE CODE, AND, IF POSSIBLE, TEST THAT THE DEFAULT VALUES ARE UNCHANGED, FOR INSTANCE:
    ## testthat::expect_identical(object = base::formals(fun = utils::getFromNamespace)$envir, expected = base::quote(expr = as.environment(pos)))

    ## function (x, y) 
    ## {
    ##     code_for_col <- base::as.vector(x = base::unlist(x = base::mapply(FUN = function(x, 
    ##         y) {
    ##         base::rep(x = y, base::length(x = x))
    ##     }, x = x, y = y, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE), 
    ##         recursive = TRUE, use.names = TRUE), mode = "any")
    ##     code_for_col2 <- base::as.vector(x = base::unlist(x = base::mapply(FUN = function(x, 
    ##         y) {
    ##         base::rep(x = y, base::length(x = x))
    ##     }, x = x, y = y, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE), recursive = TRUE, use.names = TRUE), mode = "any")
    ##     middle_bracket <- base::do.call(what = base::c, args = code_for_col, quote = FALSE, envir = )
    ##     middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, 
    ##         quote = FALSE, envir = base::parent.frame(n = 1))
    ## }

\
