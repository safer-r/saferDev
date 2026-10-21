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
    ## ADD THE MISSING <PACKAGE>::<FUNCTION> (OR <PACKAGE>:::<FUNCTION> FOR FUNCTION STARTING BY A DOT)
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
    ## ERROR IN saferDev:::.all_args_here_fill() INSIDE saferDev::all_args_here().
    ## 
    ## THE TESTED FUNCTION test2 SEEMS TO HAVE A WRITING ERROR IN LINE 13 AND FUNCTION length.
    ## PLEASE, RUN THE TESTED FUNCTION FIRST.
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
    ## ./res.tsv

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

``` r

FUN2_safer # modified function (or NULL when the code was already clean).
```

    ## function (x, y) 
    ## {
    ##     middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, 
    ##         quote = FALSE, envir = base::parent.frame(n = 1))
    ## }
    ## <environment: 0x560752583b88>

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

    ## MODIFIED CODE EXPORTED IN:
    ## ./FUN2_safer.R

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
    ## CORRECTIONS NOT APPLIED:
    ## line 8: mapply(x = x, y = y,FUN = function(x,  y) { base::rep(x = y, base::length(x = x)) }, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
    ## line 8: unlist(x = base::mapply(FUN = function(x,  y) { base::rep(x = y, base::length(x = x)) }, x = x, y = y), recursive = TRUE, use.names = TRUE)
    ## line 8: as.vector(x = base::unlist(x = base::mapply(FUN = function(x,  y) { base::rep(x = y, base::length(x = x)) }, x = x, y = y)), mode = "any")

    ## CORRECTIONS NOT APPLIED:
    ## line 8: mapply(x = x, y = y,FUN = function(x,  y) { base::rep(x = y, base::length(x = x)) }, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
    ## line 8: unlist(x = base::mapply(FUN = function(x,  y) { base::rep(x = y, base::length(x = x)) }, x = x, y = y), recursive = TRUE, use.names = TRUE)
    ## line 8: as.vector(x = base::unlist(x = base::mapply(FUN = function(x,  y) { base::rep(x = y, base::length(x = x)) }, x = x, y = y)), mode = "any")

\
