# all_args_here(): more examples

\

\

### Datasets

``` r

FUN0 <- function(x){ # function with a call written without its package.
    mean(x)
}
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
FUN3 <- function(x, m, d){ # function with calls that cannot be fully corrected.
    a <- base::matrix(x, ncol = 2) # base::matrix() tests missing(nrow)
    b <- base::diag(x) # base::diag() counts its arguments with nargs()
    e <- stats::t.test(formula = y ~ g, data = d) # the method is chosen from the formula, x not written
    f <- base::round(x) # round.POSIXt(), method for date-times, has no digits argument
    base::rownames(m) <- base::paste0("r", 1:2) # target of an assignment
}
# Folder where the files are written.
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(), fileext = "")
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
```

\

### Simple examples

``` r

# Example that returns an error: the calls must be written with their package (see colons_check()).
saferDev::all_args_here(
    x = FUN0,
    table_export = FALSE,
    fun_export = FALSE,
    out_path = NULL,
    df_name = "res.tsv",
    overwrite = FALSE,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::all_args_here().
    ## 
    ## CANNOT GET THE ARGUMENTS OF A FUNCTION THAT IS NOT ASSOCIATED TO ITS PACKAGE IN LINE 3:
    ## 
    ## mean(x)
    ## 
    ## PLEASE, RUN saferDev::colons_check(FUN0) FIRST,
    ## AND RERUN saferDev::all_args_here(FUN0).
    ## 
    ## ================

``` r

# Example indicating missing arguments, and returning the modified function.
saferDev::all_args_here(
    x = FUN2,
    table_export = FALSE,
    fun_export = FALSE,
    out_path = NULL,
    df_name = "res.tsv",
    overwrite = FALSE,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
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

### Empty values and corrections not applied

The default value of `nrow` is not written into the
[`base::matrix()`](https://rdrr.io/r/base/matrix.html) call, because
[`base::matrix()`](https://rdrr.io/r/base/matrix.html) tests
`missing(nrow)`: `nrow` gets an empty value, which keeps the result. The
other calls are left unchanged, and listed with their reason in the
`CORRECTIONS NOT APPLIED` message:
[`base::diag()`](https://rdrr.io/r/base/diag.html) uses
[`nargs()`](https://rdrr.io/r/base/nargs.html), `x =` added to
[`stats::t.test()`](https://rdrr.io/r/stats/t.test.html) would change
the method used, `digits = 0` added to
[`base::round()`](https://rdrr.io/r/base/Round.html) would fail for
date-times, and
[`base::rownames()`](https://rdrr.io/r/base/colnames.html) is the target
of an assignment:

``` r

saferDev::all_args_here(
    x = FUN3,
    table_export = FALSE,
    fun_export = FALSE,
    out_path = NULL,
    df_name = "res.tsv",
    overwrite = FALSE,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
)
```

    ## 
    ## INSIDE FUN3(), ARGUMENTS ARE MISSING.

    ##   LINE_NB FUN_NAME                          FUN_ARGS FUN_POS
    ## 1       3   matrix               matrix(x, ncol = 2)      12
    ## 2       4     diag                           diag(x)      12
    ## 3       5   t.test t.test(formula = y ~ g, data = d)      13
    ## 4       6    round                          round(x)      12
    ## 5       7 rownames                       rownames(m)       7
    ## 6       7   paste0                  paste0("r", 1:2)      28
    ##                                                        DEF_ARGS
    ## 1 data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL
    ## 2                               x = 1, nrow, ncol, names = TRUE
    ## 3                                                        x, ...
    ## 4                                            x, digits = 0, ...
    ## 5                             x, do.NULL = TRUE, prefix = "row"
    ## 6                        ..., collapse = NULL, recycle0 = FALSE
    ##             MISSING_ARG_NAMES                                      MISSING_ARGS
    ## 1 data, nrow, byrow, dimnames data = x, nrow = , byrow = FALSE, dimnames = NULL
    ## 2        x, nrow, ncol, names             x = x, nrow = , ncol = , names = TRUE
    ## 3                           x                                              x = 
    ## 4                   x, digits                                 x = x, digits = 0
    ## 5          x, do.NULL, prefix             x = m, do.NULL = TRUE, prefix = "row"
    ## 6          collapse, recycle0                 collapse = NULL, recycle0 = FALSE
    ##                                                                STATUS
    ## 1 matrix(data = x, nrow = , ncol = 2, byrow = FALSE, dimnames = NULL)
    ## 2                         diag(x = x, nrow = , ncol = , names = TRUE)
    ## 3                              t.test(formula = y ~ g, data = d,x = )
    ## 4                                            round(x = x, digits = 0)
    ## 5                     rownames(x = m, do.NULL = TRUE, prefix = "row")
    ## 6                 paste0("r", 1:2, collapse = NULL, recycle0 = FALSE)

    ## ARGUMENTS WITH EMPTY VALUE (DEFAULT VALUE THAT CANNOT BE COPIED, OR NO DEFAULT VALUE):
    ## line 3: base::matrix(): nrow (DEFAULT: 1, TESTED BY missing())
    ## USE "= ," OR "= )" TO SEEK EACH CASE IN THE CODE, AND, IF POSSIBLE, TEST THAT THE DEFAULT VALUES ARE UNCHANGED, FOR INSTANCE:
    ## testthat::expect_identical(object = base::formals(fun = utils::getFromNamespace)$envir, expected = base::quote(expr = as.environment(pos)))

    ## CORRECTIONS NOT APPLIED:
    ## line 4: diag(x = x, nrow = , ncol = , names = TRUE)
    ##     REASON: the called function counts its arguments with nargs(), so that any added argument can change its result, e.g., base::diag(3) differs from base::diag(x = 3, nrow = , ncol = , names = TRUE). Apply the proposal manually only if the result is unchanged.
    ## line 5: t.test(formula = y ~ g, data = d,x = )
    ##     REASON: the first argument of the called function (S3 generic) is not written in the call, and adding it can change the method used, e.g., x = in stats::t.test(formula = y ~ g, data = d, x = ), which fails at call time. Apply the other arguments manually, if needed.
    ## line 6: round(x = x, digits = 0)
    ##     REASON: a method of the called function (S3 generic) has neither ... nor an argument added by the proposal, so that the corrected call fails for the objects of its class, e.g., round.POSIXt() has no digits argument, and base::round(x = t, digits = 0) fails for date-times. Apply the proposal manually only if the call never receives such objects.
    ## line 7: rownames(x = m, do.NULL = TRUE, prefix = "row")
    ##     REASON: the call is the target of an assignment, like base::rownames(x) <- value, which calls the replacement function (rownames<-), whose arguments differ from the proposed ones. See them with utils::getAnywhere() and apply them manually, if needed.

    ## function (x, m, d) 
    ## {
    ##     a <- base::matrix(data = x, ncol = 2, nrow = , byrow = FALSE, dimnames = NULL)
    ##     b <- base::diag(x)
    ##     e <- stats::t.test(formula = y ~ g, data = d)
    ##     f <- base::round(x)
    ##     base::rownames(m) <- base::paste0("r", 1:2, collapse = NULL, recycle0 = FALSE)
    ## }

Likewise, `graphics::plot(x)` and `graphics::plot(x = x)` are listed
under `CORRECTIONS NOT APPLIED`, because the methods of
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) name their
arguments differently: `plot.formula()` names the first one `formula`,
and an added `y =` would match `ylim` and `ylab` of `plot.table()`.\

### Arguments `table_export`, `out_path`, `df_name` and `overwrite`

``` r

# Write the table into the res.tsv file of the tempo_dir folder,
# and overwrite res.tsv if it already exists in this folder.
saferDev::all_args_here(
    x = FUN2,
    table_export = TRUE,
    fun_export = FALSE,
    out_path = tempo_dir,
    df_name = "res.tsv",
    overwrite = TRUE,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
)
```

    ## 
    ## INSIDE FUN2(), ARGUMENTS ARE MISSING.
    ## RESULT EXPORTED IN:
    ## /tmp/RtmpTr5hQz/dir1386543fd925/res.tsv

    ## function (x, y) 
    ## {
    ##     middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, 
    ##         quote = FALSE, envir = base::parent.frame(n = 1))
    ## }

\

### Argument `fun_export`: return or write the modified function

When arguments are missing,
[`all_args_here()`](../reference/all_args_here.md) returns the modified
function (the missing arguments being inserted as proposed in the
`$STATUS` column of the table), or writes it into a `.R` file named with
the name of the submitted function and `_safer` added at the end:

``` r

# Returns the modified function, or NULL if nothing has to be modified.
FUN2_safer <- saferDev::all_args_here(
    x = FUN2,
    table_export = FALSE,
    fun_export = FALSE,
    out_path = NULL,
    df_name = "res.tsv",
    overwrite = FALSE,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
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

FUN2_safer # modified function.
```

    ## function (x, y) 
    ## {
    ##     middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, 
    ##         quote = FALSE, envir = base::parent.frame(n = 1))
    ## }

``` r

# Write the modified function into the FUN2_safer.R file of the tempo_dir folder.
saferDev::all_args_here(
    x = FUN2,
    table_export = FALSE,
    fun_export = TRUE,
    out_path = tempo_dir,
    df_name = "res.tsv",
    overwrite = TRUE,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
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
    ## /tmp/RtmpTr5hQz/dir1386543fd925/FUN2_safer.R

When `fun_export = TRUE`, the modified function is not returned, but
written into the file. No file is written if nothing is corrected.\

### Argument `safer_check`

``` r

# Safer checks are performed before running the main code.
saferDev::all_args_here(
    x = FUN2,
    table_export = FALSE,
    fun_export = FALSE,
    out_path = NULL,
    df_name = "res.tsv",
    overwrite = FALSE,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
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

# The "NOTGOOD" path does not exist.
saferDev::all_args_here(
    x = FUN2,
    table_export = FALSE,
    fun_export = FALSE,
    out_path = NULL,
    df_name = "res.tsv",
    overwrite = FALSE,
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
saferDev::all_args_here(
    x = FUN2,
    table_export = FALSE,
    fun_export = FALSE,
    out_path = NULL,
    df_name = "res.tsv",
    overwrite = FALSE,
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
    ## ERROR IN saferDev::all_args_here() === TEXT ADDED ===
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### All the arguments

``` r

saferDev::all_args_here(
    x = FUN1, # R function.
    table_export = FALSE, # write the table into a file?
    fun_export = FALSE, # write the modified function into a .R file?
    out_path = NULL, # folder where the files are written (required if table_export or fun_export is TRUE).
    df_name = "res.tsv", # name of the file of the table.
    overwrite = FALSE, # overwrite the files already existing in out_path?
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

    ## ARGUMENTS WITH EMPTY VALUE (DEFAULT VALUE THAT CANNOT BE COPIED, OR NO DEFAULT VALUE):
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
