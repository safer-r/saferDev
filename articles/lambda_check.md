# lambda_check(): more examples

\

``` r

library(saferDev)
```

\

### Datasets

``` r

# Warning: the \(x) lambda writing is only kept in the code of a function carrying its source references (srcref). The functions are therefore built with base::parse(keep.source = TRUE) followed by base::eval(), whatever the value of base::getOption("keep.source").
# Function with all the anonymous functions written with the standard function() writing.
fun1 <- eval(parse(text = "function(x){ base::sapply(X = 1:3, FUN = function(y){ y * 2 }) }", keep.source = TRUE))
# Function with some anonymous functions written with the lambda writing \\().
fun2 <- eval(parse(text = "function(x){ base::sapply(X = 1:3, FUN = \\(y){ y * 2 }) ; base::lapply(X = x, FUN = \\(z) z + 1) }", keep.source = TRUE))
```

\

### Datasets info

``` r

fun1 # Function.
```

    ## function(x){ base::sapply(X = 1:3, FUN = function(y){ y * 2 }) }

``` r

fun2 # Function.
```

    ## function(x){ base::sapply(X = 1:3, FUN = \(y){ y * 2 }) ; base::lapply(X = x, FUN = \(z) z + 1) }

\

### Argument `x`

``` r

# Test if all the anonymous functions of fun1 are written with the standard function() writing.
# Here everything is fine.
lambda_check(x = fun1)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::lambda_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

``` r

# Here some anonymous functions are written with the lambda writing.
lambda_check(x = fun2)
```

    ## 
    ## 
    ## 
    ## INSIDE fun2(), SOME LAMBDA FUNCTIONS ARE NOT WRITTEN function():
    ## 
    ## LINE SYMBOL      CODE
    ## 1    \(y)        function(x){ base::sapply(X = 1:3, FUN = \(y){ y * 2 }) ; base::lapply(X = x, FUN = \(z) z + 1) }
    ## 1    \(z)        function(x){ base::sapply(X = 1:3, FUN = \(y){ y * 2 }) ; base::lapply(X = x, FUN = \(z) z + 1) }

    ## function (x) 
    ## {
    ##     base::sapply(X = 1:3, FUN = function(y) {
    ##         y * 2
    ##     })
    ##     base::lapply(X = x, FUN = function(z) z + 1)
    ## }

\

### Argument `safer_check`

``` r

# Safer checkings are performed before main code running.
lambda_check(
    x = fun1, 
    safer_check = TRUE
)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::lambda_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

\

### Argument `lib_path`

``` r

# The "NOTGOOD" path does not exists.
lambda_check(
    x = fun1,  
    lib_path = "NOTGOOD"
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::lambda_check().
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Argument `error_text`

``` r

# Add a text in error messages returned by lambda_check().
lambda_check(
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
    ## ERROR IN saferDev::lambda_check() === TEXT ADDED ===
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Arguments `export` and `out_path`: return or export the corrected function

``` r

# The corrected function is returned (NULL if nothing has to be corrected).
fun2_safer <- lambda_check(x = fun2)
```

    ## 
    ## 
    ## 
    ## INSIDE fun2(), SOME LAMBDA FUNCTIONS ARE NOT WRITTEN function():
    ## 
    ## LINE SYMBOL      CODE
    ## 1    \(y)        function(x){ base::sapply(X = 1:3, FUN = \(y){ y * 2 }) ; base::lapply(X = x, FUN = \(z) z + 1) }
    ## 1    \(z)        function(x){ base::sapply(X = 1:3, FUN = \(y){ y * 2 }) ; base::lapply(X = x, FUN = \(z) z + 1) }

``` r

fun2_safer # \\() replaced by function().
```

    ## function (x) 
    ## {
    ##     base::sapply(X = 1:3, FUN = function(y) {
    ##         y * 2
    ##     })
    ##     base::lapply(X = x, FUN = function(z) z + 1)
    ## }

``` r

# The corrected function behaves like the submitted one.
fun2(1)
```

    ## [[1]]
    ## [1] 2

``` r

fun2_safer(1)
```

    ## [[1]]
    ## [1] 2

``` r

# And it passes the check.
lambda_check(x = fun2_safer)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::lambda_check().
    ## INSIDE fun2_safer(), EVERYTHING SEEMS CLEAN.

``` r

# Export the corrected function into the fun2_safer.R file, here in a new temporary folder.
# out_path = "." (default) is the R working directory.
# An existing file is never overwritten (an error is returned instead).
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(), fileext = "")
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
lambda_check(
    x = fun2,
    export = TRUE,
    out_path = tempo_dir
)
```

    ## 
    ## 
    ## 
    ## INSIDE fun2(), SOME LAMBDA FUNCTIONS ARE NOT WRITTEN function():
    ## 
    ## LINE SYMBOL      CODE
    ## 1    \(y)        function(x){ base::sapply(X = 1:3, FUN = \(y){ y * 2 }) ; base::lapply(X = x, FUN = \(z) z + 1) }
    ## 1    \(z)        function(x){ base::sapply(X = 1:3, FUN = \(y){ y * 2 }) ; base::lapply(X = x, FUN = \(z) z + 1) }

    ## MODIFIED CODE EXPORTED IN:
    ## /tmp/RtmpYpohRB/dir1cd4f489217/fun2_safer.R

``` r

# base::source() of this file recreates the corrected function, named fun2_safer.
base::readLines(con = base::file.path(tempo_dir, "fun2_safer.R", fsep = "/"))
```

    ## [1] "fun2_safer <- function(x){ base::sapply(X = 1:3, FUN = function(y){ y * 2 }) ; base::lapply(X = x, FUN = function(z) z + 1) }"

\

### All the arguments

``` r

lambda_check(
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
    ## AFTER RUNNING saferDev::lambda_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

\

### Sophisticated example

``` r

# A lambda written in a string or in a comment is ignored.
fun3 <- eval(parse(text = "function(x){ if(x > 1){ y <- \"the \\(x) writing in a string\" } ; x }", keep.source = TRUE))
```

    ## Error:
    ## ! '\(' is an unrecognized escape in character string (<text>:1:36)

``` r

lambda_check(x = fun3)
```

    ## Error:
    ## ! object 'fun3' not found

``` r

# The backquoted `\\` call (the deprecated function() writing) is not a lambda writing: ignored.
fun4 <- eval(parse(text = "function(x){ base::`\\\\`(x) }", keep.source = TRUE))
lambda_check(x = fun4)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::lambda_check().
    ## INSIDE fun4(), EVERYTHING SEEMS CLEAN.

\
