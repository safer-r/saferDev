# env_check(): more examples

\

``` r

library(saferDev)
```

\

env_check() checks the frames of the call stack (the calling functions),
then the search path. On this page, the code is run by knitr: the frames
of the knitr functions running the code are also checked (the
`<environment>` lines of the outputs). This is why env_check() is called
inside functions below. Copy-paste the code in an R console for shorter
outputs. Call env_check() directly (e.g., `res <- env_check()`), not
inside another call like `print(env_check())` or `cat(env_check())`,
which would check the frame of print() or cat() instead.\

### Argument `pos`

``` r

# pos = 1 (default): checks the environment calling env_check(), here the frame of fun1().
fun1 <- function(){
    mean <- 1
    res <- env_check(pos = 1)
    cat(res)
}
fun1()
```

    ## SOME VARIABLES OF THE CHECKED ENVIRONMENT UNNAMED ARE ALSO PRESENT IN :
    ## package:base: mean
    ## SEARCH PATH CHECKED:
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## .GlobalEnv
    ## package:saferDev
    ## package:stats
    ## package:graphics
    ## package:grDevices
    ## package:utils
    ## package:datasets
    ## package:methods
    ## Autoloads
    ## tools:callr
    ## package:base

``` r

# NULL means that no object name of the checked environment is used in the environments above.
fun2 <- function(){
    tempo_name_used_nowhere_else <- 1
    env_check(pos = 1)
}
fun2()
```

    ## NULL

``` r

# pos = 2: checks the environment one step further up, here the frame of fun3(), which calls fun4().
fun3 <- function(){
    min <- 1
    fun4()
}
fun4 <- function(){
    res <- env_check(pos = 2)
    cat(res)
}
fun3()
```

    ## SOME VARIABLES OF THE CHECKED ENVIRONMENT UNNAMED ARE ALSO PRESENT IN :
    ## package:base: min
    ## SEARCH PATH CHECKED:
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## .GlobalEnv
    ## package:saferDev
    ## package:stats
    ## package:graphics
    ## package:grDevices
    ## package:utils
    ## package:datasets
    ## package:methods
    ## Autoloads
    ## tools:callr
    ## package:base

``` r

# The environments above are those of the call stack: the frame of fun5() is checked,
# although fun6() is not defined inside fun5().
fun5 <- function(){
    max <- 1
    fun6()
}
fun6 <- function(){
    max <- 2
    res <- env_check(pos = 1, name = "fun6")
    cat(res)
}
fun5()
```

    ## SOME VARIABLES OF THE CHECKED ENVIRONMENT fun6 ARE ALSO PRESENT IN :
    ## fun5(): max
    ## package:base: max
    ## SEARCH PATH CHECKED:
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## .GlobalEnv
    ## package:saferDev
    ## package:stats
    ## package:graphics
    ## package:grDevices
    ## package:utils
    ## package:datasets
    ## package:methods
    ## Autoloads
    ## tools:callr
    ## package:base

\

### Argument `name`

``` r

# The name of the checked environment is replaced by "MY_ENV" in the message.
fun8 <- function(){
    mean <- 1
    res <- env_check(pos = 1, name = "MY_ENV")
    cat(res)
}
fun8()
```

    ## SOME VARIABLES OF THE CHECKED ENVIRONMENT MY_ENV ARE ALSO PRESENT IN :
    ## package:base: mean
    ## SEARCH PATH CHECKED:
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## .GlobalEnv
    ## package:saferDev
    ## package:stats
    ## package:graphics
    ## package:grDevices
    ## package:utils
    ## package:datasets
    ## package:methods
    ## Autoloads
    ## tools:callr
    ## package:base

\

### Argument `safer_check`

``` r

# Safer checks are performed before running.
fun9 <- function(){
    tempo_name_used_nowhere_else <- 1
    env_check(safer_check = TRUE)
}
fun9()
```

    ## NULL

\

### Argument `lib_path`

``` r

# The "NOTGOOD" path does not exist.
env_check(lib_path = "NOTGOOD")
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::env_check().
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Argument `error_text`

``` r

# Add a text in error messages returned by env_check().
env_check(
    lib_path = "NOTGOOD", 
    error_text = " === TEXT ADDED ==="
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::env_check() === TEXT ADDED ===
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### All the arguments

``` r

fun10 <- function(){
    mean <- 1
    res <- env_check(
        pos = 1, # environment checked: "pos" steps above env_check() in the call stack.
        name = "MY_ENV", # name of the checked environment (used in the potential output message).
        safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
        lib_path = NULL, # where the required packages are if not in the default directories.
        error_text = "" # add information in error messages returned by the function.
    )
    cat(res)
}
fun10()
```

    ## SOME VARIABLES OF THE CHECKED ENVIRONMENT MY_ENV ARE ALSO PRESENT IN :
    ## package:base: mean
    ## SEARCH PATH CHECKED:
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## .GlobalEnv
    ## package:saferDev
    ## package:stats
    ## package:graphics
    ## package:grDevices
    ## package:utils
    ## package:datasets
    ## package:methods
    ## Autoloads
    ## tools:callr
    ## package:base

\

### Sophisticated examples

``` r

# The name of the function calling env_check() can be retrieved with base::sys.calls():
fun11 <- function(){
    t.test <- 0
    mean <- 5
    name <- base::as.character(x = base::sys.calls()[[base::length(x = base::sys.calls())]])
    saferDev::env_check(pos = 1, name = name)
}
a <- fun11()
base::cat(a)
```

    ## SOME VARIABLES OF THE CHECKED ENVIRONMENT fun11 ARE ALSO PRESENT IN :
    ## doTryCatch(return(expr), name, parentenv, handler): name
    ## tryCatchOne(expr, names, parentenv, handlers[[1]]): name
    ## doTryCatch(return(expr), name, parentenv, handler): name
    ## tryCatchOne(tryCatchList(expr, names[-nh], parentenv, handlers[-nh]), names[nh], parentenv, handlers[[nh]]): name
    ## package:stats: t.test
    ## package:base: mean
    ## SEARCH PATH CHECKED:
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## .GlobalEnv
    ## package:saferDev
    ## package:stats
    ## package:graphics
    ## package:grDevices
    ## package:utils
    ## package:datasets
    ## package:methods
    ## Autoloads
    ## tools:callr
    ## package:base

``` r

# Name of the checked environment according to the test_pos value, in nested functions:
fun12 <- function(){
    min <- "VALUE"
    fun13 <- function(){
        test_pos <- 1 # value 1 checks the fun13 environment, 2 checks the fun12 environment
        range <- "VALUE"
        if(base::length(x = base::sys.calls()) >= test_pos){
            name <- base::as.character(x = base::sys.calls()[[base::length(x = base::sys.calls()) + 1 - test_pos]])
        }else{
            name <- base::search()[test_pos - base::length(x = base::sys.calls())]
        }
        saferDev::env_check(pos = test_pos, name = name)
    }
    fun13()
}
a <- fun12()
base::cat(a)
```

    ## SOME VARIABLES OF THE CHECKED ENVIRONMENT fun13 ARE ALSO PRESENT IN :
    ## doTryCatch(return(expr), name, parentenv, handler): name
    ## tryCatchOne(expr, names, parentenv, handlers[[1]]): name
    ## doTryCatch(return(expr), name, parentenv, handler): name
    ## tryCatchOne(tryCatchList(expr, names[-nh], parentenv, handlers[-nh]), names[nh], parentenv, handlers[[nh]]): name
    ## package:base: range
    ## SEARCH PATH CHECKED:
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## .GlobalEnv
    ## package:saferDev
    ## package:stats
    ## package:graphics
    ## package:grDevices
    ## package:utils
    ## package:datasets
    ## package:methods
    ## Autoloads
    ## tools:callr
    ## package:base
