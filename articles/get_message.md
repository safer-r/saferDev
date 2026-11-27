# get_message(): more examples

\

``` r

library(saferDev)
```

\

### Dataset

``` r

char1 <- "log(-1)" # string of a command to evaluate -> warning message
char2 <- "message('ahah')" # idem -> standard message
char3 <- "NOTGOOD" # idem -> error message
```

\

### Dataset info

``` r

char1
```

    ## [1] "log(-1)"

``` r

char2
```

    ## [1] "message('ahah')"

``` r

char3
```

    ## [1] "NOTGOOD"

\

### Simple example

``` r

# By default, get_message() returns the error message of the evaluated string.
# Here no error message
get_message(data = char1)
```

    ## NULL

``` r

# Here no error message
get_message(data = char2)
```

    ## NULL

``` r

# Error message
get_message(data = char3)
```

    ## [1] "ERROR MESSAGE REPORTED:\nIn base::eval(expr = base::parse(text = data, file = \"\", n = NULL,  : \n  object 'NOTGOOD' not found\n"

\

### Argument `kind`

``` r

# To get the last potential warning message.
get_message(data = char1, kind = "warning")
```

    ## [1] "WARNING MESSAGE REPORTED:\nIn log(-1): NaNs produced\n"

``` r

# To get the potential standard messages (neither error nor warning).
get_message(data = char2, kind = "message")
```

    ## [1] "STANDARD MESSAGE REPORTED:\nahah"

\

### Argument `header`

``` r

# To get the output message without header: errors and warnings are then returned as formatted by R (here "Error in ..." instead of "In ...").
get_message(data = char3, header = FALSE)
```

    ## [1] "Error in base::eval(expr = base::parse(text = data, file = \"\", n = NULL,  : \n  object 'NOTGOOD' not found\n"

``` r

# To get the output message with header (by default).
get_message(data = char3, header = TRUE)
```

    ## [1] "ERROR MESSAGE REPORTED:\nIn base::eval(expr = base::parse(text = data, file = \"\", n = NULL,  : \n  object 'NOTGOOD' not found\n"

\

### Argument `print_no`

``` r

# Return a "NO ... REPORTED" string instead of NULL when nothing is caught.
get_message(data = char1, print_no = TRUE)
```

    ## [1] "NO ERROR MESSAGE REPORTED"

``` r

# Here no warning message.
get_message(data = char2, kind = "warning", print_no = TRUE)
```

    ## [1] "NO WARNING MESSAGE REPORTED"

\

### Argument `text`

``` r

# String inserted in the header of the output message (before the colon), and added to the "NO ... REPORTED" strings.
# Useful when used inside a function to name it.
get_message(data = char3, text = "IN MY EXECUTION")
```

    ## [1] "ERROR MESSAGE REPORTED IN MY EXECUTION:\nIn base::eval(expr = base::parse(text = data, file = \"\", n = NULL,  : \n  object 'NOTGOOD' not found\n"

\

### Argument `env`

``` r

# The instruction is evaluated in env: Pillai() is an internal function of the stats package.
saferDev::get_message(
    data = "Pillai(eig = 1, q = 1, df.res = 1)",
    kind = "error",
    header = TRUE,
    print_no = TRUE,
    text = NULL,
    env = base::asNamespace(ns = "stats", base.OK = TRUE),
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
)
```

    ## [1] "NO ERROR MESSAGE REPORTED"

``` r

# In the global environment, Pillai() is not found.
saferDev::get_message(
    data = "Pillai(eig = 1, q = 1, df.res = 1)",
    kind = "error",
    header = TRUE,
    print_no = TRUE,
    text = NULL,
    env = base::globalenv(),
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
)
```

    ## [1] "ERROR MESSAGE REPORTED:\nIn Pillai(eig = 1, q = 1, df.res = 1) : \n  could not find function \"Pillai\"\n"

\

### Argument `safer_check`

``` r

# Safer checks are performed before running the main code.
get_message(data = char2, safer_check = TRUE)
```

    ## NULL

\

### Argument `lib_path`

``` r

# The "NOTGOOD" path does not exist.
get_message(data = char2, lib_path = "NOTGOOD")
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::get_message().
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Argument `error_text`

``` r

# Add a text in error messages returned by get_message().
get_message(
    data = char2,
    lib_path = "NOTGOOD",
    error_text = " === TEXT ADDED ==="
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::get_message() === TEXT ADDED ===
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### All the arguments

``` r

get_message(
    data = char1, # instruction to evaluate.
    kind = "warning", # kind of message to report.
    header = FALSE, # add a header in the returned message?
    print_no = TRUE, # return a "NO ... REPORTED" string instead of NULL when nothing is caught?
    text = "IN A", # string added to the header (not used here, because header = FALSE) and to the "NO ... REPORTED" strings.
    env = NULL, # existing environment where to evaluate the instruction.
    safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
    lib_path = NULL, # where the required packages are if not in the default directories.
    error_text = "" # add information in error messages returned by the function.
)
```

    ## [1] "simpleWarning in log(-1): NaNs produced\n"

\

### Printed outputs

``` r

# The instruction is evaluated once. Its printed outputs are not caught: use utils::capture.output() to get them.
printed <- utils::capture.output(msg <- get_message(data = "print(1) ; log(-1)", kind = "warning"))
printed
```

    ## [1] "[1] 1"

``` r

msg
```

    ## [1] "WARNING MESSAGE REPORTED:\nIn log(-1): NaNs produced\n"
