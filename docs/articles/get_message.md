# get_message(): more examples

  

``` r
library(saferDev)
```

  

These examples may not work well here because of a particular
environment. Please, copy-paste in a local environment.  

### Dataset

``` r
char1 <- "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)" # string of a command to evaluate -> warning message
char2 <- "message('ahah')" # idem -> standard message
char3 <- "NOTGOOD" # idem -> error message
char4 <- "mean(2)" # idem -> no message
```

  

### Dataset info

``` r
char1
```

    ## [1] "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)"

``` r
char2
```

    ## [1] "message('ahah')"

``` r
char3
```

    ## [1] "NOTGOOD"

  

### Simple example

``` r
# By default, get_message() returns the first error message of the evaluated string.
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

  

### Argument `kind`

``` r
# To get the last potential warning message.
get_message(data = char1, kind = "warning")
```

    ## NULL

``` r
# See that 2 warning messages are returned when evaluating char1.
 eval(parse(text = char1))
```

    ## 
    ##  Wilcoxon signed rank exact test
    ## 
    ## data:  c(1, 1, 3) and c(1, 2, 4)
    ## V = 0, p-value = 0.5
    ## alternative hypothesis: true location shift is not equal to 0

``` r
# To get the last potential standart message (neither error nor warning).
get_message(data = char2, kind = "message")
```

    ## ahah

    ## NULL

  

### Argument `header`

``` r
# To get the output message without header.
get_message(data = char3, header = FALSE)
```

    ## [1] "Error in base::eval(expr = base::parse(text = data, file = \"\", n = NULL,  : \n  object 'NOTGOOD' not found\n"

``` r
# To get the output message with header (by default).
get_message(data = char3, header = TRUE) 
```

    ## [1] "ERROR MESSAGE REPORTED:\nIn base::eval(expr = base::parse(text = data, file = \"\", n = NULL,  : \n  object 'NOTGOOD' not found\n"

  

### Argument `print_no`

``` r
# Print a message when no "kind" message returned.
get_message(data = char1, print_no = TRUE)
```

    ## [1] "NO ERROR MESSAGE REPORTED"

``` r
# Example with potential returned warning message.
get_message(data = char2, kind = "warning", print_no = TRUE) 
```

    ## ahah

    ## [1] "NO WARNING MESSAGE REPORTED"

  

### Argument `text`

``` r
# String added to the output message, even if nothing reported (when print_no = TRUE).
# Useful when used inside a function to name it.
get_message(data = char3, text = "IN MY EXECUTION") 
```

    ## [1] "ERROR MESSAGE REPORTED IN MY EXECUTION:\nIn base::eval(expr = base::parse(text = data, file = \"\", n = NULL,  : \n  object 'NOTGOOD' not found\n"

  

### Argument `env`

``` r
# Evaluation only in the indicated environment.
get_message(data = char1, env = .GlobalEnv)
```

    ## NULL

``` r
get_message(data = char1, kind = "warning", env = asNamespace("stats"))
```

    ## NULL

  

### Argument `safer_check`

``` r
# Safer checkings are performed before main code running.
get_message(data = char2, safer_check = TRUE)
```

    ## NULL

  

### Argument `lib_path`

``` r
# The "NOTGOOD" path does not exists.
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

  

### All the arguments

``` r
get_message(
    data = char1, # instruction to evaluate.
    kind = "warning", # kind of message to report.
    header = FALSE, # add a header in the returned message?
    print_no = TRUE, # print a message saying that nothing has to be reported?
    text = "IN A", # string added to the output message (even if nothing reported).
    env = NULL, # existing environment where to evaluate the instruction.
    safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
    lib_path = NULL, # where the required packages are if not in the default directories.
    error_text = "" # add information in error messages returned by the function.
)
```

    ## [1] "NO WARNING MESSAGE REPORTED IN A"
