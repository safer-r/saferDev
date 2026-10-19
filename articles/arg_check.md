# arg_check(): more examples

\

``` r

library(saferDev)
```

\

### Dataset

``` r

vec1 <- -1:3 # vector of integers.
vec2 <- 1:3 / 3 # vector of positive proportions.
vec3 <- c(1, 2, 3) # vector of positive integers but stored as "double".
vec4 <- "pearson" # vector of characters.
vec5 <- c("a", "b","a", "b") # vector of characters.
mat1 <- matrix(vec1) # matrix of integers.
mat2 <- matrix(c(1:3 / 3, NA)) # matrix of proportions with NA.
```

\

### Dataset info

``` r

vec1 # vector of integers.
```

    ## [1] -1  0  1  2  3

``` r

vec2 # vector of proportions.
```

    ## [1] 0.3333333 0.6666667 1.0000000

``` r

vec3 # vector of integers but stored as "double".
```

    ## [1] 1 2 3

``` r

vec4 # vector of characters.
```

    ## [1] "pearson"

``` r

vec5 # vector of characters.
```

    ## [1] "a" "b" "a" "b"

``` r

mat1 # matrix of integers.
```

    ##      [,1]
    ## [1,]   -1
    ## [2,]    0
    ## [3,]    1
    ## [4,]    2
    ## [5,]    3

``` r

mat2 # matrix of proportions with NA.
```

    ##           [,1]
    ## [1,] 0.3333333
    ## [2,] 0.6666667
    ## [3,] 1.0000000
    ## [4,]        NA

\

### Simple examples

``` r

# Check that vec1 is type integer.
arg_check(data = vec1, typeof = "integer")
```

    ## $problem
    ## [1] FALSE
    ## 
    ## $text
    ## [1] "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT."
    ## 
    ## $object_name
    ## [1] "vec1"

``` r

# Check that vec1 is a vector of type double -> error because vec1 is not type "double", 
# but type "integer".
arg_check(data = vec1, class = "integer", typeof = "double")
```

    ## $problem
    ## [1] TRUE
    ## 
    ## $text
    ## [1] "ERROR\n\nTHE vec1 ARGUMENT MUST BE TYPEOF double"
    ## 
    ## $object_name
    ## [1] "vec1"

``` r

# Check that vec1 is an integer vector of length 3 without negative values and 
# without NA -> error because of length 5 and negative values inside vec1.
arg_check(
    data = vec1, 
    class = "integer", 
    typeof = "integer", 
    length = 3, 
    neg_values = FALSE, 
    na_contain = FALSE
)
```

    ## $problem
    ## [1] TRUE
    ## 
    ## $text
    ## [1] "ERROR\n\nTHE vec1 ARGUMENT MUST BE LENGTH 3 AND THE vec1 ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES."
    ## 
    ## $object_name
    ## [1] "vec1"

``` r

# No result displayed because the output list is assigned into res
# (see below the print argument).
res <- arg_check(data = vec1, typeof = "integer")


# With data = NULL, the function systematically report a checking problem.
arg_check(data = NULL, typeof = "integer")
```

    ## $problem
    ## [1] TRUE
    ## 
    ## $text
    ## [1] "ERROR\n\nTHE NULL ARGUMENT MUST BE TYPEOF integer"
    ## 
    ## $object_name
    ## [1] "NULL"

\

### Arguments `class`, `typeof`, `mode` and `length`

These arguments accept any character strings (single or vector of
character strings), tested exactly (same values, in the same order)
against `class(data)`, `typeof(data)` and `mode(data)`, respectively.
Since R 4.0.0, matrices have class `c(“matrix”, “array”)`, and ordered
factors have class `c(“ordered”, “factor”)`. See the help of
`arg_check`.

``` r

# Example.
arg_check(
    data = vec1, 
    class = "integer", 
    typeof = "integer", 
    mode = "numeric", 
    length = 5, 
)
```

    ## $problem
    ## [1] FALSE
    ## 
    ## $text
    ## [1] "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT."
    ## 
    ## $object_name
    ## [1] "vec1"

``` r

# Warning: the function does not check for inconsistencies between arguments.
# It just checks if everything is ok between arguments values and the tested object.
# Here the inconsistancy between typeof and mode is not signaled.
arg_check(
    data = vec1, 
    typeof = "integer", 
    mode = "character"
)
```

    ## $problem
    ## [1] TRUE
    ## 
    ## $text
    ## [1] "ERROR\n\nTHE vec1 ARGUMENT MUST BE MODE character"
    ## 
    ## $object_name
    ## [1] "vec1"

``` r

# Bad mode argument value.
arg_check(
    data = vec1, 
    mode = "integer",
)
```

    ## $problem
    ## [1] TRUE
    ## 
    ## $text
    ## [1] "ERROR\n\nTHE vec1 ARGUMENT MUST BE MODE integer"
    ## 
    ## $object_name
    ## [1] "vec1"

``` r

# Vector class values are also possible, tested exactly (same values, same order).
# Here, no problem because class(mat1) is c("matrix", "array"), matching exactly the class argument.
arg_check(
    data = mat1, 
    class = c("matrix", "array"), 
    typeof = "integer"
)
```

    ## $problem
    ## [1] FALSE
    ## 
    ## $text
    ## [1] "NO PROBLEM DETECTED FOR THE mat1 ARGUMENT."
    ## 
    ## $object_name
    ## [1] "mat1"

``` r

# Error here, because the written values must be identical, element per element, in the same order.
arg_check(
    data = mat1, 
    class = c("array", "matrix"), 
    typeof = "integer"
)
```

    ## $problem
    ## [1] TRUE
    ## 
    ## $text
    ## [1] "ERROR\n\nTHE mat1 ARGUMENT MUST BE CLASS array, matrix"
    ## 
    ## $object_name
    ## [1] "mat1"

\

### Argument `prop`

``` r

# Check for values between 0 and 1 only.
arg_check(
    data = vec2, 
    prop = TRUE 
)
```

    ## $problem
    ## [1] FALSE
    ## 
    ## $text
    ## [1] "NO PROBLEM DETECTED FOR THE vec2 ARGUMENT."
    ## 
    ## $object_name
    ## [1] "vec2"

\

### Argument `double_as_integer_allowed`

``` r

# Check for integers, even if numbers are stored as double.
# With double_as_integer_allowed = TRUE # with TRUE, integers stored as double are accepted.
arg_check(
    data = vec3, 
    typeof = "integer",
    double_as_integer_allowed = TRUE
)
```

    ## $problem
    ## [1] FALSE
    ## 
    ## $text
    ## [1] "NO PROBLEM DETECTED FOR THE vec3 ARGUMENT."
    ## 
    ## $object_name
    ## [1] "vec3"

\

### Argument `options`

``` r

# Check for the content among values. Here no error detected because vec4 contains "pearson".
arg_check(
    data = vec4, 
    options = c("pearson", "spearman", "kendall")
)
```

    ## $problem
    ## [1] FALSE
    ## 
    ## $text
    ## [1] "NO PROBLEM DETECTED FOR THE vec4 ARGUMENT."
    ## 
    ## $object_name
    ## [1] "vec4"

\

### Argument `all_options_in_data`

``` r

# Check for the content among values.
# Here no error detected because vec5 indeed contains at least one copy of "a" and "b" and nothing else.
arg_check(
    data = vec5,
    options = c("a", "b"), 
    all_options_in_data = TRUE
)
```

    ## $problem
    ## [1] FALSE
    ## 
    ## $text
    ## [1] "NO PROBLEM DETECTED FOR THE vec5 ARGUMENT."
    ## 
    ## $object_name
    ## [1] "vec5"

``` r

# Here no error detected because vec5 indeed contains at least one copy of "a" and "b", among other values.
arg_check(
    data = vec5,
    options = c("a", "b", "c"), 
    all_options_in_data = FALSE
)
```

    ## $problem
    ## [1] FALSE
    ## 
    ## $text
    ## [1] "NO PROBLEM DETECTED FOR THE vec5 ARGUMENT."
    ## 
    ## $object_name
    ## [1] "vec5"

``` r

# Here an error is detected because vec5 must contain "a", "b" and "c", at least once each and nothing else.
arg_check(
    data = vec5,
    options = c("a", "b", "c"), 
    all_options_in_data = TRUE
)
```

    ## $problem
    ## [1] TRUE
    ## 
    ## $text
    ## [1] "ERROR\n\nTHE vec5 ARGUMENT MUST BE MADE OF ALL THESE OPTIONS:\na\nb\nc\nTHE MISSING ELEMENT IN vec5 IS:\nc"
    ## 
    ## $object_name
    ## [1] "vec5"

\

### Argument `na_contain`

``` r

# Check for the absence of NA.
# Warning: na_contain = TRUE means "no check". Thus, NA can be present or not. 
arg_check(
    data = mat2, 
    class = c("matrix", "array"), 
    prop = TRUE,
    na_contain = FALSE 
)
```

    ## $problem
    ## [1] TRUE
    ## 
    ## $text
    ## [1] "ERROR\n\nTHE mat2 ARGUMENT CONTAINS NA WHILE NOT AUTHORIZED."
    ## 
    ## $object_name
    ## [1] "mat2"

\

### Argument `neg_values`

``` r

# Check for the absence of negative values.
# Warning: neg_values = TRUE means "no check". Thus, negative numbers can be present or not. 
arg_check(
    data = vec1, 
    typeof = "integer",
    neg_values = FALSE
)
```

    ## $problem
    ## [1] TRUE
    ## 
    ## $text
    ## [1] "ERROR\n\nTHE vec1 ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES."
    ## 
    ## $object_name
    ## [1] "vec1"

``` r

# Warning: neg_values = FALSE cannot be used if typeof and mode are NULL. 
arg_check(
    data = mat1, 
    class = c("matrix", "array"),
    neg_values = FALSE
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::arg_check().
    ## 
    ## THE neg_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF typeof AND mode ARGUMENTS ARE NULL.
    ## 
    ## ================

\

### Argument `inf_values`

``` r

# Check for the absence of Inf/-Inf values.
# Warning: inf_values = TRUE means "no check". Thus, infinite numbers can be present or not.
arg_check(
    data = vec2, 
    typeof = "double",
    inf_values = FALSE
)
```

    ## $problem
    ## [1] FALSE
    ## 
    ## $text
    ## [1] "NO PROBLEM DETECTED FOR THE vec2 ARGUMENT."
    ## 
    ## $object_name
    ## [1] "vec2"

``` r

# Warning: inf_values = FALSE cannot be used with typeof = "integer". 
arg_check(
    data = mat1, 
    typeof = "integer",
    inf_values = FALSE
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::arg_check().
    ## 
    ## THE typeof ARGUMENT MUST BE A SINGLE "double" VALUE IF ABSENCE OF INFINITE VALUES IS CONTROLED BY SWITCHING THE inf_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE.
    ## "integer" IS NOT ALLOWED BECAUSE OBJECTS WITH INFINITE VALUES ARE NOT INTEGERS.
    ## 
    ## ================

``` r

# Warning: inf_values = FALSE cannot be used if typeof and mode are NULL. 
arg_check(
    data = mat1, 
    class = c("matrix", "array"),
    inf_values = FALSE
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::arg_check().
    ## 
    ## THE inf_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF typeof AND mode ARGUMENTS ARE NULL.
    ## 
    ## ================

\

### Argument `print`

``` r

# No error message printed because print is FALSE.
res <- arg_check(
    data = mat1, 
    class = "data.frame",
    print = FALSE
)

# Error message printed. Warning: does not trigger "exit". It is just a message.
res <- arg_check(
    data = mat1, 
    class = "data.frame",
    print = TRUE
)
```

    ## 
    ## 
    ## ================
    ## 
    ## ERROR
    ## 
    ## THE mat1 ARGUMENT MUST BE CLASS data.frame
    ## 
    ## ================

``` r

# Even if print is TRUE, no error message printed because no error.
res <- arg_check(
    data = mat1, 
    class = c("matrix", "array"),
    print = TRUE
)
```

\

### Argument `data_name`

``` r

# Modification of the name of the tested object in the message
arg_check(
    data = vec1, 
    typeof = "integer", 
    data_name = "OBSERVATION_1"
)
```

    ## $problem
    ## [1] FALSE
    ## 
    ## $text
    ## [1] "NO PROBLEM DETECTED FOR THE OBSERVATION_1 ARGUMENT."
    ## 
    ## $object_name
    ## [1] "OBSERVATION_1"

``` r

# This argument is interesting when data is not an object.
x <- 1
arg_check(
    data = get("x"), 
    typeof = "integer", 
    data_name = "x"
)
```

    ## $problem
    ## [1] TRUE
    ## 
    ## $text
    ## [1] "ERROR\n\nTHE x ARGUMENT MUST BE TYPEOF integer"
    ## 
    ## $object_name
    ## [1] "x"

``` r

# If "data_name" is not used.
arg_check(
    data = get("x"), 
    typeof = "integer", 
    data_name = NULL
)
```

    ## $problem
    ## [1] TRUE
    ## 
    ## $text
    ## [1] "ERROR\n\nTHE get(\"x\") ARGUMENT MUST BE TYPEOF integer"
    ## 
    ## $object_name
    ## [1] "get(\"x\")"

\

### Arguments `data_arg`

``` r

# Message with "argument". Useful when arg_check() is used inside a function to test its arguments.
arg_check(
    data = vec2, 
    typeof = "integer", 
    data_arg = TRUE
)
```

    ## $problem
    ## [1] TRUE
    ## 
    ## $text
    ## [1] "ERROR\n\nTHE vec2 ARGUMENT MUST BE TYPEOF integer"
    ## 
    ## $object_name
    ## [1] "vec2"

``` r

# Message with "object".
arg_check(
    data = vec2, 
    typeof = "integer", 
    data_arg = FALSE
)
```

    ## $problem
    ## [1] TRUE
    ## 
    ## $text
    ## [1] "ERROR\n\nTHE vec2 OBJECT MUST BE TYPEOF integer"
    ## 
    ## $object_name
    ## [1] "vec2"

\

### Argument `safer_check`

``` r

# Safer checkings performed before main code running.
arg_check(
    data = mat1, 
    class = c("matrix", "array"), 
    safer_check = TRUE
)
```

    ## $problem
    ## [1] FALSE
    ## 
    ## $text
    ## [1] "NO PROBLEM DETECTED FOR THE mat1 ARGUMENT."
    ## 
    ## $object_name
    ## [1] "mat1"

\

### Argument `lib_path`

``` r

# The "NOTGOOD" path does not exists.
arg_check(
    data = mat1, 
    class = c("matrix", "array"), 
    lib_path = "NOTGOOD"
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::arg_check().
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Argument `error_text`

``` r

# Add a text in error messages returned by all_args_here().
arg_check(
    data = mat1, 
    class = c("matrix", "array"), 
    lib_path = "NOTGOOD",
    error_text = " === TEXT ADDED ==="
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::arg_check() === TEXT ADDED ===
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### All the arguments

``` r

arg_check(
    data = vec1, # object to check.
    class = NULL, # check the expected class (tested exactly, same values, same order).
    typeof = "integer", # check the expected type.
    mode = NULL,  # check the expected mode.
    length = NULL,  # check the expected length.
    prop = FALSE,  # check the expected proportion.
    double_as_integer_allowed = FALSE,  # can integers be typeof = "double" or class = "numeric"?
    options = NULL,  # check all the possible option values.
    all_options_in_data = FALSE,  # all the options argument values must be present at least once in the data argument, and nothing else?
    na_contain = FALSE,  # can data contain NA? Only tested if FALSE.
    neg_values = TRUE,  # can data contain negative values? Only tested if FALSE.
    inf_values = TRUE,  # can data contain infinite values? Only tested if FALSE.
    print = FALSE,  # print a message if the $problem output is TRUE? 
    data_name = NULL,  # name of the object to test.
    data_arg = TRUE,  # If TRUE, "ARGUMENT" is written in output messages, otherwise "OBJECT".
    safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
    lib_path = NULL, # where the required packages are if not in the default directories.
    error_text = "" # add information in error messages returned by the function.
)
```

    ## $problem
    ## [1] FALSE
    ## 
    ## $text
    ## [1] "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT."
    ## 
    ## $object_name
    ## [1] "vec1"
