# report(): more examples

\

``` r

library(saferDev)
```

\

### Dataset

``` r

string1 <- "THIS IS A LOG" # string.
vec1 <- 1:6 # vector of integers.
vec2 <- letters[1:9] # vector of characters.
mat1 <- matrix(vec1, nrow = 2, ncol = 3, byrow = TRUE) # 2x3 matrix of integers.
dimnames(mat1) <- list(c("Row_1","Row_2"), c("C1", "C2", "C3")) # mat1 matrix with column and row names.
mat2 <- mat1
mode(mat2) <- "character" # as mat1 but made of characters.
t1 <- table(mat1) # 1D table.
vec3 <- as.vector(t1) # vector of intergers
names(vec3) <- letters[1:6] # vector with names.
t2 <- table(vec2, vec2) # 2D table.
l1 <- list(vec1, vec2) # list.
```

\

### Dataset info

``` r

string1 # string.
```

    ## [1] "THIS IS A LOG"

``` r

vec1 # vector of integers.
```

    ## [1] 1 2 3 4 5 6

``` r

vec2 # vector of characters.
```

    ## [1] "a" "b" "c" "d" "e" "f" "g" "h" "i"

``` r

vec3 # vector of integers with names. No more [1] in front of values.
```

    ## a b c d e f 
    ## 1 1 1 1 1 1

``` r

mat1 # 2x3 matrix of integers with column and row names.
```

    ##       C1 C2 C3
    ## Row_1  1  2  3
    ## Row_2  4  5  6

``` r

mat2 # as mat1 but made of characters.
```

    ##       C1  C2  C3 
    ## Row_1 "1" "2" "3"
    ## Row_2 "4" "5" "6"

``` r

t1 # 1D table.
```

    ## mat1
    ## 1 2 3 4 5 6 
    ## 1 1 1 1 1 1

``` r

t2 # 2D table.
```

    ##     vec2
    ## vec2 a b c d e f g h i
    ##    a 1 0 0 0 0 0 0 0 0
    ##    b 0 1 0 0 0 0 0 0 0
    ##    c 0 0 1 0 0 0 0 0 0
    ##    d 0 0 0 1 0 0 0 0 0
    ##    e 0 0 0 0 1 0 0 0 0
    ##    f 0 0 0 0 0 1 0 0 0
    ##    g 0 0 0 0 0 0 1 0 0
    ##    h 0 0 0 0 0 0 0 1 0
    ##    i 0 0 0 0 0 0 0 0 1

``` r

l1 # list.
```

    ## [[1]]
    ## [1] 1 2 3 4 5 6
    ## 
    ## [[2]]
    ## [1] "a" "b" "c" "d" "e" "f" "g" "h" "i"

\

### Simple examples

``` r

# Print the value of "data" into the log.txt output file in the indicated path.
report(data = string1, append = TRUE, path = ".")

# Idem.
report(data = vec1, append = TRUE, path = ".")

# Idem.
report(data = vec2, append = TRUE, path = ".")

# Idem.
report(data = vec3, append = TRUE, path = ".")

# Idem.
report(data = mat1, append = TRUE, path = ".")

# Idem. Of note, we cannot see in the log.txt file that mat2 is mode character (see noquote below).
report(data = mat2, append = TRUE, path = ".")

# Idem.
report(data = t1,, append = TRUE, path = ".")

# Idem.
report(data = t2, append = TRUE, path = ".")

# Idem.
report(data = l1, append = TRUE, path = ".")
```

\

### Argument `output`

``` r

# Print the value of vec1 into the test.txt file in the indicated path.
report(data = vec1, output = "test.txt", append = TRUE, path = ".")
```

\

### Argument `append`

``` r

# Error returned because the test.txt file already exists.
report(data = vec2, append = FALSE, output = "test.txt", path = ".")
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::report().
    ## 
    ## FILE DEFINED BY THE path AND output ARGUMENTS
    ## ./test.txt
    ## ALREADY EXISTS AND CANNOT BE OVERWRITTEN.
    ## PLEASE:
    ## REMOVE THE FILE
    ## OR CHANGE THE NAME OF THE output ARGUMENT
    ## OR SET THE append ARGUMENT TO TRUE TO APPEND IN THE EXISTING FILE.
    ## 
    ## 
    ## ================

\

### Argument `rownames_kept`

``` r

 # Keep the row names when print the matrix mat1 in log.txt.
report(
  data = mat1, 
  rownames_kept = TRUE, 
  append = TRUE, 
  path = "."
)

# Remove the row names.
report(
  data = mat1, 
  rownames_kept = FALSE, 
  append = TRUE, 
  path = "."
)

# Keep the row names when printing the table t1 in log.txt.
report(
  data = t1, 
  rownames_kept = TRUE, 
  append = TRUE, 
  path = "."
) 

# Remove the row names.
report(
  data = t1, 
  rownames_kept = FALSE, 
  append = TRUE, 
  path = "."
)

# Keep the row names when print the table t2 in log.txt.
report(
  data = t2, 
  rownames_kept = TRUE, 
  append = TRUE, 
  path = "."
)

# Remove the row names.
report(
  data = t2, 
  rownames_kept = FALSE, 
  append = TRUE, 
  path = "."
) 
```

\

### Argument `vector_cat`

``` r

# Print a vector of length > 1 using cat() instead of capture.output().
# Names are lost.
report(
  data = vec3, 
  vector_cat = TRUE, 
  append = TRUE, 
  path = "."
)

# Names are not lost.
report(
  data = vec3, 
  vector_cat = FALSE, 
  append = TRUE, 
  path = "."
)
```

\

### Argument `noquote`

``` r

# Quotes are kept for the characters.
report(
  data = mat2, 
  noquote = FALSE, 
  append = TRUE, 
  path = "."
)
```

\

### Argument `sep`

``` r

# 4 empty lines added after printed data.
report(
  data = vec1, 
  sep = 4, 
  append = TRUE, 
  path = "."
)

#Error returned because at least 1 empty lines between printings.
report(
  data = vec1, 
  sep = 0, 
  append = TRUE, 
  path = "."
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::report().
    ## 
    ## sep ARGUMENT CANNOT BE EQUAL TO ZERO.
    ## 
    ## ================

\

### Argument `safer_check`

``` r

# Safer checkings are performed before main code running 
report(
    data = vec1, 
    safer_check = TRUE, 
    append = TRUE, 
    path = "."
)
```

\

### Argument `lib_path`

``` r

# The "NOTGOOD" path does not exists.
report(
    data = vec1, 
    append = TRUE, 
    path = ".", 
    lib_path = "NOTGOOD"
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::report().
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Argument `error_text`

``` r

# Add a text in error messages returned by report().
report(
    data = vec1, 
    append = TRUE, 
    path = ".", 
    lib_path = "NOTGOOD", 
    error_text = " === TEXT ADDED ==="
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::report() === TEXT ADDED ===
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### All the arguments with their descriptions

``` r

# matrix mat1 will be print in the file "text2.txt" in the path, with quote and row names being kept, and 4 lines after printing data
report(
    data = mat2, # object to print.
    output = "test2.txt", # name of the output file.
    path = ".", # path of the output file.
    append = TRUE, # append in the output file?
    rownames_kept = TRUE, # keep row names?
    vector_cat = TRUE, # use cat() instead of capture.output() to print vectors?
    noquote = FALSE, # print quotes for characters?
    sep = 4, # empty lines after the printed data.
    safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
    lib_path = NULL, # where the required packages are if not in the default directories.
    error_text = "" # add information in error messages returned by the function.
)
```
