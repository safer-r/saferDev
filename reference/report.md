# Report

Print a character string or a data object into a same output file.
Convenient for log file generation.

## Usage

``` r
report(
  data,
  output = "log.txt",
  path,
  append = FALSE,
  rownames_kept = FALSE,
  vector_cat = FALSE,
  noquote = TRUE,
  sep = 2,
  safer_check = TRUE,
  lib_path = NULL,
  error_text = ""
)
```

## Arguments

- data:

  Object to print in the output file. If `NULL`, nothing is done, with
  no warning.

- output:

  Single character string. Name of the output file.

- path:

  Single character string indicating the path where to write the output
  file.

- append:

  Single logical value. If the output file already exists and `append`
  is `FALSE`, an error message is returned (no overwrite of existing
  file). Otherwise, the printing is appended (and the output file is
  created if it does not exist yet).

- rownames_kept:

  Single logical value. Defines whether row names have to be removed or
  not in 2D objects. Warning: in 1D tables, names over the values are
  taken as row names, and are thus removed if `rownames_kept` is
  `FALSE`.

- vector_cat:

  Single logical value. If `TRUE`, print a vector of length \> 1 using
  [`cat()`](https://rdrr.io/r/base/cat.html) instead of
  [`capture.output()`](https://rdrr.io/r/utils/capture.output.html).
  Otherwise (default `FALSE`) print a vector of length \> 1 using
  [`capture.output()`](https://rdrr.io/r/utils/capture.output.html).
  Names of values are not printed when `TRUE`.

- noquote:

  Single logical value. If `TRUE` no quote are added to the returned
  character strings.

- sep:

  Single non null and positive integer representing the number of empty
  lines added in the output file after the printed data.

- safer_check:

  Single logical value. Perform some "safer" checks? If `TRUE`,
  checkings are performed before main code running (see the [safer-r
  project](https://github.com/safer-r)): 1) correct `lib_path` argument
  value 2) required functions and related packages effectively present
  in local R libraries and 3) R classical operators (like `"<-"`) not
  overwritten by another package because of the R scope. Must be set to
  `FALSE` if this function is used inside another "safer" function to
  avoid pointless multiple checkings.

- lib_path:

  Vector of characters specifying the absolute pathways of the
  directories containing the required packages for the function, if not
  in the default directories. Useful when R packages are not installed
  in the default directories because of lack of admin rights. More
  precisely, `lib_path` is passed through the `new` argument of
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) so that the new
  library paths are `unique(c(new, .Library.site, .Library))`. Warning:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) is restored to
  the initial paths, after function execution. Ignored if `NULL`
  (default) or if the `safer_check` argument is `FALSE`: only the
  pathways specified by the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) are used for
  package calling.

- error_text:

  Single character string used to add information in error messages
  returned by the function, notably if the function is inside other
  functions, which is practical for debugging. Example:
  `error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."`.
  If `NULL`, converted into `""`.

## Value

Nothing (implementation of an existing file) or a text file.

## See also

[`capture.output`](https://rdrr.io/r/utils/capture.output.html).

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferDev/articles/report.html
# Write in the results.txt file of a new temporary folder:
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(), fileext = "")
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
saferDev::report(data = "THE FOLLOWING VECTOR IS:\n", output = "results.txt", path = tempo_dir,
    append = TRUE, sep = 1)
saferDev::report(data = 1:3, output = "results.txt", path = tempo_dir, append = TRUE,
    rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 2)
saferDev::report(data = "THE FOLLOWING DATA FRAME IS:\n", output = "results.txt",
    path = tempo_dir, append = TRUE, sep = 1)
saferDev::report(data = base::data.frame(A = 1:3, B = letters[1:3]), output = "results.txt",
    path = tempo_dir, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE,
    noquote = FALSE, sep = 1)
base::cat(base::readLines(con = base::file.path(tempo_dir, "results.txt")), sep = "\n")
#> THE FOLLOWING VECTOR IS:
#> 
#> 
#> 
#> [1] 1 2 3
#> 
#> 
#> THE FOLLOWING DATA FRAME IS:
#> 
#> 
#> 
#>    A B
#>    1 a
#>    2 b
#>    3 c
#> 
```
