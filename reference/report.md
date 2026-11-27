# Write an Object or a Text Into a Log File

Writes a character string or a data object into a text file, after its
previous content if required. Convenient to build log files.

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

  Object to write (single value, vector, factor, matrix, table, data
  frame, list, ...). If `NULL`, nothing is done, with no warning.

- output:

  Single character string: name of the output file.

- path:

  Single character string: path of the existing folder of the output
  file. No default value.

- append:

  Single logical value. If `TRUE`, `data` is written after the content
  of the output file (created if it does not exist). If `FALSE`
  (default), the output file must not exist: an existing file is never
  overwritten (error).

- rownames_kept:

  Single logical value. Keep the row names of matrices, tables and data
  frames? Warning: the names above the values of 1D tables are row
  names, removed if `FALSE`.

- vector_cat:

  Single logical value. If `TRUE`, vectors of length \> 1 are written
  with [`cat()`](https://rdrr.io/r/base/cat.html) (values separated by a
  space, names not written), otherwise as printed in the console.
  Vectors of length 1 are always written with
  [`cat()`](https://rdrr.io/r/base/cat.html). Here, vectors are atomic
  vectors with no attribute other than names: factors, dates and lists
  are always written as printed in the console.

- noquote:

  Single logical value. If `TRUE`, character vectors (of length \> 1,
  with `vector_cat = FALSE`), character matrices and character arrays
  are written without quotes. No effect on the other objects.

- sep:

  Single positive integer: number of empty lines written after `data`.

- safer_check:

  Single logical value. If `TRUE` (default), checks before running that
  the required packages and functions are installed and that the
  classical R operators (like `<-`) are not masked by another package or
  by a user-defined object of the global environment. These checks take
  time at each call: set `FALSE` in loops and inside other safer-r
  functions, to avoid repeating them.

- lib_path:

  Vector of character strings: paths of existing extra library folders
  containing the required packages (e.g., packages installed without
  admin rights). They are searched after the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) during the call
  (the current libraries keep priority), and
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) is restored on
  exit. If `NULL` (default), only the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) are used.

- error_text:

  Single character string added to the error messages (e.g.,
  `" INSIDE pkg1::fun1()"`) to name the calling functions. `NULL` is
  converted into `""`.

## Value

No return value (invisible `NULL`), called for its side effect: `data`
is written into the `output` file of the `path` folder.

## See also

[`capture.output`](https://rdrr.io/r/utils/capture.output.html),
[`cat`](https://rdrr.io/r/base/cat.html).

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# See also https://safer-r.github.io/saferDev/articles/report.html
# Write in the results.txt file of a new temporary folder:
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(check = FALSE), fileext = "")
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
saferDev::report(data = "THE FOLLOWING VECTOR IS:", output = "results.txt", path = tempo_dir,
    append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1,
    safer_check = TRUE, lib_path = NULL, error_text = "")
saferDev::report(data = 1:3, output = "results.txt", path = tempo_dir, append = TRUE,
    rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 2, safer_check = TRUE,
    lib_path = NULL, error_text = "")
saferDev::report(data = "THE FOLLOWING DATA FRAME IS:", output = "results.txt",
    path = tempo_dir, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE,
    sep = 1, safer_check = TRUE, lib_path = NULL, error_text = "")
saferDev::report(data = base::data.frame(A = 1:3, B = base::letters[1:3], row.names = NULL,
    check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE),
    output = "results.txt", path = tempo_dir, append = TRUE, rownames_kept = FALSE,
    vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL,
    error_text = "")
base::cat(base::readLines(con = base::file.path(tempo_dir, "results.txt", fsep = "/"), n = -1L,
    ok = TRUE, warn = TRUE, encoding = "unknown", skipNul = FALSE), file = "", sep = "\n",
    fill = FALSE, labels = NULL, append = FALSE)
#> THE FOLLOWING VECTOR IS:
#> 
#> [1] 1 2 3
#> 
#> 
#> THE FOLLOWING DATA FRAME IS:
#> 
#>  A B
#>  1 a
#>  2 b
#>  3 c
#> 
base::unlink(x = tempo_dir, recursive = TRUE, force = FALSE, expand = TRUE) # folder removed
```
