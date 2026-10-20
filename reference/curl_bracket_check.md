# Curl Brackets Check

Verify that all the bodies of the `if`, `else`, `for`, `while`, `repeat`
and `function` (including lambda `\(x)`) constructs of a function are
delimited by curly brackets [`{ }`](https://rdrr.io/r/base/Paren.html).
For instance: `if(x > 1){ base::return(x) }` and not
`if(x > 1) base::return(x)`. The safer-r rule requires curly brackets
for every body, even single-statement ones.

## Usage

``` r
curl_bracket_check(
  x,
  export = FALSE,
  out_path = ".",
  safer_check = TRUE,
  lib_path = NULL,
  error_text = ""
)
```

## Arguments

- x:

  a function name, written without quotes and brackets.

- export:

  Single logical value. Export the modified function into a `.R` file
  (see the `out_path` argument)? If `TRUE`, the modified function is not
  returned but only exported. An error is returned if the file already
  exists in `out_path`, and if the tested function is not designated by
  a function name (like `test` or `package::test`), i.e., written by an
  inline definition (like `function(x){x}`) or by a component writing
  (like `FUN_LIST$fun`). If `FALSE` (default), the modified function is
  returned by the function instead of being exported.

- out_path:

  Single character string indicating the absolute pathway of the folder
  where to export the modified function file, named
  `<FUNCTION_NAME>_safer.R` (e.g., `mean_safer.R`). `out_path = "."`
  (default) means the R working directory set by the user. Ignored if
  the `export` argument is `FALSE`.

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

If the `export` argument is `FALSE`: the modified function, i.e., the
submitted function with all the missing curly brackets
[`{ }`](https://rdrr.io/r/base/Paren.html) added, returned by
[`base::return()`](https://rdrr.io/r/base/function.html). `NULL` is
returned if no modification was done, i.e., if the submitted function
was already compliant.

If the `export` argument is `TRUE`: the modified function is written
into the `<FUNCTION_NAME>_safer.R` file of the `out_path` directory and
`NULL` is returned (invisibly), with a message indicating where the
modified function has been exported. If no modification was done, no
file is written and `NULL` is also returned.

In both cases, a table-like text message indicating the missing curly
brackets [`{ }`](https://rdrr.io/r/base/Paren.html) is printed, or a
message saying that everything seems fine.

Table-like: column 1, the line number in the function code (starting at
the `"<- function"` line, i.e., without counting the `#'` header lines);
column 2, the construct (`if`, `else`, `for`, `while`, `repeat` or
`function`); column 3, the code preceding the construct body.

## Details

The result is used to modify the code inside the tested function like
this: `{ <CODE> }`.

More precisely, `curl_bracket_check()` verifies that the first code
element of the body of each `if`, `else`, `for`, `while`, `repeat` and
`function` construct (after the `()` of the condition or of the
arguments, when present) is a curly bracket `{`. Comments are ignored
during this verification.

In `else if` chains, an `else` part followed by an `if()` construct is
not reported, because the chained `if()` is checked on its own. When the
modified function is built (see the `returns` section), such an
`else if()` construct is left unmodified (only the `else` is not
modified): the chained `if()` construct itself is modified on its own.

Function bodies reduced to a single `.Primitive(...)` call are skipped,
because primitive functions cannot be delimited by curly brackets. For
instance, the body of
[`` base::`+` ``](https://rdrr.io/r/base/Arithmetic.html) is
`.Primitive("+")` and is not reported. Contrary to these ones,
`UseMethod(...)` bodies are reported (e.g.,
[`base::mean`](https://rdrr.io/r/base/mean.html)).

When the modified function is built, the curly brackets
[`{ }`](https://rdrr.io/r/base/Paren.html) are added as follows: a `{`
followed by a space is inserted before the first character of the
construct body, and a space followed by a `}` is inserted after the last
character of the construct body. Comments located between the construct
keyword and the body (e.g., `if(x > 1) # comment`) are kept before the
added opening bracket. If the body contains a comment between
statements, like `if(x > 1) x # comment` followed by `+ 1` on the next
line, the added closing bracket is inserted at the end of the last code
line of the body, i.e., after the comment. The added curly brackets are
always separated from the body content by a single space, and the
original code formatting is otherwise left unchanged. Warning: because
the curly brackets are added around the construct body, code lines can
become longer than 80 characters: check the modified function with a
linter if needed.

When the modified function is built, it is validated by re-parsing the
modified code: if the modified code cannot be parsed, an internal error
is returned (please, report it in the link given by the error message).
Note that the line numbers of the reported missing curly brackets
(column 1 of the table-like message) are those of the submitted
function, before modification.

When the `export` argument is `TRUE`, the exported file contains the
code of the modified function obtained with
[`base::deparse()`](https://rdrr.io/r/base/deparse.html): comments of
the submitted function are lost in this file (this is a known limitation
of [`base::deparse()`](https://rdrr.io/r/base/deparse.html)). The
returned modified function (when `export = FALSE`) preserves the
comments of the submitted function. The exported file contains an
assignment of the modified function
(`<FUNCTION_NAME>_safer <- function(...) ...`), so that
[`base::source()`](https://rdrr.io/r/base/source.html)ing it recreates
the modified function. Warning: the tested function must be designated
by a function name (like `test` or `package::test`) when
`export = TRUE`: an inline definition (like `function(x){x}`) or a
component writing (like `FUN_LIST$fun`) returns an error, because the
name of the exported file could not be determined reliably.

Warning: `curl_bracket_check()` cannot check constructs written with
backquotes, like `` `if`(x > 1, base::return(x), base::return(0)) ``.
Please, use the standard writing.

The returned line numbers are indicative, depending on which source is
checked. For instance, [`saferDev::colons_check`](colons_check.md)
(compiled) has not the same line numbers as its source file. Notably,
compiled functions do not have comments anymore, compared to the same
source function sourced into the working environment. In addition, the
counting starts at the `"<- function"` line, i.e., without counting the
`#'` header lines potentially present in source files.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferDev/articles/curl_bracket_check.html
fun1 <- function(x){ if(x > 1){ base::return(x) } else { base::return(0) } }
saferDev::curl_bracket_check(fun1)
#> 
#> 
#> AFTER RUNNING saferDev::curl_bracket_check().
#> INSIDE fun1(), EVERYTHING SEEMS CLEAN.
#> 
fun2 <- function(x){ if(x > 1) base::return(x) } ; saferDev::curl_bracket_check(fun2)
#> 
#> 
#> 
#> INSIDE fun2(), SOME CURLY BRACKETS {} ARE MISSING AFTER:
#> 
#> LINE CONSTRUCT       CODE
#> 3    if      if (x > 1) 
#> 
#> function (x) 
#> {
#>     if (x > 1) {
#>         base::return(x)
#>     }
#> }
#> <environment: 0x564c11f2eda8>
fun2 <- function(x){ if(x > 1) base::return(x) } ; fun2_safer <- saferDev::curl_bracket_check(fun2) ; fun2_safer(2)
#> 
#> 
#> 
#> INSIDE fun2(), SOME CURLY BRACKETS {} ARE MISSING AFTER:
#> 
#> LINE CONSTRUCT       CODE
#> 3    if      if (x > 1) 
#> 
#> [1] 2
fun2_ter <- function(x){ if(x > 1) base::return(x) } ; saferDev::curl_bracket_check(x = fun2_ter, export = TRUE, out_path = ".") # fun2_ter_safer.R written in the working directory, NULL returned
#> 
#> 
#> 
#> INSIDE fun2_ter(), SOME CURLY BRACKETS {} ARE MISSING AFTER:
#> 
#> LINE CONSTRUCT       CODE
#> 3    if      if (x > 1) 
#> 
#> MODIFIED CODE EXPORTED IN:
#> ./fun2_ter_safer.R
#> 
#> NULL
saferDev::curl_bracket_check(mean)
#> 
#> 
#> 
#> INSIDE mean(), SOME CURLY BRACKETS {} ARE MISSING AFTER:
#> 
#> LINE CONSTRUCT       CODE
#> 1    function        function (x, ...) 
#> 
#> function (x, ...) 
#> {
#>     UseMethod("mean")
#> }
#> <environment: namespace:base>
```
