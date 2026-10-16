# Curl Brackets Check

Verify that all the bodies of the `if`, `else`, `for`, `while`, `repeat`
and `function` (including lambda `\(x)`) constructs of a function are
delimited by curly brackets [`{ }`](https://rdrr.io/r/base/Paren.html).
For instance: `if(x > 1){ base::return(x) }` and not
`if(x > 1) base::return(x)`. The safer-r rule requires curly brackets
for every body, even single-statement ones.

## Usage

``` r
curl_bracket_check(x, safer_check = TRUE, lib_path = NULL, error_text = "")
```

## Arguments

- x:

  a function name, written without quotes and brackets.

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

A table-like text message indicating the missing curly brackets
[`{ }`](https://rdrr.io/r/base/Paren.html) or a message saying that
everything seems fine.

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
not reported, because the chained `if()` is checked on its own.

Function bodies reduced to a single `.Primitive(...)` call are skipped,
because primitive functions cannot be delimited by curly brackets. For
instance, the body of
[`` base::`+` ``](https://rdrr.io/r/base/Arithmetic.html) is
`.Primitive("+")` and is not reported. Contrary to these ones,
`UseMethod(...)` bodies are reported (e.g.,
[`base::mean`](https://rdrr.io/r/base/mean.html)).

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
saferDev::curl_bracket_check(mean)
#> 
#> 
#> 
#> INSIDE mean(), SOME CURLY BRACKETS {} ARE MISSING AFTER:
#> 
#> LINE CONSTRUCT       CODE
#> 1    function        function (x, ...) 
#> 
```
