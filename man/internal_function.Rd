% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/intern_.all_args_here_fill.R,
%   R/intern_.base_op_check.R, R/intern_.colons_check_message.R,
%   R/intern_.extract_all_fun_names.R, R/intern_.fun_args_pos.R,
%   R/intern_.functions_detect.R, R/intern_.has_odd_number_of_quotes.R,
%   R/intern_.in_parenthesis_replacement.R, R/intern_.in_quotes_replacement.R,
%   R/intern_.noclean_functions.R, R/intern_.pack_and_function_check.R
\name{.all_args_here_fill}
\alias{.all_args_here_fill}
\alias{.base_op_check}
\alias{.colons_check_message}
\alias{.extract_all_fun_names}
\alias{.fun_args_pos}
\alias{.functions_detect}
\alias{.has_odd_number_of_quotes}
\alias{.in_parenthesis_replacement}
\alias{.in_quotes_replacement}
\alias{.noclean_functions}
\alias{.pack_and_function_check}
\title{.all_args_here_fill}
\usage{
.all_args_here_fill(
  arg_full,
  arg_full_names,
  tempo_split,
  three_dots_log,
  i2,
  col1_i2,
  col2_i2,
  arg_user_setting_x,
  error_text,
  warn,
  warn_count
)

.base_op_check(error_text)

.colons_check_message(
  list.fun,
  list.fun.pos,
  line.nb,
  ini,
  arg_user_setting,
  error_text,
  text,
  internal_fun_names
)

.extract_all_fun_names(text, pattern, error_text)

.fun_args_pos(text, pattern, error_text)

.functions_detect(x, arg_user_setting, error_text)

.has_odd_number_of_quotes(input_string, pattern, error_text)

.in_parenthesis_replacement(
  string,
  pattern,
  no_regex_pattern,
  replacement,
  perl,
  open_pos,
  close_pos,
  error_text
)

.in_quotes_replacement(
  string,
  pattern,
  no_regex_pattern,
  replacement,
  perl,
  error_text
)

.noclean_functions(col1, col2, col3, ini, error_text)

.pack_and_function_check(fun, lib_path, error_text)
}
\arguments{
\item{arg_full}{List of all arguments of the function with default value.}

\item{arg_full_names}{Vector of strings of the names of the arguments of the function. Can be NULL.}

\item{tempo_split}{Vector of strings of the observed argument writting of the function.}

\item{three_dots_log}{Vector of logical. Is ... present among arg_full_names.}

\item{i2}{Single integer value indicating the loop number.}

\item{col1_i2}{Single integer value indicating the code line number of the checked function.}

\item{col2_i2}{Single string indicating the name of the checked sub function.}

\item{arg_user_setting_x}{Single string indicating the name of the checked function.}

\item{error_text}{Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = "INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>". If NULL, converted into "".}

\item{warn}{warning string. Can be NULL.}

\item{warn_count}{warning count.}

\item{list.fun}{List of names of all the functions.}

\item{list.fun.pos}{List of positions of first character of names of all the functions in ini.}

\item{line.nb}{Vector of integers of corresponding line numbers.}

\item{ini}{Vector of strings of the initial function code analyzed.}

\item{arg_user_setting}{Argument user settings list.}

\item{text}{Single string.}

\item{internal_fun_names}{Vector of strings of names of internal functions in the function code analyzed. Can be NULL}

\item{pattern}{Single string indicating the pattern to detect. Warning : must be very simple pattern, like "\\(".}

\item{x}{Function name, written without quotes and brackets.}

\item{input_string}{Single string.}

\item{string}{Single string.}

\item{no_regex_pattern}{Single string of the pattern to detect but without escape characters or list, etc.}

\item{replacement}{Single string for pattern replacement. Is not regex.}

\item{perl}{Single logical value. Use Perl regex in pattern ?}

\item{open_pos}{Single integer indicating the position of the opening parenthesis.}

\item{close_pos}{Single integer indicating the position of the closing parenthesis.}

\item{col1}{Vector of strings.}

\item{col2}{Vector of strings of the function names.}

\item{col3}{Vector of strings of the code before the function name.}

\item{fun}{Character vector of the names of the required functions, preceded by the name of the package they belong to and a double or triple colon. Example: c("ggplot2::geom_point", "grid::gpar").}

\item{lib_path}{Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful to overcome R execution using system with non admin rights for R package installation in the default directories. Ignored if NULL (default): only the pathways specified by .libPaths() are used for package calling. Specify the right path if the function returns a package path error.}

\item{pattern:}{Either '"' or "'".}
}
\value{
A list:
   $col6: the $MISSING_ARG_NAMES.
   $col7: the $MISSING_ARGS.
   $col8: the $STATUS.

An error message if at least one of the checked operator is present in the R scope, nothing otherwise.

A list:
 $output.cat: the message (string).
 $colon_not_here: logical vector. Does list.fun contain function names without :: or ::: ?

List containing:
$string: the function names without parenthesis.
$pos: position of the first character of the function names in the input string

A list containing two positions:
$begin_fun: position of 1st letter of the function name.
$begin: position of the "(" of the function.
$end: position of the closing ")" of the function.
$middle_bracket_pos: list of positions of the couple of brackets in the middle of the begin and end positions. In each compartment, the first number is the position of ( and the second the position of ). NULL if no inside brackets.

A list:
$code: vector of strings of the code of the tested function.
$all_basic_funs: vector or strings of names of all the basic R functions.
$fun_names: list of names of all the functions, not considering base::c("function", "if", "for", "while", "repeat"). Compartment names indicate the code line number of the functions in $code.
$fun_names_pos: list of position of the first character of each $fun_names. Compartment names indicate the code line number of the functions in $code.
$code_line_nb: vector of integers of the code line numbers of code for each non empty compartment of $fun_names and $fun_names_pos.
$internal_fun_names: vector of string of names of internal functions in the code of the tested function.
$arg_user_setting: list of arg user settings of the tested function.

TRUE or FALSE.

A list containing:
$string: The input string with all pattern replaced by the replacement pattern.
$pos: the positions of the 1rst character of the replaced pattern. NULL if no replaced pattern. In that case, $string is identical to the input string

A list containing:
$string: The input string with all pattern replaced by the replacement pattern.
$pos: vector of the positions of the 1rst character of the replaced pattern. NULL if no replaced pattern. In that case, $string is identical to the input string.

A logical vector indicating if function names of col2 are inside quotes or after $ (TRUE) in ini or not (FALSE). Can be length 0.

An error message if at least one of the checked packages is missing in lib_path, or if at least one of the checked functions is missing in the required package, nothing otherwise.
}
\description{
Get the $MISSING_ARG_NAMES, $MISSING_ARGS and $NEW of all_args_here().

Check if critical operators of R are not present in other packages or in the global env.
Others functions of the R scope can be overwritten because safer functions always use :: when using any function.

Create the message for the colons_check() function.

Extract all function names.

Return the positions of 1st letter of the function name and opening and closing parenthesis, as well as positions of the internal parenthesis.

Detect all the functions names used inside a function.

Determine if a string has a odd number of quotes.

Replace any pattern inside () by another replacement pattern

Replace any pattern inside simple ou double quotes by another replacement pattern and get the position of replacements

Indicate if function names are inside quotes or after $

Check if 1) required functions are present in required packages and 2) required packages are installed locally.
Simplified version of saferDev::is_function_here(), used as internal function for the other functions of the package.
}
\details{
- Warning: requires saferDev::arg_check. In the safer Backbone section "######## check of the required functions from the required packages" add this function when checking for the presence of saferDev:::.all_args_here_fill.

- Warning: requires saferDev::arg_check, .noclean_functions. In the safer Backbone section "######## check of the required functions from the required packages" add this function when checking for the presence of saferDev:::.colons_check_message.

- Warning: requires saferDev::arg_check. In the safer Backbone section "######## check of the required functions from the required packages" add this function when checking for the presence of saferDev:::.extract_all_fun_names.

- Warning: the string must be cleaned form brackets between quotes. Use .in_quotes_replacement() for that.
- Warning: quotes in strings are escaped, so that position of ( in \"a( is 3, not 4.
- Warning: requires saferDev::arg_check. In the safer Backbone section "######## check of the required functions from the required packages" add this function when checking for the presence of saferDev:::.all_args_here_fill.

- Does not check if the functions inside the code exist.
- Use the regex pattern "([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*\\(" to detect a function in the code.
- $all_basic_funs are all the functions in base::c("package:stats", "package:graphics",  "package:grDevices", "package:utils", "package:datasets", "package:methods", "Autoloads", "package:base")
- Warning: requires saferDev::arg_check, saferDev:::.extract_all_fun_names, saferDev:::.has_odd_number_of_quotes. In the safer Backbone section "######## check of the required functions from the required packages" add these functions when checking for the presence of saferDev:::.functions_detect.

- Warning: requires saferDev::arg_check. In the safer Backbone section "######## check of the required functions from the required packages" add this function when checking for the presence of saferDev:::.has_odd_number_of_quotes.

- Warning : must be very simple pattern, like "\\(".
- Warning: requires saferDev::arg_check. In the safer Backbone section "######## check of the required functions from the required packages" add this function when checking for the presence of saferDev:::.in_parenthesis_replacement.

- Warning : must be very simple pattern, like "\\(".
- Warning: requires saferDev::arg_check, saferDev:::.has_odd_number_of_quotes. In the safer Backbone section "######## check of the required functions from the required packages" add these functions when checking for the presence of saferDev:::.in_quotes_replacement.

- Warning: requires saferDev::arg_check, saferDev:::.has_odd_number_of_quotes. In the safer Backbone section "######## check of the required functions from the required packages" add these functions when checking for the presence of saferDev:::.noclean_functions.

- Warning: requires saferDev::arg_check. In the safer Backbone section "######## check of the required functions from the required packages" add this function when checking for the presence of saferDev:::.pack_and_function_check.
}
\examples{
\dontrun{ # Example that shouldn't be run because this is an internal function
.all_args_here_fill(arg_full = list(x = pairlist(x = quote(expr = ))), arg_full_names = "x", tempo_split = "x", three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 =  "length", arg_user_setting_x = "\"FUN1\"", error_text = "", warn = NULL, warn_count = 0)
}
\dontrun{
# Example that shouldn't be run because this is an internal function
assign("!", 1) ; assign("+", 2) ; .base_op_check(error_text = " INSIDE fun1.") # commented because this example returns an error
rm("!") ; rm("+") 
}
\dontrun{ # Example that shouldn't be run because this is an internal function
.colons_check_message(list.fun = list(c2 = "UseMethod"), list.fun.pos = list(c2 = 1), line.nb = 2, ini = c("function (x, ...) ", "UseMethod(\"mean\")", "<bytecode: 0x000001969e09e580>", "<environment: namespace:base>"), arg_user_setting = list(x = x, arg_user_setting = arg_user_setting, error_text = "P1::F1", internal_error_report_link = "" ), error_text = " INSIDE P1::F1", internal_error_report_link = "test", text = "BASIC", internal_fun_names = NULL)
}
\dontrun{ # Example that shouldn't be run because this is an internal function
.extract_all_fun_names(text = 'This is a test string with sum()', pattern = "[a-zA-Z.][a-zA-Z0-9._]*\\\\s*\\\\(", error_text = " INSIDE P1::F1", internal_error_report_link = "test")
}
\dontrun{ # Example that shouldn't be run because this is an internal function
# Warning : examples only with strings that must be cleaned from brackets between quotes
.fun_args_pos(text = ' "a" ; paste0("I", paste0(sum(1:3), collapse = " "), min(1) ) ; range(2)', pattern = paste0("paste0", "[\\\\s\\\\r\\\\n]*\\\\("),  error_text = " INSIDE P1::F1", internal_error_report_link = "test")
}
\dontrun{ # Example that shouldn't be run because this is an internal function
source("C:\\\\Users\\\\gmillot\\\\Documents\\\\Git_projects\\\\safer-r\\\\saferDev\\\\dev\\\\other\\\\test.R") ; .functions_detect(x = test, arg_user_setting = base::list(x =  as.name(x = "test")), error_text = " INSIDE P1::F1", internal_error_report_link = "test")
}
\dontrun{ # Example that shouldn't be run because this is an internal function
.has_odd_number_of_quotes(input_string = 'This is a "test" string with "even" quotes', pattern = '"')
}
\dontrun{ # Example that shouldn't be run because this is an internal function
.in_parenthesis_replacement(string = "pattern = base::paste0(pattern, \"\\\\\\\\(#\"), text = text", pattern = ",", no_regex_pattern = ",", replacement = " ", perl = TRUE, open_pos = 23, close_pos = 39,  error_text = " INSIDE P1::F1", internal_error_report_link = "test")
}
\dontrun{ # Example that shouldn't be run because this is an internal function
source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") ; .in_quotes_replacement(string = paste(deparse(test), collapse = ""), pattern = "\\\\)", no_regex_pattern = ")", replacement = " ", perl = TRUE, error_text = " INSIDE P1::F1", internal_error_report_link = "test")
.in_quotes_replacement(string = 'paste0("IAGE((", paste0(1:3, collapse = " "), "A)B()")', pattern = "\\\\)", no_regex_pattern = ")", replacement = " ", perl = TRUE, error_text = " INSIDE P1::F1", internal_error_report_link = "test")
}
\dontrun{ # Example that shouldn't be run because this is an internal function
source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R")
.noclean_functions(col1 =  c(15, 17), col2 = c("gregexpr", "regmatches"), col3 = c("matches <- ",  "matched_strings <- " ), ini = utils::capture.output(test), error_text = " INSIDE P1::F1", internal_error_report_link = "test")
}
\dontrun{ # Example that shouldn't be run because this is an internal function
.pack_and_function_check(fun = 1, lib_path = NULL, error_text = " INSIDE F1.") # this example returns an error
.pack_and_function_check(fun = "ggplot2::notgood", lib_path = base::.libPaths(), error_text = " INSIDE P1::F1") # this example returns an error
.pack_and_function_check(fun = c("ggplot2::geom_point", "grid::gpar"), lib_path = base::.libPaths(), error_text = " INSIDE P1::F1")
}
}
\author{
Gael Millot <gael.millot@pasteur.fr>

Yushi Han <yushi.han2000@gmail.com>

Haiding Wang <wanghaiding442@gmail.com>
}
\keyword{internal}
