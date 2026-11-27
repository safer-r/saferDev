#' @title Check and Complete the Arguments of Function Calls
#' @description
#' Checks that the function calls inside a function are written with all their arguments, and returns the function with the missing arguments added. For instance, \code{base::paste0(letters[1:2])} becomes \code{base::paste0(letters[1:2], collapse = NULL, recycle0 = FALSE)}.
#'
#' The calls must already be written with their package (e.g., \code{base::paste0()}; run \code{\link{colons_check}} first), otherwise an error is returned, except the calls not analyzed (see the Details section) and the calls, without package, of the internal functions of \code{x}, as defined in \code{\link{colons_check}} (\code{"INTERNAL_FUNCTION"}).
#' @param x Function to check, written without quotes and brackets (e.g., \code{fun1} or \code{pkg::fun1}).
#' @param table_export Single logical value. Write the table (see the Value section) into the \code{df_name} file of \code{out_path}, instead of printing it?
#' @param fun_export Single logical value. If \code{TRUE}, the corrected function is written into the \code{<name>_safer.R} file of \code{out_path} instead of being returned (e.g., \code{fun1_safer.R} for \code{x = fun1} or \code{x = pkg::fun1}), as \code{fun1_safer <- function(...)}, which \code{base::source()} recreates. Nothing is written if nothing has to be corrected, but an error is returned if the file already exists and \code{overwrite} is \code{FALSE}, or if \code{x} is not a function name (e.g., \code{function(x)\{x\}} or \code{FUN_LIST$fun}).
#' @param out_path Single character string: path of an existing folder where the files are written. Required when \code{table_export} or \code{fun_export} is \code{TRUE}; otherwise not used, but a non-\code{NULL} value must still be a single non-empty character string. \code{NULL} by default.
#' @param df_name Single character string (not \code{""}): name of the file of the table, written in \code{out_path} when \code{table_export} is \code{TRUE}.
#' @param overwrite Single logical value. Overwrite the files already existing in \code{out_path} (\code{df_name} and \code{<name>_safer.R}) when \code{table_export} or \code{fun_export} is \code{TRUE}?
#' @inheritParams is_function_here
#' @returns
#' The corrected function, which keeps the environment of \code{x}, or \code{NULL}, invisibly, if nothing has to be corrected, if \code{fun_export} is \code{TRUE}, or if \code{x} contains no function call, as a primitive function like \code{base::sum} (with a warning).
#'
#' The table of the detected calls is printed as a message, or written into a file if \code{table_export} is \code{TRUE}, but never returned. Its columns are:
#' \itemize{
#'   \item \code{$LINE_NB}: line number in the code of \code{x} (see the Details section).
#'   \item \code{$FUN_NAME}: function name.
#'   \item \code{$FUN_ARGS}: the call as written, without its package (e.g., \code{sort(v)}). \code{"NOT_CONSIDERED"} for a name followed by \code{(} inside a string (e.g., \code{"a("} in \code{base::paste0("a(", x)}), which is not a call (the calls with a function name written between quotes or after \code{$} or \code{@} are not listed).
#'   \item \code{$FUN_POS}: position of the first character of the function name in the line, after removal of the leading and repeated spaces.
#'   \item \code{$DEF_ARGS}: arguments of the function with their default values (\code{""} for a function without arguments, e.g., \code{base::Sys.time()}, and for \code{"NOT_CONSIDERED"} in \code{$FUN_ARGS}). \code{"NO_ARGS"}: primitive function whose arguments are not available (e.g., \code{base::return()}); \code{"INTERNAL_FUNCTION"}: call, written without package, of an internal function inside the function that defines it (see the Description section); \code{"SKIPPED"}: call not analyzed (see the Details section).
#'   \item \code{$MISSING_ARG_NAMES}: names of the missing arguments, and of the arguments written with a partial name.
#'   \item \code{$MISSING_ARGS}: these arguments with their proposed values.
#'   \item \code{$STATUS}: \code{"GOOD"} if all the arguments are written, otherwise the proposed call. \code{""} if the call is not analyzed (see \code{$FUN_ARGS}, \code{$DEF_ARGS} and the warnings), or if the function has no argument other than \code{...} (e.g., \code{base::Sys.time()} or \code{base::c()}).
#' }
#'
#' Other messages: \code{"EVERYTHING SEEMS CLEAN"} if no argument is missing, \code{"ARGUMENTS WITH EMPTY VALUE"} (empty values inserted into the corrected function, see the Details section) and \code{"CORRECTIONS NOT APPLIED"} (proposals not inserted into the corrected function, each with its reason).
#' @details
#' Analyzed calls: the names followed by \code{(}, except \code{function}, \code{if}, \code{for}, \code{while}, \code{repeat}, \code{else} and the names preceded by \code{$} or \code{@} (e.g., \code{x$fun()}). A function passed without brackets (e.g., \code{FUN = base::as.character}) or whose name is written between quotes (e.g., \code{base::"rownames<-"(x, "a")}) is not checked. The calls inside formulas, one-sided ones included (e.g., \code{offset()} in \code{y ~ x + offset(z)}, or \code{base::mean()} in \code{~ base::mean(.x)}), the calls inside \code{quote()}, \code{bquote()}, \code{expression()}, \code{alist()} and \code{substitute()}, whose arguments are code used as data (e.g., \code{base::paste()} in the plotmath label \code{base::expression(base::paste("Temp (", degree, "C)"))}), except the code that R evaluates (\code{.()} and spliced \code{..()} of \code{bquote()}, its \code{where} argument, and the \code{env} argument of \code{substitute()}), the function called by \code{.Internal()} (e.g., \code{toupper} in \code{base::.Internal(toupper(x))}) and \code{as.environment()} are not analyzed (\code{"SKIPPED"}). The other functions that use the code of their arguments get the completed code (e.g., the default axis label of \code{ggplot2::aes(x = base::log(y))} becomes \code{base::log(x = y, base = base::exp(x = 1))}). A call written with more arguments than its function has is not analyzed, with a warning (also for a valid replacement call like \code{base::body(f, envir = e) <- v}). The package of each \code{pkg::fun()} call must be installed (not only loaded with \code{pkgload::load_all()}), and the stats, graphics, grDevices, utils, datasets and methods packages must be attached, as in a default R session, otherwise an error is returned.
#'
#' Missing arguments: a value written without argument name gets its name (e.g., \code{base::sort(v)} becomes \code{base::sort(x = v, decreasing = FALSE)}), a partial name is completed (only for the arguments before \code{...}, as in R), and the other missing arguments are added with their default value, written with packages and argument names (e.g., \code{base::log(x = 2)} becomes \code{base::log(x = 2, base = base::exp(x = 1))}). An empty value (e.g., \code{envir = }), which a function written in R treats as a missing argument (except the first argument of an S3 generic function, which selects the method), is written instead when the default value cannot be copied safely: no default value (e.g., \code{new} in \code{base::.libPaths()}); a default value that depends on another argument, on a variable internal to the called function, on the frame where it is evaluated (e.g., \code{parent.frame()}, a formula or a function definition), or on an object that \code{x} does not find, or finds differently (e.g., an internal function of a package, or a variable masked by an argument of \code{x}); a default value whose function calls are written \code{pkg::fun()}, lack some arguments (e.g., \code{getOption("digits")}) or use the code of their arguments (e.g., \code{quote(exp(1))}); an argument tested with \code{missing()} (e.g., \code{nrow} in \code{base::matrix(x, ncol = 2)}). An argument already written with an empty value is accepted, except if its default value can be copied, which is then added.
#'
#' Corrected function: the missing arguments are inserted into the code of \code{x}, whose line breaks are kept. A proposal is not inserted, and is listed with its reason in the \code{"CORRECTIONS NOT APPLIED"} message, when the call receives a value from a pipe (\code{|>}, \code{\%>\%}, \code{\%<>\%}, \code{\%T>\%} or \code{\%!>\%}), forwards \code{...}, has an empty argument (e.g., \code{base::sum(1, )}), is the target of an assignment (e.g., \code{base::rownames(x) <- "a"}), or has a value written without name that is also proposed with its name; when the called function counts its arguments with \code{nargs()} (e.g., \code{base::diag()}), or is a builtin primitive that would receive an empty value (e.g., \code{PACKAGE = } in \code{base::.Call()}); when the called function is an S3 generic whose first argument is not written, or is written without name while a method names it differently (e.g., \code{formula} in \code{t.test.formula()}), or when a method has neither \code{...} nor an added argument (e.g., \code{digits} for \code{round.POSIXt()}), or has arguments that an added argument matches partially (e.g., \code{y} matching \code{ylim} and \code{ylab} of \code{plot.table()}); and when the call cannot be read, located, parsed or matched to its \code{$STATUS} proposal.
#'
#' An error is returned if \code{x} contains \code{\\(x)} lambda functions (only detected with source references): run \code{\link{lambda_check}} first.
#'
#' Source references: line 1 is the line of the \code{function} keyword. Comments and indentation are kept only if \code{x} carries its source references (attribute \code{srcref}). Functions created with \code{options(keep.source = FALSE)} (default of Rscript) and functions of installed packages (e.g., \code{saferDev::colons_check}) have none: their deparsed code is analyzed, without comments and with line numbers that differ from the source file.
#'
#' Limitations: the detection is text-based, and some unusual writings (e.g., in comments) can dupe it. The \code{$STATUS} proposals remain suggestions to check. Please report unexpected results \href{https://github.com/safer-r/saferDev/issues}{here}.
#' @seealso The code checkers of saferDev, in the recommended order of use: \code{\link{super_assign_check}}, \code{\link{lambda_check}}, \code{\link{colons_check}}, \code{\link{all_args_here}}, \code{\link{curly_bracket_check}}, \code{\link{true_false_check}}, \code{\link{for_loop_nb_check}} (see \code{vignette("saferDev")}).
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @author Haiding Wang
#' @author Yushi Han
#' @examples
#' # Without source references ("Run examples" link, Rscript), line numbers and comments differ.
#' # See also https://safer-r.github.io/saferDev/articles/all_args_here.html
#' fun1 <- function(x){
#'     base::paste0(x, "a") # collapse and recycle0 arguments not written
#' }
#' fun1_safer <- saferDev::all_args_here(x = fun1, table_export = FALSE, fun_export = FALSE,
#'     out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE,
#'     lib_path = NULL, error_text = "") # arguments added
#' fun1_safer # corrected function
#' # Write the table (res.tsv) and the corrected function (fun1_safer.R) in a new temporary folder:
#' tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(check = FALSE), fileext = "")
#' base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
#' saferDev::all_args_here(x = fun1, table_export = TRUE, fun_export = TRUE, out_path = tempo_dir,
#'     df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL,
#'     error_text = "")
#' base::unlink(x = tempo_dir, recursive = TRUE, force = FALSE, expand = TRUE) # folder removed
#'
#' @export
all_args_here <- function(
    x,
    table_export = FALSE,
    fun_export = FALSE,
    out_path = NULL,
    df_name = "res.tsv",
    overwrite = FALSE,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
){
    # DEBUGGING
    # FUN1 <- function(x){base::length(x)} ; x = FUN1 ; table_export = FALSE ; fun_export = FALSE ; out_path = NULL ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- "all_args_here" ; arg_user_setting = base::list(x = as.name(x = "FUN1"), table_export = FALSE, fun_export = FALSE, out_path = NULL,  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # x = .expand_R_libs_env_var ; table_export = FALSE ; fun_export = FALSE ; out_path = NULL ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- "all_args_here" ; arg_user_setting = base::list(x = as.name(x = ".expand_R_libs_env_var"), table_export = FALSE, fun_export = FALSE, out_path = NULL,  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # library(saferGraph) ; x = close2 ; table_export = FALSE ; fun_export = FALSE ; out_path = NULL ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\get_message.R") ; x = get_message ; table_export = TRUE ; fun_export = FALSE ; out_path = base::tempdir() ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # library(saferDev) ; x = get_message ; table_export = TRUE ; fun_export = FALSE ; out_path = base::tempdir() ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\all_args_here.R") ; x = all_args_here ; table_export = TRUE ; fun_export = FALSE ; out_path = base::tempdir() ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # arg_user_setting = base::list(x = as.name(x = "test2"), table_export = TRUE, fun_export = FALSE, out_path = base::tempdir(),  df_name = "res.tsv",  overwrite = TRUE,  lib_path = NULL,  safer_check = TRUE, error_text = "")
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test2.R") ; x = test2 ; table_export = TRUE ; fun_export = FALSE ; out_path = base::tempdir() ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = "" # use the folling line before out <-
    # function_name <- "all_args_here" ; arg_user_setting = base::list(x = as.name(x = "test2"), table_export = TRUE, fun_export = FALSE, out_path = base::tempdir(),  df_name = "res.tsv",  overwrite = TRUE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\.github\\profile\\backbone.R") ; x = BACKBONE ; table_export = TRUE ; fun_export = TRUE ; out_path = base::tempdir() ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = "" # use the folling line before out <-
    # function_name <- "all_args_here" ; arg_user_setting = base::list(x = as.name(x = "BACKBONE"), table_export = TRUE, fun_export = TRUE, out_path = base::tempdir(),  df_name = "res.tsv",  overwrite = TRUE, lib_path = NULL, safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # FUN1 <- function(x, y){middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, quote = FALSE, envir = base::parent.frame())} ; x = FUN1 ; table_export = FALSE ; fun_export = FALSE ; out_path = NULL ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- "all_args_here" ; arg_user_setting = base::list(x = as.name(x = "FUN1"), table_export = FALSE, fun_export = FALSE, out_path = NULL,  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # x = saferDev::colons_check ; table_export = FALSE ; fun_export = FALSE ; out_path = NULL ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- "colons.check" ; arg_user_setting = base::list(x = as.name(x = "colons.check"), table_export = FALSE, fun_export = FALSE, out_path = NULL,  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # x = saferDev::all_args_here ; table_export = FALSE ; fun_export = FALSE ; out_path = NULL ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- "all_args_here" ; arg_user_setting = base::list(x = as.name(x = "all_args_here"), table_export = FALSE, fun_export = FALSE, out_path = NULL,  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # x = .functions_detect ; table_export = FALSE ; fun_export = FALSE ; out_path = NULL ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- ".functions_detect" ; arg_user_setting = base::list(x = as.name(x = ".functions_detect"), table_export = FALSE, fun_export = FALSE, out_path = NULL,  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # fun1 <- function(){utils::installed.packages(lib.loc = lib_path)} ; x = fun1 ; table_export = FALSE ; fun_export = FALSE ; out_path = NULL ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- "fun1" ; arg_user_setting = base::list(x = as.name(x = "fun1"), table_export = FALSE, fun_export = FALSE, out_path = NULL,  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # x = .colons_check_message ; table_export = FALSE ; fun_export = FALSE ; out_path = NULL ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- ".colons_check_message" ; arg_user_setting = base::list(x = as.name(x = ".colons_check_message"), table_export = FALSE, fun_export = FALSE, out_path = NULL,  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")


    #### package name
    package_name <- "saferDev" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message
    #### end internal error report link

    #### function name
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = )) # warning: I have written n = 0 to avoid error when a safer function is inside another functions. In addition, arguments values retrieved are not evaluated base::match.call, but this is solved with get() below. Warning: envir is left empty on purpose: its default parent.frame(2L), evaluated inside base::match.call(), is the frame that called this function, where the ... of the call are expanded (base::parent.frame(n = 2L) written here would be evaluated one frame too far, which breaks the calls through base::lapply(), base::vapply(), base::apply(), base::Filter() or a function forwarding ..., base::sapply() only working by coincidence)
    if(base::is.name(x = tempo_settings[[1]]) | base::is.call(x = tempo_settings[[1]])){
        function_name <- base::paste0(tempo_settings[[1]], "()", collapse = NULL, recycle0 = FALSE)
    }else{
        function_name <- "<anonymous function>()" # the function was called through its object (e.g., base::do.call(what = <function object>, args = ...)), so that its name is not in the call
    }
    # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function_name[1] == "::()" | function_name[1] == ":::()"){
        function_name <- function_name[3]
    }
    #### end function name

    #### arguments settings
    arg_user_setting <- tempo_settings[-1] # list of the argument settings (excluding default values not provided by the user). Always a list, even if 1 argument. So ok for lapply() usage (management of NA section)
    arg_user_setting_names <- base::names(x = arg_user_setting)
    # evaluation of values if they are expression, call, etc.
    if(base::length(x = arg_user_setting) != 0){
        arg_user_setting_eval <- base::lapply(
            X = arg_user_setting_names, 
            FUN = function(x){
                base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = TRUE) # n = 2 because of lapply(), inherit = TRUE to be sure to correctly evaluate
            }
        )
        base::names(x = arg_user_setting_eval) <- arg_user_setting_names
    }else{
        arg_user_setting_eval <- NULL
    }
    # end evaluation of values if they are expression, call, etc.
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 0)), envir = base::parent.frame(n = 1))) # names of all the arguments
    #### end arguments settings

    #### error_text initiation

    ######## basic error text start
    error_text <- base::paste0(base::unlist(x = error_text, recursive = TRUE, use.names = TRUE), collapse = "", recycle0 = FALSE) # convert everything to string. if error_text is a string, changes nothing. If NULL or empty (even list) -> "" so no need to check for management of NULL or empty value
    package_function_name <- base::paste0(
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)), 
        function_name,
        collapse = NULL, 
        recycle0 = FALSE
    )
    error_text_start <- base::paste0(
        "ERROR IN ", # must not be changed, because this "ERROR IN " string is used for text replacement
        package_function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    ######## end basic error text start

    ######## internal error text
    intern_error_text_start <- base::paste0(
        package_function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    intern_error_text_end <- base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, ".", collapse = NULL, recycle0 = FALSE))
    ######## end internal error text

    ######## error text when embedding
    # use this in the error_text of safer functions if present below 
    embed_error_text  <- base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    embed_error_text  <- base::sub(pattern = "\n*$", replacement = "", x = embed_error_text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove all the trailing \n, because added later
    ######## end error text when embedding

    #### end error_text initiation

    #### argument primary checking

    ######## arg ... forbidden
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    if("..." %in% arg_names) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start, 
            "ARGUMENT ... IS NOT ALLOWED IN SAFER-R FUNCTIONS.\n\nPLEASE, REWRITE YOUR FUNCTION CORRECTLY.", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # nocov end
    ######## end arg ... forbidden

    ######## mandatory arg of safer-r functions
    mandat_args <- base::c("lib_path", "safer_check", "error_text")
    tempo_log <- ! mandat_args %in% arg_names
    if(base::any(tempo_log, na.rm = TRUE)) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S ARE", no = " IS"), 
            " MANDATORY IN SAFER-R FUNCTIONS:\n", 
            base::paste0(mandat_args[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end mandatory arg of safer-r functions

    ######## arg with no default values
    # optional section: remove the code if none of your arguments has no default value
    no_def_args <- base::c(
        "x"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(no_def_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", 
            base::paste0(no_def_args[tempo], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end arg with no default values

    ######## management of NULL arguments
    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "x",
        "table_export",
        "fun_export",
        # "out_path", # inactivated because can be NULL
        "df_name",
        "overwrite",
        "safer_check"
        # "lib_path", # inactivated because can be NULL
        # "error_text" # inactivated because NULL converted to "" above
    )
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame.
    if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA with base::is.null()
        tempo_cat <- base::paste0(
            error_text_start, 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
            " CANNOT BE NULL:\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # # before NA checking because is.na(logical()) is logical(0) (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "x",
        "table_export",
        "fun_export",
        "out_path",
        "df_name",
        "overwrite",
        "safer_check",
        "lib_path"
        # "error_text" # inactivated because empty value converted to "" above
    )
    tempo_arg_user_setting_eval <- arg_user_setting_eval[base::names(x = arg_user_setting_eval) %in% tempo_arg]
    if(base::length(x = tempo_arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = tempo_arg_user_setting_eval, 
                FUN = function(x){
                    base::length(x = x) == 0 & ! base::is.null(x = x)
                }, 
                simplify = TRUE, 
                USE.NAMES = TRUE
            ), 
            classes = "warning"
        ) # no argument provided by the user can be empty non NULL object. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE AN EMPTY NON NULL OBJECT:\n", 
                base::paste0(base::names(x = tempo_arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # Mandatory section : argument of safer-r functions cannot have NA as only value, to prevent all(, na.rm = TRUE) or any(, na.rm = TRUE) to return a logical value
    if(base::length(x = arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = base::lapply(
                    X = arg_user_setting_eval, 
                    FUN = function(x){
                        base::is.na(x = x) # if x is empty, return empty, but ok with below
                    }
                ), 
                FUN = function(x){
                    base::all(x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK
                }, 
                simplify = TRUE, 
                USE.NAMES = TRUE
            ), 
            classes = "warning"
        ) # no argument provided by the user can be just made of NA. is.na(NULL) returns logical(0), the reason why base::length(x = x) > 0 is used # warning: all(x = x, na.rm = TRUE) but normally no NA because base::is.na() used here. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE MADE OF NA ONLY:\n", 
                base::paste0(base::names(x = arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    if( ! (base::all(base::typeof(x = safer_check) == "logical", na.rm = TRUE) & base::length(x = safer_check) == 1)){ # no need to test NA because NA only already managed above and base::length(x = safer_check) == 1)
        if(base::all(base::mode(x = safer_check) == "function", na.rm = TRUE)){
            safer_check <- base::deparse1(expr = safer_check, collapse = "", width.cutoff = 500L)
        }
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).\nHERE IT IS:\n", 
            base::ifelse(test = base::length(x = safer_check) == 0 | base::all(base::suppressWarnings(expr = safer_check == base::quote(expr = ), classes = "warning"), na.rm = TRUE) | base::all(safer_check == "", na.rm = TRUE), yes = "<NULL, \"\", EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(safer_check, collapse = "\n", recycle0 = FALSE)),
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end safer_check argument checking

    ######## check of lib_path
    # must be before any :: or ::: non basic package calling
    # if(safer_check == TRUE){ # this line must be activated if lib_path is only used inside if(safer_check == TRUE){}. Which is not the case using saferDev::arg_check()
    if( ! base::is.null(x = lib_path)){ #  is.null(NA) returns FALSE so OK.
        if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = TRUE)){ # na.rm = TRUE but no NA returned with typeof (typeof(NA) == "character" returns FALSE)
            if(base::all(base::mode(x = lib_path) == "function", na.rm = TRUE)){
                lib_path <- base::deparse1(expr = lib_path, collapse = "", width.cutoff = 500L)
            }
            tempo_cat <- base::paste0(
                error_text_start,
                "THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\nHERE IT IS:\n",
                base::ifelse(test = base::length(x = lib_path) == 0 | base::all(base::suppressWarnings(expr = lib_path == base::quote(expr = ), classes = "warning"), na.rm = TRUE), yes = "<NULL, EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(lib_path, collapse = "\n", recycle0 = FALSE)),
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA. dir.exists(paths = NA) returns an error, so ok. dir.exists(paths = "") returns FALSE so ok
            tempo_log <- ! base::dir.exists(paths = lib_path)
            tempo_cat_b <- lib_path[tempo_log] # here lib_path is character string
            tempo_cat_b[tempo_cat_b == ""] <- "\"\""
            tempo_cat <- base::paste0(
                error_text_start,
                "THE DIRECTORY PATH",
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S", no = ""),
                " INDICATED IN THE lib_path ARGUMENT DO",
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "", no = "ES"),
                " NOT EXIST:\n",
                base::paste0(tempo_cat_b, collapse = "\n", recycle0 = FALSE),
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            ini_lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            base::on.exit(expr = base::.libPaths(new = ini_lib_path, include.site = FALSE), add = TRUE, after = TRUE) # return to the previous libPaths(). include.site = FALSE: exact restore, since ini_lib_path already contains the site library if it was in use
            base::.libPaths(new = base::sub(x = base::c(ini_lib_path, lib_path), pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = FALSE) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
            lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
        }
    }else{
        lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base::.libPaths(new = lib_path) # or base::.libPaths(new = base::c(base:::.libPaths(), lib_path))
    }
    # } # this line must be activated if lib_path is only used inside if(safer_check == TRUE){}. Which is not the case using saferDev::arg_check()
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check <- utils::getFromNamespace(x = ".pack_and_function_check", ns = "saferDev", pos = -1, envir = )
        .pack_and_function_check(
            fun = base::c(
                # functions required in this code
                "saferDev::arg_check", # also in internal functions
                "saferDev::is_function_here", 
                "saferDev::lambda_check", 
                # end functions required in this code
                # internal functions required in this code
                "saferDev:::.base_op_check", # also in internal functions
                "saferDev:::.functions_detect", # requires saferDev::arg_check, saferDev:::.extract_all_fun_names, saferDev:::.in_quotes_replacement
                "saferDev:::.in_quotes_replacement", # requires saferDev::arg_check
                "saferDev:::.fun_args_pos", # requires saferDev::arg_check
                "saferDev:::.extract_all_fun_names", # requires saferDev::arg_check
                "saferDev:::.in_parenthesis_replacement", # requires saferDev::arg_check
                "saferDev:::.all_args_here_fill", # requires saferDev::arg_check
                "saferDev:::.all_args_insert" # requires saferDev::arg_check
                # end internal functions required in this code
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev", pos = -1, envir = )
    .functions_detect <- utils::getFromNamespace(x = ".functions_detect", ns = "saferDev", pos = -1, envir = )
    .in_quotes_replacement <- utils::getFromNamespace(x = ".in_quotes_replacement", ns = "saferDev", pos = -1, envir = )
    .fun_args_pos <- utils::getFromNamespace(x = ".fun_args_pos", ns = "saferDev", pos = -1, envir = )
    .extract_all_fun_names <- utils::getFromNamespace(x = ".extract_all_fun_names", ns = "saferDev", pos = -1, envir = )
    .in_parenthesis_replacement <- utils::getFromNamespace(x = ".in_parenthesis_replacement", ns = "saferDev", pos = -1, envir = )
    .all_args_here_fill <- utils::getFromNamespace(x = ".all_args_here_fill", ns = "saferDev", pos = -1, envir = )
    .all_args_insert <- utils::getFromNamespace(x = ".all_args_insert", ns = "saferDev", pos = -1, envir = )
    ######## end escaping CRAN submission NOTE for internal functions

    ######## critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
            error_text = embed_error_text
        )
    }
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    arg_check_error_text <- base::paste0("ERROR ", embed_error_text, "\n\n", collapse = NULL, recycle0 = FALSE) # must be used instead of error_text = embed_error_text when several arg_check are performed on the same argument (tempo1, tempo2, see below)
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object_name))
    # add as many lines as below, for each of your arguments of your function in development
    tempo <- saferDev::arg_check(data = x, class = "function", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = table_export, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = fun_export, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = out_path)){
        tempo <- saferDev::arg_check(data = out_path, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    tempo <- saferDev::arg_check(data = df_name, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = overwrite, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    if( ! base::is.null(x = argum_check)){
        if(base::any(argum_check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste0(text_check[argum_check], collapse = "\n\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # check with r_debugging_tools
    # source("https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    tempo_arg <- base::c(
        "out_path", 
        "df_name"
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame.  #  need to test is.null() here
    if(base::any(tempo_log, na.rm = TRUE)){
        # This check is here in case the developer has not correctly fill tempo_arg
        tempo_cat <- base::paste0(
            "INTERNAL ERROR IN THE BACKBONE PART OF ", 
            intern_error_text_start, 
            "IN THE SECTION \"management of \"\" in arguments of mode character\"\n", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS ARE", no = "THIS ARGUMENT IS"), 
            " NOT CLASS \"character\":\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            "\nIf saferMake::saferMake() HAS BEEN USED TO MODIFY YOUR FUNCTION, PLEASE RESTART WITH YOUR MAIN CODE AND SELECT PROPER CLASS, TYPE AND/OR MODE FOR YOUR ARGUMENTS\n.HERE, ARGUMENTS ARE TYPE:\n", 
            base::paste0(base::sapply(X = tempo_arg, FUN = function(x){base::paste0(x, ": \"", base::typeof(x = base::get(x = x, pos = -1L, envir = base::parent.frame(n = 3), mode = "any", inherits = FALSE)), "\"", collapse = NULL, recycle0 = FALSE)}, simplify = TRUE, USE.NAMES = TRUE), collapse = "\n", recycle0 = FALSE), # parent.frame(n = 3) because the get() is evaluated inside the FUN closure of sapply. But sapply(FUN) calls lapply(FUN) internally !! So parent.frame(n = 1) is the internal lapply() frame called by sapply, parent.frame(n = 2) is sapply's frame, parent.frame(n = 3) is BACKBONE's frame. NB: paste0() and typeof() are primitives and add no frame.
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame. # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test is.null()
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), 
                base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),
                "\nCANNOT CONTAIN EMPTY STRING \"\".", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }
    }
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## reserved words
    reserved_words <- base::paste0("NOT_", "CONSIDERED", collapse = NULL, recycle0 = FALSE) # written in two parts, so that the code of all_args_here() does not contain the reserved word (all_args_here() checking itself would warn)
    ######## end reserved words

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ini_warning_length <- base::options()$warning.length # required to have the max characters of output messages
    base::options(warning.length = 8170)
    base::on.exit(expr = base::options(warning.length = ini_warning_length), add = TRUE, after = TRUE) # registered here so that warning.length is restored even if an error occurs before the warning-output on.exit() registrations below
    warn <- NULL
    warn_count <- 0
    ######## end warning initiation

    ######## graphic device checking
    ######## end graphic device checking

    ######## other checkings
    # x forwarded through the ... argument of another function (e.g., fun2 <- function(...){saferDev::all_args_here(...)} called with x = fun1): base::match.call() writes x ..1 (or ..2, etc.) in arg_user_setting, instead of its writing in the call. This writing is recovered from the promise of x by base::substitute() (x is not reassigned before this line)
    if(base::is.symbol(x = arg_user_setting$x) && base::grepl(x = base::as.character(x = arg_user_setting$x), pattern = "^\\.\\.[0-9]+$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
        arg_user_setting$x <- base::substitute(expr = x, env = base::environment(fun = NULL))
    }
    # capture of the folder path for the exports (out_path is modified below by the folding of df_name)
    out_dir <- out_path
    if(table_export == TRUE | fun_export == TRUE){
        if(base::is.null(x = out_path)){
            tempo_cat <- base::paste0(
                error_text_start,
                "THE out_path ARGUMENT MUST BE THE PATHWAY OF AN EXISTING FOLDER WHEN table_export OR fun_export IS TRUE.",
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        # removal of trailing / or \\ in the path
        if(base::grepl(x = out_path, pattern = "/$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
            out_path <- base::sub(pattern = "/$", replacement = "", x = out_path, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        }else if(base::grepl(x = out_path, pattern = "\\\\$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
            out_path <- base::sub(pattern = "\\\\$", replacement = "", x = out_path, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        }
        # end removal of trailing / or \\ in the path
        if( ! base::dir.exists(paths = out_path)){
            tempo_cat <- base::paste0(
                error_text_start,
                "DIRECTORY PATH INDICATED IN THE out_path ARGUMENT DOES NOT EXIST:\n",
                out_path,
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        out_dir <- out_path # folder path after trailing / or \\ removal
        if(fun_export == TRUE){
            # determination of the file name: name of the submitted function, as written by the user, with _safer added at the end
            # arg_user_setting$x is x as written in the call: a name (as.name(x = "test")), a call (like the parsed call "saferDev::test"), or a function object when the call holds the function itself (e.g., x = fun1 in the args argument of base::do.call()). The function object is tested first, because base::as.character() cannot convert it
            if(( ! base::is.function(x = arg_user_setting$x)) && (base::as.character(x = arg_user_setting$x)[1] == "::" | base::as.character(x = arg_user_setting$x)[1] == ":::")){
                tempo_fun_name <- base::as.character(x = arg_user_setting$x)[3] # form "package::function" or "package:::function"
            }else if(base::is.symbol(x = arg_user_setting$x)){
                tempo_fun_name <- base::as.character(x = arg_user_setting$x)[1]
            }else{
                # multi-component written name (like FUN_LIST$fun), inline definition (like function(x){x}), function object, or other unexpected writing: cannot determine a file name reliably. FUN_LIST$fun would give fun_safer.R, but a same name can designate different functions depending on the object browsed by $: the written form is not reliable for the file name
                if(base::is.function(x = arg_user_setting$x)){
                    tempo_x <- "A FUNCTION OBJECT, WITHOUT ITS NAME (E.G., x = fun1 IN THE args ARGUMENT OF base::do.call(): USE x = base::quote(expr = fun1) INSTEAD)"
                }else{
                    tempo_x <- base::paste0(base::deparse(expr = arg_user_setting$x, width.cutoff = 60L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = "\n", recycle0 = FALSE)
                }
                tempo_cat <- base::paste0(
                    error_text_start,
                    "THE x ARGUMENT MUST BE DESIGNATED BY A FUNCTION NAME (NOT BY AN INLINE FUNCTION DEFINITION, NOR BY A COMPONENT WRITING LIKE obj$fun) WHEN THE fun_export ARGUMENT IS TRUE.\nHERE IT IS:\n",
                    tempo_x,
                    collapse = NULL,
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            tempo_fun_file_name <- base::paste0(tempo_fun_name, "_safer.R", collapse = NULL, recycle0 = FALSE)
            # the file existence check is performed here, like in the other safer functions having the export argument: the check is performed before the main code, so a pre-existing file is never silently overwritten (except when overwrite = TRUE)
            if(overwrite == FALSE & base::file.exists(base::paste0(out_path, "/", tempo_fun_file_name, collapse = NULL, recycle0 = FALSE))){
                tempo_cat <- base::paste0(
                    error_text_start,
                    "FILE NAME ALREADY EXISTS AT THE INDICATED PATH:\n",
                    base::paste0(out_path, "/", tempo_fun_file_name, collapse = NULL, recycle0 = FALSE),
                    "\n\nPLEASE, RENAME OR REMOVE THIS FILE, OR CHANGE THE out_path ARGUMENT, AND RERUN ",
                    package_function_name,
                    ".",
                    collapse = NULL,
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
        if(overwrite == FALSE){
            if(table_export == TRUE){
                if(base::file.exists(base::paste0(out_path, "/", df_name, collapse = NULL, recycle0 = FALSE))){
                    tempo_cat <- base::paste0(
                        error_text_start,
                        "FILE NAME ALREADY EXISTS AT THE INDICATED PATH:\n",
                        base::paste0(out_path, "/", df_name, collapse = NULL, recycle0 = FALSE),
                        collapse = NULL,
                        recycle0 = FALSE
                    )
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }
            }
            # fun_export = TRUE: the file existence check has already been performed above, in the other checkings section (skipped when overwrite = TRUE)
        }
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    skipped_base <- base::c("function", "if", "for", "while", "repeat", "else", "in") # skipped function
    skipped_other <- base::c("as.environment") # skipped function. Write "" if no function to skip 

    # arg_user_setting$x written as a single string for the messages (same rule as in the other code checkers, without the "()" added in the messages): the writing of x in the call (name, pkg::name, pkg:::name, or other expression deparsed on one line, like dots[[1L]][[1L]] through base::Map()), or "x" for a function object, which has no name (e.g., x = fun1 in the args argument of base::do.call()). deparse() is used because base::as.character() splits a call like saferDev::colons_check into "::" "saferDev" "colons_check"
    if(base::is.function(x = arg_user_setting$x)){
        arg_user_setting$x <- "x"
    }else{
        arg_user_setting$x <- base::paste0(base::trimws(x = base::deparse(expr = arg_user_setting$x, width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), which = "left", whitespace = "[ \\t]"), collapse = " ", recycle0 = FALSE)
    }
    # pre-check of the lambda writings: lambda_check() must be run first, because the \\ writings are not managed by the text-based detection of all_args_here(). export = FALSE: no file writing, thus out_path = NULL
    tempo_lambda <- base::suppressMessages(expr = saferDev::lambda_check(x = x, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text), classes = "message")
    if( ! base::is.null(x = tempo_lambda)){
        tempo_cat <- base::paste0(
            error_text_start,
            "SOME LAMBDA FUNCTIONS (WRITTEN \\(x)) ARE DETECTED IN THE CODE OF ",
            arg_user_setting$x,
            ".\n\nPLEASE, RUN saferDev::lambda_check(",
            arg_user_setting$x,
            ") FIRST,\nUSE THE CORRECTED FUNCTION RETURNED BY lambda_check() (OR EXPORT IT WITH export = TRUE),\nAND RERUN saferDev::all_args_here(",
            arg_user_setting$x,
            ").",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    out_path <- base::paste0(out_path, "/", df_name, collapse = NULL, recycle0 = FALSE)
    out <- .functions_detect(
        x = x, 
        skipped_base = skipped_base, 
        arg_user_setting2 = arg_user_setting, 
        lib_path = lib_path, 
        error_text = embed_error_text
    )
    if(base::is.null(x = out)){
        base::return(base::invisible(x = NULL)) # tested function empty of function: warning already returned by .functions_detect()
    }
    code_line_nb <- out$code_line_nb # vector of line numbers in code where functions are
    fun_names <-  out$fun_names # list of function names for each line of code
    fun_names_pos <-  out$fun_names_pos # list of pos (1st character) of function names for each line of code

    code <- out$code # vector of strings of the tested function code
    fun_1_line <- base::paste(out$code, collapse = " ", recycle0 = FALSE, sep = " ") # assemble the code of the tested  function (without comments) in a single line
    if(base::grepl(x = fun_1_line, pattern = reserved_words, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") THE RESERVED WORD \"", base::paste0(reserved_words, collapse = " ", recycle0 = FALSE), "\" HAS BEEN DETECTED IN THE CODE OF THE INPUT FUNCTION,\nWHICH COULD HAMPER THE ACCURACY OF THE OUTPUT TABLE.", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
    }
    # cumulative nchar of each non empty lines of code 
    cum_nchar_code_line <- base::cumsum(x = base::nchar(x = out$code, type = "chars", allowNA = FALSE, keepNA = NA) + 1) # +1 because of the 1 space added between lines of codes fused # will serve to know the position of first character of fun_names in fun_1_line
    cum_nchar_code_line <- base::c(0, cum_nchar_code_line[-base::length(x = cum_nchar_code_line)]) # because pos in fun_names_pos will be added to this
    cum_nchar_code_line <- cum_nchar_code_line[code_line_nb]
    # end cumulative nchar of each non empty lines of code 
    # replacement of all the ) between quotes
    tempo <- .in_quotes_replacement(
        string = fun_1_line, 
        pattern = "\\)", 
        no_regex_pattern = ")", 
        replacement = " ", 
        perl = TRUE, 
        lib_path = lib_path, 
        error_text = embed_error_text
    )
    fun_1_line_replace <- tempo$string # code of the tested function that will serve to better detect functions in it
    pos_rep <- tempo$pos # replaced positions in fun_1_line
    # end replacement of all the ) between quotes
    # replacement of all the ( between quotes
    tempo <- .in_quotes_replacement(
        string = fun_1_line_replace, 
        pattern = "\\(", 
        no_regex_pattern = "(", 
        replacement = " ", 
        perl = TRUE, 
        lib_path = lib_path, 
        error_text = embed_error_text
    )
    fun_1_line_replace <- tempo$string
    pos_rep <- base::sort(x = base::c(pos_rep, tempo$pos), decreasing = FALSE)
    # end replacement of all the ( between quotes
    # recovery of the functions, in the tested function, with written arguments inside ()
    arg_string_for_col3 <- fun_names # like fun_names but added with all what is between ()
    arg_string <- fun_names # like arg_string_for_col3 but with only the arguments
    mid_bracket_pos_in_fun_1_line <- base::lapply(X = fun_names, FUN = function(x){base::lapply(X = x, FUN = function(y){NULL})}) # list of lists, will be used to get inside ( and ) positions, from fun_1_line
    for(i1 in base::seq_along(along.with = fun_names)){
        tempo_pos_in_code <- base::as.integer(x = base::sub(pattern = "^c", replacement = "", x = base::names(x = fun_names)[i1], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE))
        tempo_which <- base::which(x = code_line_nb %in% tempo_pos_in_code, arr.ind = FALSE, useNames = TRUE)
        for(i2 in base::seq_along(along.with = fun_names[[i1]])){
            fun_pos_start <- fun_names_pos[[i1]][i2] + cum_nchar_code_line[tempo_which]
            fun_pos_stop <- fun_pos_start + base::nchar(x = fun_names[[i1]][i2], type = "chars", allowNA = FALSE, keepNA = NA) - 1
            tempo_fun <- base::substr(x = fun_1_line_replace, start = fun_pos_start, stop = fun_pos_stop)
            if(tempo_fun != fun_names[[i1]][i2]){
                # nocov start
                # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
                tempo_cat <- base::paste0("INTERNAL ERROR 1 IN ", intern_error_text_start, "tempo_fun MUST BE IDENTICAL TO fun_names[[i1]][i2]\n\ntempo_fun: ", tempo_fun, "\n\nfun_names[[i1]][i2]: ", fun_names[[i1]][i2], "\n\ni1: ", i1, "\n\ni2: ", i2, intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                # nocov end
            }
            tempo_str_after <- base::substr(x = fun_1_line_replace, start = fun_pos_stop + 1, stop = base::nchar(x = fun_1_line_replace, type = "chars", allowNA = FALSE, keepNA = NA))
            if(base::grepl(x = tempo_str_after, pattern = "^[\\s\\r\\n]*\\(", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)){ # detection that it is a function of interest because ( after function name not removed
                tempo_text <- fun_1_line_replace
                base::substr(x = tempo_text, start = 1, stop = fun_pos_start - 1) <- base::strrep(x = " ", times = fun_pos_start - 1) # the call is searched from the position of its name: the same name written before, but not called, followed by a line starting with ( (e.g., y <- base::paste0<line break>(x)), would be taken for this call
                tempo_pos <- .fun_args_pos(
                    text = tempo_text,
                    pattern = base::paste0("(?<![a-zA-Z._])", base::gsub(pattern = ".", replacement = "\\.", x = fun_names[[i1]][i2], ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), "[\\s\\r\\n]*\\(", collapse = NULL, recycle0 = FALSE), # the name as a whole word, with its dots escaped: not preceded by a letter, a dot or an underscore, i.e., not the end of a longer word (e.g., or, the end of for, before the bracket of for()), and not any characters matched by a dot (e.g., the name .. of the ..() of bquote(), which would match the two spaces before the bracket of a call already processed)
                    lib_path = lib_path, 
                    error_text = embed_error_text
                ) # positions of 1st letter of the function name and opening and closing brackets # Warning: fun_1_line_replace used because the input string must be cleaned form brackets between quotes
                tempo_str_before <- base::substr(x = fun_1_line_replace, start = 1, stop = fun_pos_start - 1)
                tempo_log <- base::grepl(x = tempo_str_before, pattern = "[$@][[:space:]]*$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # same rule as in .colons_check_message()
                if(tempo_log){ # remove functions preceded by $ or @, like a$fun() or a@fun()
                    arg_string_for_col3[[i1]][i2] <- ""
                    fun_names[[i1]][i2] <- ""
                    arg_string[[i1]][i2] <- ""
                    base::substr(x = fun_1_line_replace, start = 1, stop = tempo_pos$begin - 1) <- base::paste(base::rep(x = " ", tempo_pos$begin - 1), collapse = "", recycle0 = FALSE, sep = " ") # function name also replaced by spaces, as below, otherwise a next call of a function with the same name would get the position of this one
                }else{
                    arg_string_for_col3[[i1]][i2] <- base::substr(x = fun_1_line, start = tempo_pos$begin_fun, stop = tempo_pos$end) # add the "function(args)" string into arg_string_for_col3. I use fun_1_line because I want unaltered values of args here (fun_1_line_replace have quoted () replaced by spaces)
                    arg_string[[i1]][i2] <- base::substr(x = fun_1_line, start = tempo_pos$begin + 1, stop = tempo_pos$end - 1) # idem arg_string_for_col3 but inside () of the function (just the arguments written)
                    if( ! base::is.null(x = tempo_pos$middle_bracket_pos)){ # I have to use if(){}, otherwise mid_bracket_pos_in_fun_1_line[[i1]][[i2]] disappears
                        mid_bracket_pos_in_fun_1_line[[i1]][[i2]] <- base::unlist(x = tempo_pos$middle_bracket_pos, recursive = TRUE, use.names = TRUE) # positions of the () inside a function
                    }
                    base::substr(x = fun_1_line_replace, start = 1, stop = tempo_pos$begin - 1) <- base::paste(base::rep(x = " ", tempo_pos$begin - 1), collapse = "", recycle0 = FALSE, sep = " ") # trick that replaces function name by the same number of spaces. This, to avoid to take always the first paste0 for instance in the fun_1_line_replace string when several are present in fun_names
                }
            }else{
                arg_string_for_col3[[i1]][i2] <- reserved_words
                arg_string[[i1]][i2] <- ""
            }
        }
    }
    # end recovery of the functions, in the tested function, with written arguments inside ()
    # recovery of the positions of inside () in col3
    mid_bracket_pos_in_col3 <- base::lapply(X = fun_names, FUN = function(x){base::lapply(X = x, FUN = function(y){NULL})}) # list of lists, will be used to get inside ( and ) positions, from col3
    for(i1 in base::seq_along(along.with = fun_names)){
        for(i2 in base::seq_along(along.with = fun_names[[i1]])){
            pattern2 <- base::paste0(base::gsub(pattern = ".", replacement = "\\.", x = fun_names[[i1]][i2], ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), "[\\s\\r\\n]*\\(", collapse = NULL, recycle0 = FALSE) # function detection in col3, which starts with the function name (dots escaped as above)
            # pattern2 <- paste0("[a-zA-Z.][a-zA-Z0-9._]* *\\$ *", fun_names[[i1]][i2], "[\\s\\r\\n]*\\(") # function like a$fun()
            if(base::grepl(x = arg_string_for_col3[[i1]][i2], pattern = pattern2, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){ # because of "NOT_CONSIDERED" in some cases
                # detection of inside () between quotes
                tempo1 <- .in_quotes_replacement(
                    string = arg_string_for_col3[[i1]][i2], 
                    pattern = "\\(", 
                    no_regex_pattern = "(", 
                    replacement = " ", 
                    perl = TRUE, 
                    lib_path = lib_path, 
                    error_text = embed_error_text
                )
                tempo2 <- .in_quotes_replacement(
                    string =tempo1$string, 
                    pattern = "\\)", 
                    no_regex_pattern = ")", 
                    replacement = " ", 
                    perl = TRUE, 
                    lib_path = lib_path, 
                    error_text = embed_error_text
                )
                tempo_pos <- .fun_args_pos(
                    text = tempo2$string, 
                    pattern = pattern2, 
                    lib_path = lib_path, 
                    error_text = embed_error_text
                ) # positions of 1st letter of the function name and opening and closing brackets # Warning: tempo2$string used because the input string must be cleaned form brackets between quotes
                if( ! base::is.null(x = tempo_pos$middle_bracket_pos)){ # I have to use if(){}, otherwise mid_bracket_pos_in_fun_1_line[[i1]][[i2]] disappears
                    mid_bracket_pos_in_col3[[i1]][[i2]] <- base::unlist(x = tempo_pos$middle_bracket_pos, recursive = TRUE, use.names = TRUE) # positions of the () inside a function
                }
            }
        }
    }
    # end recovery of the positions of inside () in col3
    # preparation of columns
    code_for_col <- code[code_line_nb]
    code_for_col <- base::as.vector(x = base::unlist(x = base::mapply(FUN = function(x, y){base::rep(x = y, base::length(x = x))}, x = fun_names, y = code_for_col, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE), recursive = TRUE, use.names = TRUE), mode = "any")
    col1 <- base::as.vector(x = base::unlist(x = base::mapply(FUN = function(x, y){base::rep(x = y, base::length(x = x))}, x = fun_names, y = code_line_nb, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE), recursive = TRUE, use.names = TRUE), mode = "any") # code line number
    col2 <- base::as.vector(x = base::unlist(x = fun_names, recursive = TRUE, use.names = TRUE), mode = "any") # all the function names inside the tested functions (functions between quotes are already removed thanks to fun_1_line_replace)
    col3 <- base::as.vector(x = base::unlist(x = arg_string_for_col3, recursive = TRUE, use.names = TRUE), mode = "any") # as col2 but with its arguments between ()
    col4 <- base::as.vector(x = base::unlist(x = fun_names_pos, recursive = TRUE, use.names = TRUE), mode = "any") # as col2 but position in the code string of 1st character of function name. From col4, we can have pos of opening ( with col4 + nchar(col2) and pos of closing ) with col4 + nchar(col3)
    col_skip <- base::as.vector(x = base::unlist(x = out$fun_names_skip, recursive = TRUE, use.names = TRUE), mode = "any") # as col2 but TRUE for the names not analyzed: names of the code that R does not evaluate as written (formulas, quoted code) and name of the function called by .Internal() (see .functions_detect()). Warning: these names are not removed from fun_names above, because each call is found as the first remaining name( of the line, the names already processed being replaced by spaces
    col_internal <- base::as.vector(x = base::unlist(x = out$fun_names_internal, recursive = TRUE, use.names = TRUE), mode = "any") # as col2 but TRUE for the names of the internal functions of x, inside their scope (see .functions_detect())
    middle_bracket <- base::do.call(what = base::c, args = mid_bracket_pos_in_col3, quote = FALSE, envir = base::parent.frame(n = 1)) #  concatenate the sublists into a single list -> flatten the outer list while keeping the result as a list
    middle_bracket_open_in_col3 <- base::lapply(X = middle_bracket, FUN = function(x){if( ! base::is.null(x = x)){x[base::seq(1, base::length(x = x), by = 2)]}else{NULL}})
    middle_bracket_close_in_col3 <- base::lapply(X = middle_bracket, FUN = function(x){if( ! base::is.null(x = x)){x[base::seq(2, base::length(x = x), by = 2)]}else{NULL}})
    if( ! (base::length(x = col1) == base::length(x = col2) & base::length(x = col1) == base::length(x = col3) & base::length(x = col1) == base::length(x = col4) & base::length(x = col1) == base::length(x = code_for_col) & base::length(x = col1) == base::length(x = middle_bracket))){
        # nocov start
        # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
        tempo_cat <- base::paste0("INTERNAL ERROR 2 IN ", intern_error_text_start, "LENGTHS OF col1 (", base::length(x = col1), "), col2 (", base::length(x = col2), "), col3 (", base::length(x = col3), "), col4 (", base::length(x = col4), "), code_for_col (", base::length(x = code_for_col), "), AND middle_bracket (", base::length(x = middle_bracket), "), SHOULD BE EQUAL.", intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
    }
    tempo_log <- base::as.vector(x = base::unlist(x = base::mapply(
        FUN = function(x, y){
            if(y != ""){
                if(base::grepl(x = y, pattern = base::paste0("^", x, "[\\s\\r\\n]*\\(.*\\)$", collapse = NULL, recycle0 = FALSE), ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) | base::grepl(x = y, pattern = base::paste0("^", reserved_words, "$", collapse = NULL, recycle0 = FALSE), ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
                    base::return(FALSE)
                }else{
                    # nocov start
                    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
                    base::return(TRUE) # TRUE = problem: does not start by what is expected, i.e., base::paste0("^", x, "[\\s\\r\\n]*\\(.*\\)$" # related to internal error 3. Thus, cannot be code covered.
                    # nocov end
                }
            }else{
                base::return(FALSE)
            }
        }, 
        x = col2, 
        y = col3,
        MoreArgs = NULL, 
        SIMPLIFY = TRUE, 
        USE.NAMES = TRUE
    ), recursive = TRUE, use.names = TRUE), mode = "any")
    if(base::any(tempo_log, na.rm = TRUE)){
        # nocov start
        # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
        tempo_cat <- base::paste0("INTERNAL ERROR 3 IN ", intern_error_text_start,"col3 MUST BE MADE OF STRINGS STARTING BY\n\"<FUNCTION_NAME>[\\s\\r\\n]*\\(\"\nAND FINISHING BY\")\"\nHERE IT IS:\n\n", base::paste0(col3, collapse = "\n", recycle0 = FALSE), intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
    }
    # data.frame(POS_IN_CODE = col1, FUN = col2, FUN_OBS_ARGS = col3, base::as.vector(base::unlist(arg_string_for_col5)))
    # removal of detected function preceded by $ or @, which are "" in col2
    tempo_log <- col2 == "" 
    if(base::any(tempo_log, na.rm = TRUE)){
        code_for_col <- code_for_col[ ! tempo_log]
        col1 <- col1[ ! tempo_log]
        col2 <- col2[ ! tempo_log]
        col3 <- col3[ ! tempo_log]
        col4 <- col4[ ! tempo_log]
        col_skip <- col_skip[ ! tempo_log]
        col_internal <- col_internal[ ! tempo_log]
        middle_bracket <- middle_bracket[ ! tempo_log]
        middle_bracket_open_in_col3 <- middle_bracket_open_in_col3[ ! tempo_log]
        middle_bracket_close_in_col3 <- middle_bracket_close_in_col3[ ! tempo_log]
    }
    # end removal of detected function preceded by $ or @, which are "" in col2
    # end preparation of columns
    # two new columns for arg proposal
    output_fun <- NULL # modified function returned (NULL if no modification has been applied, or if the modified function is exported)
    if( (base::length(x = col1) == 0)){
        base::message(base::paste0("\n\nINSIDE ", base::as.character(x = out$arg_user_setting$x), "(), EVERYTHING SEEMS CLEAN, AS NO ANALYZABLE FUNCTION DETECTED.\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE)
    }else{
        col5 <- NULL # all arguments of the function with default value
        col6 <- NULL # missing arg names
        col7 <- NULL # potential missing args with values
        col8 <- NULL # reconstructed function with all arg
        empty_args <- NULL # args written (or proposed) with an empty value, like envir =
        empty_keys <- NULL # "line <n>: <STATUS>" of the call of each element of empty_args, to keep only the proposals inserted into the modified function
        frame_dep_funs <- base::c("parent.frame", "environment", "sys.call", "sys.calls", "sys.function", "sys.frame", "sys.frames", "sys.nframe", "sys.parent", "sys.parents", "sys.on.exit", "match.call", "missing", "nargs") # functions that make a default value depend on the frame where it is evaluated
        no_insert <- base::rep(x = "", times = base::length(x = col1)) # for each call, reason why the proposal must not be inserted into the modified function ("" if none): "pipe", "dots", "empty_arg", "nargs", "generic_dispatch", "generic_first_arg", "method_args", "method_partial" or "builtin_empty" (see below)
        # names of the arguments tested with missing() in an expression (e.g., the body of a function)
        missing_walk <- function(expr){
            tempo_args <- base::character(length = 0L)
            if(base::is.call(x = expr)){
                if(base::identical(x = expr[[1]], y = base::as.name(x = "missing"), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE) | base::identical(x = expr[[1]], y = base::quote(expr = base::missing), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                    if(base::length(x = expr) >= 2){
                        if(base::is.name(x = expr[[2]])){
                            tempo_args <- base::as.character(x = expr[[2]])
                        }
                    }
                }
                for(i8 in base::seq_along(along.with = expr)){
                    tempo_args <- base::c(tempo_args, missing_walk(expr = expr[[i8]]))
                }
            }
            base::return(tempo_args)
        }
        # TRUE if an expression (e.g., the body of a function) contains a call of UseMethod() without its object argument: the method is then chosen from the first argument written in the call of the generic, not from the default value of its first argument
        usemethod_walk <- function(expr){
            if(base::is.call(x = expr)){
                if(base::identical(x = expr[[1]], y = base::as.name(x = "UseMethod"), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE) | base::identical(x = expr[[1]], y = base::quote(expr = base::UseMethod), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                    tempo_empty <- base::vapply(X = base::seq_along(along.with = expr)[-1], FUN = function(k){base::identical(x = expr[[k]], y = base::quote(expr = ), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)}, FUN.VALUE = base::logical(length = 1L), USE.NAMES = FALSE) # index based because an empty value cannot be passed as an argument of a function
                    if(base::length(x = expr) < 3 | base::any(tempo_empty, na.rm = TRUE)){
                        base::return(TRUE) # UseMethod("f"), or object written with an empty value (e.g., base::UseMethod(generic = "f", object = ))
                    }
                }
                for(i8 in base::seq_along(along.with = expr)){
                    if(usemethod_walk(expr = expr[[i8]])){
                        base::return(TRUE)
                    }
                }
            }
            base::return(FALSE)
        }
        # features of the code of a called function (fun, named fun_name, of the package pack_name, whose first argument is first_arg), and of the code of its methods if it is an S3 generic (UseMethod() in its code, or primitive function that dispatches internally, like base::round()):
        # - missing: arguments tested with missing(). Their default value is not copied into the call, because it can change the result (e.g., nrow = 1 in base::matrix(x, ncol = 2)): an empty value is written instead, which keeps missing() TRUE in the called function
        # - nargs: use of nargs(). Nothing is inserted into the call, because any argument added, even with an empty value, can change the result (e.g., base::diag(3))
        # - dispatch: TRUE if fun is an S3 generic that chooses its method from the first argument written in its call (see usemethod_walk() above)
        # - method_args: argument names of each method without ..., which fails if an argument with another name is added (e.g., x and units for round.POSIXt() of base::round())
        # - first_arg_differs: TRUE if fun is an S3 generic and a method registered for it names its first argument differently from first_arg (e.g., formula in t.test.formula() for stats::t.test()): naming the first argument of the call with first_arg then breaks this method. A method whose first argument is ... takes any argument name, and is not considered
        # - table_method_args: argument names of each method registered for fun, used to detect the added arguments that such a method matches partially (e.g., y = in graphics::plot(x = t, y = ) matches ylim and ylab of plot.table(), which fails)
        # the methods are the functions <fun_name>.<class> of the package of the generic (e.g., as.POSIXct.POSIXlt() for base::as.POSIXct()). For a primitive function, and for method_args, only those whose first argument is first_arg are kept, to exclude other functions named the same way (e.g., all.names() for base::all())
        # for first_arg_differs and table_method_args, the methods are the ones registered in the S3 methods table of the namespace where the generic is defined, by its package or by another one (e.g., plot.formula() and plot.table(), registered by graphics for base::plot(), also called graphics::plot()): the functions only named like a method are not considered (e.g., qr.coef() for base::qr())
        missing_nargs_get <- function(fun, fun_name, pack_name, first_arg){
            tempo_res <- base::list(missing = base::character(length = 0L), nargs = FALSE, dispatch = FALSE, method_args = base::list(), first_arg_differs = FALSE, table_method_args = base::list())
            tempo_prim <- base::is.primitive(x = fun)
            tempo_generic <- "UseMethod" %in% base::all.names(expr = base::body(fun = fun), functions = TRUE, max.names = -1L, unique = TRUE) # FALSE for a primitive function (no R code)
            if(tempo_prim){
                tempo_funs <- base::list() # no R code
            }else{
                tempo_funs <- base::list(fun)
            }
            if(tempo_generic){
                tempo_res$dispatch <- usemethod_walk(expr = base::body(fun = fun)) # usemethod_walk() only run on S3 generics, because it walks through the whole code
                tempo_table <- base::get0(x = ".__S3MethodsTable__.", envir = base::topenv(envir = base::environment(fun = fun), matchThisEnv = base::getOption(x = "topLevelEnvironment", default = NULL)), mode = "any", inherits = FALSE, ifnotfound = NULL) # S3 methods table of the namespace of the generic (NULL if the generic is not defined in a namespace)
                tempo_table_names <- base::as.character(x = base::names(x = tempo_table)) # character(0) if tempo_table is NULL
                tempo_table_names <- tempo_table_names[base::startsWith(x = tempo_table_names, prefix = base::paste0(fun_name, ".", collapse = NULL, recycle0 = FALSE))]
                for(i8 in base::seq_along(along.with = tempo_table_names)){
                    tempo_method_formals <- base::as.character(x = base::names(x = base::formals(fun = base::get0(x = tempo_table_names[i8], envir = tempo_table, mode = "function", inherits = FALSE, ifnotfound = NULL), envir = tempo_table))) # arguments of the method, character(0) if the method has no argument (or is primitive)
                    tempo_res$table_method_args <- base::c(tempo_res$table_method_args, base::list(tempo_method_formals))
                    tempo_method_first <- base::c(tempo_method_formals, "...")[1] # first argument of the method, ... if the method has no argument (or is primitive)
                    if( ! tempo_method_first %in% base::c(first_arg, "...")){
                        tempo_res$first_arg_differs <- TRUE
                    }
                }
            }
            if(tempo_prim | tempo_generic){
                tempo_ns <- base::asNamespace(ns = pack_name, base.OK = TRUE)
                tempo_ns_names <- base::names(x = tempo_ns)
                tempo_ns_names <- tempo_ns_names[base::startsWith(x = tempo_ns_names, prefix = base::paste0(fun_name, ".", collapse = NULL, recycle0 = FALSE))]
                for(i8 in base::seq_along(along.with = tempo_ns_names)){
                    tempo_method <- base::get(x = tempo_ns_names[i8], pos = -1L, envir = tempo_ns, mode = "any", inherits = FALSE)
                    if(base::is.function(x = tempo_method) & ! base::is.primitive(x = tempo_method)){
                        tempo_method_args <- base::names(x = base::formals(fun = tempo_method, envir = tempo_ns))
                        tempo_same_first <- base::identical(x = tempo_method_args[1], y = first_arg, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)
                        if(( ! tempo_prim) | tempo_same_first){
                            tempo_funs <- base::c(tempo_funs, tempo_method)
                        }
                        if(tempo_same_first & ! "..." %in% tempo_method_args){
                            tempo_res$method_args <- base::c(tempo_res$method_args, base::list(tempo_method_args))
                        }
                    }
                }
            }
            for(i8 in base::seq_along(along.with = tempo_funs)){
                tempo_body <- base::body(fun = tempo_funs[[i8]])
                tempo_names <- base::all.names(expr = tempo_body, functions = TRUE, max.names = -1L, unique = TRUE)
                if("nargs" %in% tempo_names){
                    tempo_res$nargs <- TRUE
                }
                if("missing" %in% tempo_names){
                    tempo_res$missing <- base::unique(x = base::c(tempo_res$missing, missing_walk(expr = tempo_body)), incomparables = FALSE)
                }
            }
            base::return(tempo_res)
        }
        # copy of the default values into the calls (see default_copy() below)
        # names assigned in an expression (e.g., the body of a function): targets of <-, = and <<- (x for x <- v, x[i] <- v or names(x) <- v), variables of the for loops, and arguments of the functions defined in the expression
        assign_walk <- function(expr){
            tempo_names <- base::character(length = 0L)
            if(base::is.call(x = expr)){
                if(base::is.name(x = expr[[1]]) & base::length(x = expr) > 2){
                    if(base::as.character(x = expr[[1]]) %in% base::c("<-", "=", "<<-", "for")){
                        tempo_target <- expr[[2]]
                        while(base::is.call(x = tempo_target) & base::length(x = tempo_target) > 1){
                            tempo_target <- tempo_target[[2]]
                        }
                        if(base::is.name(x = tempo_target) | base::is.character(x = tempo_target)){
                            tempo_names <- base::as.character(x = tempo_target)
                        }
                    }else if(base::as.character(x = expr[[1]]) == "function"){
                        tempo_names <- base::names(x = expr[[2]])
                    }
                }
                for(i8 in base::seq_along(along.with = expr)){
                    tempo_names <- base::c(tempo_names, assign_walk(expr = expr[[i8]]))
                }
            }
            base::return(tempo_names)
        }
        x_env <- base::environment(fun = x) # environment of x, from which the code of x finds its objects
        x_local <- base::c(base::names(x = base::formals(fun = x, envir = x_env)), assign_walk(expr = base::body(fun = x))) # arguments of x and names assigned in x, which can mask the variables of a default value copied into x
        caller_dep_funs <- base::c("parent.frame", "sys.call", "sys.calls", "sys.function", "sys.frame", "sys.frames", "sys.nframe", "sys.parent", "sys.parents", "substitute", "match.call") # functions whose result depends on the call of the function that uses them: on its frame, or on the code written as its arguments (substitute() and match.call(), e.g., deparse(substitute(x)) used as a label, or bquote()), which writing the packages and argument names would change
        # object found under a name from an environment, as R finds it (mode "function" for the name of a called function), in a list (list(NULL) for the NULL object): NULL if not found, or if its search fails (e.g., promise whose evaluation returns an error)
        object_get <- function(name, envir, mode){
            base::tryCatch(
                expr = {
                    if(base::exists(x = name, where = -1, envir = envir, frame = , mode = mode, inherits = TRUE)){ # frame = written empty because absence of value means something in base::exists()
                        base::list(base::get(x = name, pos = -1L, envir = envir, mode = mode, inherits = TRUE))
                    }else{
                        NULL
                    }
                },
                error = function(e){NULL},
                finally =
            )
        }
        # TRUE if the object found under a name (a variable, or mode "function" for the name of a called function) from the environment of x is the one found from the environment of the called function (fun_env), so that a default value using this name gives the same result in x. FALSE if the name is an argument of the called function (fun_args), a name assigned in its code (fun_local, e.g., a variable created in its body), or a function that depends on the frame of its call (frame_dep_funs, e.g., parent.frame()), and, for a variable, if the name is an argument of x or a name assigned in x (x_local), because the copied value is evaluated in the frame of x
        name_copiable <- function(name, mode, fun_env, fun_args, fun_local){
            if(name %in% base::c(fun_args, fun_local, frame_dep_funs) | (mode == "any" & name %in% x_local)){
                base::return(FALSE)
            }
            tempo_x_obj <- object_get(name = name, envir = x_env, mode = mode)
            base::return(( ! base::is.null(x = tempo_x_obj)) && base::identical(x = tempo_x_obj, y = object_get(name = name, envir = fun_env, mode = mode), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE))
        }
        # definition of the arguments of a function: the function itself, or base::args() for a primitive function (NULL if not available, e.g., base::return()), with the argument of base::isS4() named x instead of object (base::args() names it object, but this primitive function only accepts x: base::isS4(object = 1) fails)
        fun_def <- function(fun){
            if( ! base::is.primitive(x = fun)){
                base::return(fun)
            }
            tempo_def <- base::args(name = fun)
            if(base::identical(x = fun, y = base::isS4, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                base::formals(fun = tempo_def, envir = base::environment(fun = tempo_def)) <- base::alist(x = )
            }
            base::return(tempo_def)
        }
        # default value of an argument of a called function (environment fun_env, arguments fun_args, names assigned in its code fun_local), written to be copied into the call, in a list (list(NULL) for the NULL value), or NULL if it cannot be copied, because it would not give the same result in the frame of x:
        # - each name (function or variable) of the default value must be copiable (see name_copiable() above). The right operand of $ and @, and the operands of :: and ::: (e.g., FUN = stats::sd), are names of elements or of packages, not objects
        # - a function called by its name is written with its package (e.g., base::exp(x = 1) for exp(1)), so that all_args_here() run on the modified function does not stop on it, and with its arguments named (base::match.call() rules), so that all_args_here() run on the modified function finds nothing to complete in the copied value. Not copied if the function is not exported by its package, which cannot be written pkg::fun, if an argument of the function other than ... is not written (e.g., getOption("digits"), whose default argument is not written), or has an empty value
        # - a call of a function written pkg::fun(), of a function that depends on the frame of its call (e.g., a function whose code uses parent.frame()) or on the code written as its arguments (see caller_dep_funs above), a formula and a function definition (which keep the environment where they are created) are not copied, nor a call of quote(), bquote(), expression(), substitute() or alist(), which return the code of their arguments, or of .Internal(), which calls the internal function named in its argument: writing the packages and argument names in their arguments would change their result (e.g., quote(exp(1)))
        default_copy <- function(expr, fun_env, fun_args, fun_local){
            if(base::is.name(x = expr)){
                if(name_copiable(name = base::as.character(x = expr), mode = "any", fun_env = fun_env, fun_args = fun_args, fun_local = fun_local)){
                    base::return(base::list(expr))
                }
                base::return(NULL)
            }
            if( ! base::is.call(x = expr)){
                base::return(base::list(expr)) # constant (e.g., 1, "a" or NULL)
            }
            tempo_head_name <- "" # name of the called function, "" if the function is not written as a name (e.g., pkg::fun or f() in f()(x))
            if(base::is.name(x = expr[[1]])){
                tempo_head_name <- base::as.character(x = expr[[1]])
            }
            tempo_fun <- NULL
            if(( ! tempo_head_name %in% base::c("", "~", "function", "quote", "bquote", "expression", "substitute", "alist", ".Internal")) && name_copiable(name = tempo_head_name, mode = "function", fun_env = fun_env, fun_args = fun_args, fun_local = fun_local)){
                tempo_fun <- object_get(name = tempo_head_name, envir = fun_env, mode = "function")[[1]]
            }
            if(base::is.null(x = tempo_fun) || (( ! base::is.primitive(x = tempo_fun)) && base::any(base::all.names(expr = base::body(fun = tempo_fun), functions = TRUE, max.names = -1L, unique = TRUE) %in% caller_dep_funs, na.rm = TRUE))){
                base::return(NULL)
            }
            # arguments of the call
            tempo_pos <- base::seq_along(along.with = expr)[-1]
            if(tempo_head_name %in% base::c("$", "@", "::", ":::")){
                tempo_pos <- tempo_pos[base::seq_len(length.out = base::as.integer(x = tempo_head_name %in% base::c("$", "@")))] # left operand of $ and @ only
            }
            for(i9 in base::seq_along(along.with = tempo_pos)){
                if( ! base::identical(x = expr[[tempo_pos[i9]]], y = base::quote(expr = ), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                    tempo_arg <- default_copy(expr = expr[[tempo_pos[i9]]], fun_env = fun_env, fun_args = fun_args, fun_local = fun_local)
                    if(base::is.null(x = tempo_arg)){
                        base::return(NULL)
                    }
                    expr[tempo_pos[i9]] <- tempo_arg
                }
            }
            # function called by its name (not an operator like + or [, nor if): written with its package and its arguments named
            if(base::make.names(names = tempo_head_name, unique = FALSE, allow_ = TRUE) == tempo_head_name){
                tempo_ns <- base::asNamespace(ns = "base", base.OK = TRUE) # namespace of a primitive function, whose environment is NULL
                if( ! base::is.primitive(x = tempo_fun)){
                    tempo_ns <- base::topenv(envir = base::environment(fun = tempo_fun), matchThisEnv = base::getOption(x = "topLevelEnvironment", default = NULL)) # namespace where the function is defined, or other top environment (e.g., the global environment)
                }
                tempo_def <- fun_def(fun = tempo_fun)
                tempo_call <- base::tryCatch(expr = base::match.call(definition = tempo_def, call = expr, expand.dots = TRUE, envir = base::emptyenv()), error = base::identity, finally = ) # condition object, not a call, if the call does not match the function (e.g., unused argument)
                tempo_log <- base::is.function(x = tempo_def) && base::is.call(x = tempo_call) && base::isNamespace(ns = tempo_ns) && tempo_head_name %in% base::getNamespaceExports(ns = tempo_ns) && base::identical(x = base::getExportedValue(ns = tempo_ns, name = tempo_head_name), y = tempo_fun, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE) && base::all(base::setdiff(x = base::names(x = base::formals(fun = tempo_def, envir = tempo_ns)), y = "...") %in% base::names(x = tempo_call), na.rm = TRUE) && ! base::any(base::vapply(X = base::seq_along(along.with = tempo_call)[-1], FUN = function(k){base::identical(x = tempo_call[[k]], y = base::quote(expr = ), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)}, FUN.VALUE = base::logical(length = 1L), USE.NAMES = FALSE), na.rm = TRUE) # arguments of the function available (not for base::return()), call matched, function exported by its namespace (pkg::fun gives this function), all its arguments other than ... written, and no empty value (index based because an empty value cannot be passed as an argument of a function)
                if( ! tempo_log){
                    base::return(NULL)
                }
                expr <- tempo_call
                expr[[1]] <- base::call(name = "::", base::as.name(x = base::getNamespaceName(ns = tempo_ns)), expr[[1]])
            }
            base::return(base::list(expr))
        }
        for(i2 in base::seq_along(along.with = col1)){
            if(col3[i2] != reserved_words){
                # check if internal function
                # only for the calls written without package: a call written pkg::fun() or pkg:::fun() never calls an internal function of x, i.e., a function created inside x, an argument of x or of a function defined in x, or x itself (e.g., base::c(v, 1) when x creates a function named c, or base::length(v) when x has an argument named length). And only inside the scope of the internal function (e.g., an argument t of a function defined inside x does not prevent the analysis of the t() calls outside this function)
                tempo_log <- col_internal[i2] & base::substr(x = code_for_col[i2], start = col4[i2] - 2, stop = col4[i2] - 1) != "::"
                if(col_skip[i2]){
                    # call inside a formula or inside quoted code, or function called by .Internal(): not analyzed, because the modeling functions use the text of the formulas (e.g., stats::offset(z) is not recognized by stats::terms()), the arguments of quote(), bquote(), expression(), alist() and substitute() not evaluated by R are code used as data (e.g., plotmath labels), and .Internal() the bare name of its function (base::.Internal(base::toupper(x)) fails)
                    col5 <- base::c(col5, "SKIPPED")
                    col6 <- base::c(col6, "")
                    col7 <- base::c(col7, "")
                    col8 <- base::c(col8, "")
                }else if(tempo_log){
                    col5 <- base::c(col5, "INTERNAL_FUNCTION")
                    col6 <- base::c(col6, "")
                    col7 <- base::c(col7, "")
                    col8 <- base::c(col8, "")
                # end check if internal function
                }else{
                    # check if the function exists
                    tempo_cat_fun_exists <- base::paste0(
                        error_text_start, 
                        "CANNOT GET THE ARGUMENTS OF A FUNCTION THAT IS NOT ASSOCIATED TO ITS PACKAGE IN LINE ", 
                        col1[i2], 
                        ":\n\n", 
                        base::paste0(base::paste0(base::substr(x = code_for_col[i2], start = 1, stop = col4[i2] - 1), col3[i2], collapse = NULL, recycle0 = FALSE), collapse = "\n", recycle0 = FALSE),
                        base::ifelse(
                            test = col2[i2] %in% out$all_basic_funs,
                            yes = base::paste0("\n\nPLEASE, RUN saferDev::colons_check(", arg_user_setting$x, ") FIRST,\nAND RERUN saferDev::all_args_here(", arg_user_setting$x, ").", collapse = NULL, recycle0 = FALSE), # basic R function, whose package is added by saferDev::colons_check()
                            no = base::paste0("\n\nPLEASE, WRITE THE PACKAGE OF THIS FUNCTION BEFORE ITS NAME (E.G., pkg::", col2[i2], "()) IN ", arg_user_setting$x, ",\nAND RERUN saferDev::all_args_here(", arg_user_setting$x, ").\nOF NOTE, saferDev::colons_check() ONLY ADDS THE PACKAGE OF THE BASIC R FUNCTIONS.", collapse = NULL, recycle0 = FALSE)
                        ),
                        collapse = NULL,
                        recycle0 = FALSE
                    )
                    if(col4[i2] <= 3){
                        base::stop(base::paste0("\n\n================\n\n", tempo_cat_fun_exists, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    }
                    tempo_string <- base::substr(x = code_for_col[i2], start = col4[i2] - 2, stop = col4[i2] - 1)
                    if(tempo_string != "::"){
                        base::stop(base::paste0("\n\n================\n\n", tempo_cat_fun_exists, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    }
                    tempo_string <- base::substr(x = code_for_col[i2], start = 1, stop = col4[i2] - 1)
                    tempo_package_name_colons <- .extract_all_fun_names(
                        text = tempo_string, 
                        pattern = "[a-zA-Z][a-zA-Z0-9.]*:{2,3}$",
                        lib_path = lib_path, 
                        error_text = embed_error_text
                    )$string # before 
                    saferDev::is_function_here(
                        fun = base::paste0(tempo_package_name_colons, col2[i2], collapse = NULL, recycle0 = FALSE), 
                        lib_path = lib_path, 
                        safer_check = FALSE, 
                        error_text = embed_error_text
                    ) # check that exists
                    tempo_package_name <- base::sub(pattern =  ":+$", replacement = "", x = tempo_package_name_colons, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
                    # end check if the function exists
                    # recovering default args of the function
                    tempo_log <- base::exists(x = col2[i2], where = -1, envir = base::getNamespace(name = tempo_package_name), frame = NULL, mode = "any", inherits = FALSE)
                    if(tempo_log){
                        tempo_fun2 <- base::get(x = col2[i2], envir = base::asNamespace(ns = tempo_package_name, base.OK = TRUE), mode = "function", inherits = FALSE, pos = -1L)
                    }else if(base::grepl(x = tempo_package_name_colons, pattern = "[^:]::$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
                        tempo_fun2 <- base::getExportedValue(ns = tempo_package_name, name = col2[i2]) # pkg::fun of a function exported by the package but defined in another one (e.g., graphics::plot, defined in base): absent from the namespace of the package, but in its exports, as checked by saferDev::is_function_here() above
                    }else{
                        # nocov start
                        # codecov inactivated because unreachable: saferDev::is_function_here() above has checked that the function is in the namespace of the package (pkg:::fun) or in its exports (pkg::fun)
                        tempo_cat <- base::paste0("INTERNAL ERROR 4 IN ", intern_error_text_start,"CANNOT GET THE ARGUMENTS OF A FUNCTION THAT IS NOT CORRECTLY ASSOCIATED TO ITS PACKAGE IN LINE ", col1[i2], ":\n\n", base::paste0(base::paste0(base::substr(x = code_for_col[i2], start = 1, stop = col4[i2] - 1), col3[i2], collapse = NULL, recycle0 = FALSE), collapse = "\n", recycle0 = FALSE), "\n\nPLEASE, RUN utils::getAnywhere('", col2[i2], "')$where FIRST,\nCORRECT THE LINE ", col1[i2], " IN ", arg_user_setting$x, ",\nSAVE AND REIMPORT ", arg_user_setting$x, "\nAND RERUN saferDev::all_args_here(", arg_user_setting$x, ").", intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                        # nocov end
                    }
                    if(base::is.primitive(x = tempo_fun2)){
                        tempo_all_args <- base::as.list(x = base::suppressWarnings(expr = base::formals(fun = fun_def(fun = tempo_fun2), envir = base::asNamespace(ns = tempo_package_name, base.OK = TRUE)), classes = "warning")) # see fun_def() above for base::isS4(). Warning when base::args() returns NULL (e.g., base::return()): no argument
                        if(base::all(base::typeof(x = tempo_fun2) %in% base::c("special", "symbol"), na.rm = TRUE)){
                            if(base::length(x = tempo_all_args) == 0){
                                arg_full <- NULL # all args of the function with default values
                            }else{
                                arg_full <- tempo_all_args # convert pairlist into list
                            }
                        }else{
                            arg_full <- tempo_all_args
                        }
                    }else{
                        arg_full <- base::as.list(x = base::formals(fun = col2[i2], envir = base::asNamespace(ns = tempo_package_name, base.OK = TRUE))) # all the argument of the function in col2[i2] with default values # convert pairlist into list
                    }
                    # end recovering default args of the function
                    if(col2[i2] %in% skipped_other){
                        col5 <- base::c(col5, "SKIPPED")
                        col6 <- base::c(col6, "")
                        col7 <- base::c(col7, "")
                        col8 <- base::c(col8, "")
                    }else if(base::is.null(x = arg_full)){
                        col5 <- base::c(col5, "NO_ARGS")
                        col6 <- base::c(col6, "")
                        col7 <- base::c(col7, "")
                        col8 <- base::c(col8, "")
                    }else{
                        # all arguments of the function with default value in col5
                        # an argument without default value is the empty symbol (not any symbol: a default value can be a symbol, e.g., object = nm in stats::setNames())
                        # the lines of a long deparsed default value are joined (e.g., conf.args of stats::qqplot()), otherwise sapply() returns several strings for this argument
                        tempo <- base::sapply(X = arg_full, FUN = function(x){base::paste0(base::ifelse(test = base::identical(x = x, y = base::quote(expr = ), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE), yes = "", no = " = "), base::paste0(base::deparse(expr = x, width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)}, simplify = TRUE, USE.NAMES = TRUE)
                        tempo <- base::paste(base::paste0(base::names(x = tempo), tempo, collapse = NULL, recycle0 = FALSE), collapse = ", ", recycle0 = FALSE, sep = " ")
                        col5 <- base::c(col5, tempo)
                        # end all arguments of the function with default value in col5
                        # default values copied into the call, or replaced by an empty value (e.g., envir = ), meaning that the default value is used but cannot be fixed
                        # a default value is copied only if it gives the same result in the frame of x (see default_copy() above): not if it depends on another argument (e.g., envir = as.environment(pos) in utils::getFromNamespace()), on a variable created in the body of the function, on the frame where it is evaluated (e.g., parent.frame()), or on an object that x does not find, or finds differently (e.g., an internal function of a package), because it would be evaluated in the frame of x and not in the one of the called function
                        tempo_fun_env <- base::environment(fun = tempo_fun2)
                        if(base::is.null(x = tempo_fun_env)){
                            tempo_fun_env <- base::asNamespace(ns = tempo_package_name, base.OK = TRUE) # primitive functions have no environment
                        }
                        tempo_fun_local <- assign_walk(expr = base::body(fun = tempo_fun2)) # names assigned in the code of the called function (NULL body for a primitive function)
                        # default values of the arguments tested with missing() by the called function: not copied (see missing_nargs_get() above)
                        tempo_first_arg <- base::ifelse(test = base::length(x = arg_full) > 0, yes = base::names(x = arg_full)[1], no = "") # first argument of the called function ("" if no argument)
                        tempo_code_check <- missing_nargs_get(fun = tempo_fun2, fun_name = col2[i2], pack_name = tempo_package_name, first_arg = tempo_first_arg)
                        arg_full_fill <- arg_full
                        for(i9 in base::seq_along(along.with = arg_full)){
                            if(base::names(x = arg_full)[i9] %in% tempo_code_check$missing){
                                arg_full_fill[i9] <- base::list(base::quote(expr = ))
                            }else if(base::is.call(x = arg_full[[i9]]) | (base::is.name(x = arg_full[[i9]]) & ! base::identical(x = arg_full[[i9]], y = base::quote(expr = ), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE))){ # constants (e.g., 1, "a" or NULL) and empty values (arguments without default value) kept as they are
                                tempo_copy <- default_copy(expr = arg_full[[i9]], fun_env = tempo_fun_env, fun_args = base::names(x = arg_full), fun_local = tempo_fun_local)
                                if(base::is.null(x = tempo_copy)){
                                    tempo_copy <- base::list(base::quote(expr = ))
                                }
                                arg_full_fill[i9] <- tempo_copy
                            }
                        }
                        # end default values copied into the call
                        # arguments: replacement of all the commas between quotes
                        tempo_col3 <- col3[i2]
                        tempo <- .in_quotes_replacement(
                            string = tempo_col3, 
                            pattern = ",", 
                            no_regex_pattern = ",", 
                            replacement = " ", 
                            perl = TRUE, 
                            lib_path = lib_path, 
                            error_text = embed_error_text
                        )
                        tempo_col3 <- tempo$string
                        pos_rep2 <- tempo$pos # replaced positions in obs_args
                        # end arguments: replacement of all the commas between quotes
                        # arguments: replacement of all the commas inside () of subfunctions
                        if(base::length(x = middle_bracket_open_in_col3[[i2]]) > 0){
                            if(base::length(x = middle_bracket_open_in_col3[[i2]]) != base::length(x = middle_bracket_close_in_col3[[i2]])){
                                # nocov start
                                # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
                                tempo_cat <- base::paste0("INTERNAL ERROR 5 IN ", intern_error_text_start, "middle_bracket_open_in_col3 AND middle_bracket_close_in_col3 MUST HAVE THE SAME LENGTH IN LOOP ", i2, "\n\nmiddle_bracket_open_in_col3 (", base::length(x = middle_bracket_open_in_col3), "):\n", base::paste0(middle_bracket_open_in_col3, collapse = " ", recycle0 = FALSE), "\n\nmiddle_bracket_close_in_col3 (", base::length(x = middle_bracket_close_in_col3), "):\n", base::paste0(middle_bracket_close_in_col3, collapse = " ", recycle0 = FALSE), "\n\ni2:\n", i2, intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                                # nocov end
                            }
                            for(i6 in base::seq_along(along.with = middle_bracket_open_in_col3[[i2]])){
                                tempo <- .in_parenthesis_replacement(
                                    string = tempo_col3, 
                                    pattern = ",", 
                                    no_regex_pattern = ",", 
                                    replacement = " ", 
                                    perl = TRUE, 
                                    open_pos = middle_bracket_open_in_col3[[i2]][i6], 
                                    close_pos = middle_bracket_close_in_col3[[i2]][i6], 
                                    lib_path = lib_path, 
                                    error_text = embed_error_text
                                )
                                tempo_col3 <- tempo$string
                                pos_rep2 <- base::c(pos_rep2, tempo$pos) # replaced positions in obs_args
                            }
                        }
                        # end arguments: replacement of all the commas inside () of subfunctions
                        # arguments: replacement of all the commas inside [] and {} (e.g., m[1, 2]), which are not argument separators
                        if(base::grepl(x = tempo_col3, pattern = "[[{]", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
                            tempo_bracket <- tempo_col3
                            for(i6 in base::seq_len(length.out = 4)){
                                tempo_bracket <- .in_quotes_replacement(
                                    string = tempo_bracket,
                                    pattern = base::c("\\[", "\\]", "\\{", "\\}")[i6],
                                    no_regex_pattern = base::c("[", "]", "{", "}")[i6],
                                    replacement = " ",
                                    perl = TRUE,
                                    lib_path = lib_path,
                                    error_text = embed_error_text
                                )$string # brackets between quotes replaced by spaces
                            }
                            tempo_chars <- base::strsplit(x = tempo_bracket, split = "", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]]
                            tempo_depth <- base::cumsum(x = tempo_chars %in% base::c("[", "{")) - base::cumsum(x = tempo_chars %in% base::c("]", "}")) # number of [ and { not closed at each position
                            tempo_comma_pos <- base::which(x = tempo_chars == "," & tempo_depth > 0, arr.ind = FALSE, useNames = TRUE)
                            for(i6 in base::seq_along(along.with = tempo_comma_pos)){
                                base::substr(x = tempo_col3, start = tempo_comma_pos[i6], stop = tempo_comma_pos[i6]) <- " "
                            }
                            pos_rep2 <- base::c(pos_rep2, tempo_comma_pos) # replaced positions in obs_args
                        }
                        # end arguments: replacement of all the commas inside [] and {}
                        # recovering obs arguments
                        obs_args <- base::sub(pattern =  base::paste0("^", col2[i2], "[\\s\\r\\n]*\\(", collapse = NULL, recycle0 = FALSE), replacement = "", x = tempo_col3, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) # removal of function name and (
                        obs_args <- base::sub(pattern =  "\\)$", replacement = "", x = obs_args, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # removal of trailing )
                        # end recovering obs arguments
                        # splitting the arguments using commas
                        tempo_split <- base::strsplit(x = obs_args, split = ",", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]] # separation of args
                        # tempo_split <- gsub(pattern = "^[\\s;]+", replacement = "", x = tempo_split) # removing leading ; and space, ; because of line: fun_1_line <- base::paste(out$code, collapse = ";")
                        # tempo_split <- gsub(pattern = "[\\s;]+$", replacement = "", x = tempo_split) # removing trailing ; and space, ; because of line: fun_1_line <- base::paste(out$code, collapse = ";")
                        # end splitting the arguments using commas
                        # rewriting the commas inside args
                        if( ! base::is.null(x = pos_rep2)){
                            # resseting the pos of the removed commas to fit obs_args
                            pos_rep2 <- pos_rep2 - base::nchar(x = col2[i2], type = "chars", allowNA = FALSE, keepNA = NA) - 1 # -1 for the opening (
                            if(base::any(pos_rep2 <= 0, na.rm = TRUE)){
                                # nocov start
                                # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
                                tempo_cat <- base::paste0("INTERNAL ERROR 6 IN ", intern_error_text_start,"POSITIONS OF REMOVED COMMAS CANOT  OR LESS\n\npos_rep2 (", base::length(x = pos_rep2), "):\n", base::paste0(pos_rep2, collapse = " ", recycle0 = FALSE), "\n\nARGUMENT STRING obs_args:\n", base::paste0(obs_args, collapse = " ", recycle0 = FALSE), "\n\ni2:\n", i2, intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                                # nocov end
                            }
                            # end resseting the pos of the removed commas to fit obs_args
                            for(i6 in base::seq_along(along.with = tempo_split)){
                                tempo_log <- pos_rep2 >= 1 & pos_rep2 <= base::nchar(x = tempo_split[i6], type = "chars", allowNA = FALSE, keepNA = NA)
                                if(base::any(base::is.na(x = tempo_log), na.rm = TRUE)){
                                    # nocov start
                                    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
                                    tempo_cat <- base::paste0("INTERNAL ERROR 7 IN ", intern_error_text_start, "tempo_log CONTAINS NA:\n", base::paste0(tempo_log, collapse = " ", recycle0 = FALSE), intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                                    # nocov end
                                }
                                if(base::any(tempo_log, na.rm = TRUE)){
                                    for(i7 in base::seq_along(along.with = tempo_log)){
                                        if(tempo_log[i7]){
                                            base::substr(x = tempo_split[i6], start = pos_rep2[i7], stop = pos_rep2[i7]) <- ","
                                        }
                                    }
                                }
                                pos_rep2 <- pos_rep2 - base::nchar(x = tempo_split[i6], type = "chars", allowNA = FALSE, keepNA = NA) - 1 # -1 because of the comma that separates each tempo_split
                            }
                        }
                        # end rewriting the commas inside args
                        # empty arguments written without space (e.g., base::matrix(, 2) or base::sum(x,,1)): written " ", like the empty arguments written with a space (e.g., base::sum(x, , 1)), so that the proposal is shown but not inserted (see no_insert below). The last empty argument of a call written without space (e.g., base::sum(x,)) is not returned by base::strsplit(), thus added. Done after the rewriting of the commas, whose positions depend on the lengths of the elements of tempo_split
                        empty_written_log <- base::any(base::trimws(x = tempo_split, which = "both", whitespace = "[ \\t\\r\\n]") == "", na.rm = TRUE) | base::grepl(x = obs_args, pattern = ",[[:space:]]*$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
                        tempo_split[base::trimws(x = tempo_split, which = "both", whitespace = "[ \\t\\r\\n]") == ""] <- " "
                        if(base::grepl(x = obs_args, pattern = ",$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
                            tempo_split <- base::c(tempo_split, " ")
                        }
                        # end empty arguments written without space
                        # call on the right side of a pipe (|>, or %>%, %<>%, %T>% and %!>% of magrittr), which receives the value of the pipe as first argument (e.g., x |> base::rep(3)), or through the _ or . placeholder: pipe operator just before pkg::fun in the code of x written on a single line (fun_1_line, position of the function name as in cum_nchar_code_line above). The native pipe |> is only written if x has source references (otherwise, the parser has rewritten x |> f(y) into f(x, y)). %$% and the other %op% operators do not pass their left value as an argument
                        tempo_pos_in_fun_1_line <- col4[i2] + base::sum(base::nchar(x = code[base::seq_len(length.out = col1[i2] - 1)], type = "chars", allowNA = FALSE, keepNA = NA) + 1, na.rm = FALSE)
                        pipe_log <- base::grepl(x = base::substr(x = fun_1_line, start = 1, stop = tempo_pos_in_fun_1_line - 1), pattern = "(\\|>|%>%|%<>%|%T>%|%!>%)[[:space:]]*[a-zA-Z][a-zA-Z0-9.]*:{2,3}$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
                        # ... written as an argument of the call (e.g., utils::read.csv(text = txt, ...)), whose content can match any argument by its name (..1 or ..2 are single values, not concerned)
                        dots_written_log <- base::any(base::trimws(x = tempo_split, which = "both", whitespace = "[ \\t\\r\\n]") == "...", na.rm = TRUE)
                        # working on each observed arg
                        arg_full_names <-  base::names(x = arg_full) # NULL if arg_full is empty list()
                        three_dots_log <- if(base::length(x = arg_full) > 0){
                            arg_full_names == "..."
                        }else{
                            FALSE
                        }
                        # checking 
                        if(base::length(x = tempo_split) > base::length(x = arg_full_names) & ! base::any(three_dots_log, na.rm = TRUE)){
                            # call written with more arguments than its function has: not analyzed, with a warning. Such a call fails in R (unused argument), except if it is the target of an assignment that calls a replacement function with more arguments (e.g., base::body(f, envir = e) <- v calls body<-, which has an envir argument, contrary to body()). The commas between quotes and inside (), [] and {} are not argument separators (replaced above)
                            warn_count <- warn_count + 1
                            tempo_warn <- base::paste0("(", warn_count, ") IN LINE ", col1[i2], ", THE CALL\n", tempo_package_name_colons, col3[i2], "\nHAS MORE ARGUMENTS THAN ITS FUNCTION, WHOSE ARGUMENTS ARE: ", base::ifelse(test = base::length(x = arg_full_names) == 0, yes = "NONE", no = base::paste0(arg_full_names, collapse = ", ", recycle0 = FALSE)), ".\nTHIS CALL IS NOT ANALYZED. IT FAILS IN R, EXCEPT IF IT IS THE TARGET OF AN ASSIGNMENT CALLING A REPLACEMENT FUNCTION THAT HAS THESE ARGUMENTS (E.G., base::body(f, envir = e) <- v CALLS body<-).", collapse = NULL, recycle0 = FALSE)
                            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                            col6 <- base::c(col6, "")
                            col7 <- base::c(col7, "")
                            col8 <- base::c(col8, "")
                            next
                        }
                        # function whose default values are all NA (e.g., base::Sys.umask()): not analyzed, with a warning, because the arg_full argument of .all_args_here_fill() cannot be made of NA only
                        if(base::length(x = arg_full_fill) > 0 & base::all(base::suppressWarnings(expr = base::is.na(x = arg_full_fill), classes = "warning"), na.rm = TRUE)){
                            warn_count <- warn_count + 1
                            tempo_warn <- base::paste0("(", warn_count, ") IN LINE ", col1[i2], ", THE CALL\n", tempo_package_name_colons, col3[i2], "\nIS NOT ANALYZED, BECAUSE ALL THE ARGUMENTS OF ITS FUNCTION HAVE NA AS DEFAULT VALUE: ", base::paste0(arg_full_names, collapse = ", ", recycle0 = FALSE), ".", collapse = NULL, recycle0 = FALSE)
                            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                            col6 <- base::c(col6, "")
                            col7 <- base::c(col7, "")
                            col8 <- base::c(col8, "")
                            next
                        }
                        # end checking
                        tempo_out <- .all_args_here_fill(
                            arg_full = arg_full_fill,
                            arg_full_names = arg_full_names,
                            tempo_split = tempo_split,
                            three_dots_log = three_dots_log,
                            pipe_log = pipe_log,
                            i2 = i2,
                            col1_i2 = col1[i2],
                            col2_i2 = col2[i2],
                            arg_user_setting_x = arg_user_setting$x, 
                            warn = warn,
                            warn_count = warn_count,
                            lib_path = lib_path, 
                            error_text = embed_error_text
                        )
                        col6 <- base::c(col6, tempo_out$col6)
                        col7 <- base::c(col7, tempo_out$col7)
                        col8 <- base::c(col8, tempo_out$col8)
                        # end working on each observed arg
                        # proposals that must not be inserted into the modified function (listed in the CORRECTIONS NOT APPLIED message), see missing_nargs_get() above:
                        # - call on the right side of a pipe, whose first argument (the value of the pipe) is not written in the call (see pipe_log above)
                        # - call that forwards ..., whose content can match the added arguments (see dots_written_log above)
                        # - call with an empty argument (see empty_written_log above)
                        # - call of a function that uses nargs()
                        # - first argument of an S3 generic added to the call, which can change the method used (e.g., x = in stats::t.test(formula = y ~ g, data = d), which fails)
                        # - first argument of an S3 generic written without its name, while a method registered for this generic names it differently (e.g., stats::t.test(y ~ g, data = d) proposed as stats::t.test(x = y ~ g, data = d), which fails, t.test.formula() having no x argument)
                        # - argument added that a method without ... does not have (e.g., digits = 0 in base::round(x = t), which fails for date-times, round.POSIXt() having no digits argument)
                        # - argument added that a method registered for the S3 generic does not have, but matches partially (e.g., y = in graphics::plot(x = t, y = ), which fails for tables, y matching ylim and ylab of plot.table())
                        # - empty value proposed in a call of a builtin primitive function, which fails on an empty argument (e.g., PACKAGE = in base::.Call())
                        tempo_miss_args <- base::tryCatch(
                            expr = {
                                base::as.list(x = base::str2lang(s = base::paste0("f(", tempo_out$col7, ")", collapse = NULL, recycle0 = FALSE)))[-1]
                            },
                            error = function(e){base::list()}, # MISSING_ARGS that cannot be parsed (e.g., a function written without name, whose body has several lines, joined into one line in MISSING_ARGS): not inserted anyway (see .all_args_insert())
                            finally =
                        )
                        tempo_miss_names <- base::names(x = tempo_miss_args) # arguments written by the proposal (NULL if none)
                        tempo_dispatch_log <- FALSE
                        tempo_first_name_log <- FALSE
                        if(tempo_code_check$dispatch == TRUE & tempo_first_arg %in% tempo_miss_names){
                            tempo_dispatch_log <- base::identical(x = tempo_miss_args[[tempo_first_arg]], y = arg_full_fill[[1]], num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE) # the first argument proposed with its default value or an empty value is added to the call. Otherwise, it is a value written without name, which only gets its name
                            tempo_first_name_log <- ( ! tempo_dispatch_log) & tempo_code_check$first_arg_differs # value written without name that gets the name of the first argument of the generic, while a method names it differently
                        }
                        tempo_method_log <- base::vapply(X = tempo_code_check$method_args, FUN = function(y){ ! base::all(tempo_miss_names %in% y, na.rm = TRUE)}, FUN.VALUE = base::logical(length = 1L), USE.NAMES = FALSE)
                        # for each method registered for the S3 generic: TRUE if an added argument is not an argument of the method, but starts the name of arguments of the method placed before its ..., the only ones that R matches partially (e.g., y = matches ylim and ylab of plot.table(), so that graphics::plot(x = t, y = ) fails for tables)
                        tempo_partial_log <- base::vapply(
                            X = tempo_code_check$table_method_args,
                            FUN = function(y){
                                tempo_before_dots <- y[base::seq_len(length.out = base::match(x = "...", table = base::c(y, "..."), nomatch = NA_integer_, incomparables = NULL) - 1L)] # arguments of the method placed before ..., all of them if the method has no ...
                                base::any(base::vapply(X = tempo_miss_names[ ! tempo_miss_names %in% base::c(y, "")], FUN = function(z){base::any(base::startsWith(x = tempo_before_dots, prefix = z), na.rm = TRUE)}, FUN.VALUE = base::logical(length = 1L), USE.NAMES = FALSE), na.rm = TRUE)
                            },
                            FUN.VALUE = base::logical(length = 1L),
                            USE.NAMES = FALSE
                        )
                        tempo_empty_log <- base::vapply(X = base::seq_along(along.with = tempo_miss_args), FUN = function(k){base::identical(x = tempo_miss_args[[k]], y = base::quote(expr = ), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)}, FUN.VALUE = base::logical(length = 1L), USE.NAMES = FALSE) # index based because an empty value cannot be passed as an argument of a function
                        if(pipe_log == TRUE){
                            no_insert[i2] <- "pipe"
                        }else if(dots_written_log == TRUE){
                            no_insert[i2] <- "dots"
                        }else if(empty_written_log == TRUE){
                            no_insert[i2] <- "empty_arg"
                        }else if(tempo_code_check$nargs == TRUE){
                            no_insert[i2] <- "nargs"
                        }else if(tempo_dispatch_log == TRUE){
                            no_insert[i2] <- "generic_dispatch"
                        }else if(tempo_first_name_log == TRUE){
                            no_insert[i2] <- "generic_first_arg"
                        }else if(base::any(tempo_method_log, na.rm = TRUE)){
                            no_insert[i2] <- "method_args"
                        }else if(base::any(tempo_partial_log, na.rm = TRUE)){
                            no_insert[i2] <- "method_partial"
                        }else if(base::typeof(x = tempo_fun2) == "builtin" & base::any(tempo_empty_log, na.rm = TRUE)){
                            no_insert[i2] <- "builtin_empty"
                        }
                        # args with empty value proposed in the STATUS column (the ones already written empty in the code are accepted and not reported). Only the proposals inserted into the modified function are kept below, after .all_args_insert()
                        tempo_call <- base::ifelse(test = base::grepl(x = tempo_out$col8, pattern = base::paste0("^", col2[i2], "\\(", collapse = NULL, recycle0 = FALSE), ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = tempo_out$col8, no = col3[i2]) # col3[i2] if col8 is GOOD or a warning about non fully written arg names
                        tempo_call <- base::tryCatch(expr = base::str2lang(s = tempo_call), error = function(e){NULL}, finally = ) # NULL if the call cannot be parsed (e.g., multi-line call): not considered
                        if(base::is.call(x = tempo_call) & ! col2[i2] %in% "quote"){ # quote(expr = ) not considered, because it is the writing of an empty value, not a potential flaw
                            tempo_call_args <- base::as.list(x = tempo_call)[-1]
                            if(base::length(x = tempo_call_args) > 0 & ! base::is.null(x = base::names(x = tempo_call_args))){
                                tempo_log <- base::vapply(X = base::seq_along(along.with = tempo_call_args), FUN = function(k){base::identical(x = tempo_call_args[[k]], y = base::quote(expr = ), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)}, FUN.VALUE = base::logical(length = 1L), USE.NAMES = TRUE) # index based because an empty value cannot be passed as an argument of a function
                                tempo_log <- tempo_log & base::names(x = tempo_call_args) %in% base::names(x = arg_full) & base::names(x = tempo_call_args) %in% base::strsplit(x = tempo_out$col6, split = ", ", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]] # only the empty args added by the proposal (MISSING_ARG_NAMES): an empty value written in the code (e.g., new = , or envir = )) is accepted as is and not reported
                                if(base::any(tempo_log, na.rm = TRUE)){
                                    tempo_empty <- base::sapply(
                                        X = base::names(x = tempo_call_args)[tempo_log],
                                        FUN = function(y){
                                            if(base::identical(x = arg_full[[y]], y = base::quote(expr = ), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                                                base::paste0(y, " (NO DEFAULT VALUE)", collapse = NULL, recycle0 = FALSE)
                                            }else{
                                                base::paste0(y, " (DEFAULT: ", base::paste0(base::deparse(expr = arg_full[[y]], width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = " ", recycle0 = FALSE), base::ifelse(test = y %in% tempo_code_check$missing, yes = ", TESTED BY missing()", no = ""), ")", collapse = NULL, recycle0 = FALSE)
                                            }
                                        },
                                        simplify = TRUE,
                                        USE.NAMES = FALSE
                                    )
                                    empty_args <- base::c(empty_args, base::paste0("line ", col1[i2], ": ", tempo_package_name_colons, col2[i2], "(): ", base::paste0(tempo_empty, collapse = ", ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE))
                                    empty_keys <- base::c(empty_keys, base::paste0("line ", col1[i2], ": ", tempo_out$col8, collapse = NULL, recycle0 = FALSE)) # same writing as the elements of the inserted vector returned by .all_args_insert()
                                }
                            }
                        }
                        # end args with empty value proposed in the STATUS column
                    }
                }
            }else{
                col5 <- base::c(col5, "")
                col6 <- base::c(col6, "")
                col7 <- base::c(col7, "")
                col8 <- base::c(col8, "")
            }
            # if(i2 != length(col8)){stop(paste0("caca ", i2))}
        }
        if(base::all(col8 %in%base::c("", "GOOD"), na.rm = FALSE)){
            tempo_cat <- base::paste0("INSIDE ", base::as.character(x = out$arg_user_setting$x), "(), EVERYTHING SEEMS CLEAN.", collapse = NULL, recycle0 = FALSE)
            if(table_export == TRUE){
                tempo_cat <- base::paste0("RESULT EXPORTED IN:\n", base::normalizePath(path = out_path, winslash = "/", mustWork = FALSE), "\nBUT ", tempo_cat, collapse = NULL, recycle0 = FALSE)
            }
            tempo_cat <- base::paste0("AFTER RUNNING ", function_name, " OF THE ", package_name, " PACKAGE:\n", tempo_cat, collapse = NULL, recycle0 = FALSE)
            base::on.exit(expr = base::message(base::paste0("\n", tempo_cat, "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE), add = TRUE, after = TRUE)
        }else{
            tempo_cat <- base::paste0("INSIDE ", base::as.character(x = out$arg_user_setting$x), "(), ARGUMENTS ARE MISSING.", collapse = NULL, recycle0 = FALSE)
            if(table_export == TRUE){
                tempo_cat <- base::paste0(tempo_cat, "\nRESULT EXPORTED IN:\n", base::normalizePath(path = out_path, winslash = "/", mustWork = FALSE), collapse = NULL, recycle0 = FALSE)
            }
            base::on.exit(expr = base::message(base::paste0("\n", tempo_cat, "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE), add = TRUE, after = TRUE)
        }
        # table of the arguments
        output <- base::data.frame(LINE_NB = col1, FUN_NAME = col2, FUN_ARGS = col3, FUN_POS = col4, DEF_ARGS = col5, MISSING_ARG_NAMES = col6, MISSING_ARGS = col7, STATUS = col8, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
        if(table_export == FALSE){
            # the table is returned as a message with on.exit(), after the EVERYTHING SEEMS CLEAN or ARGUMENTS ARE MISSING message registered above, and before the ARGUMENTS WITH EMPTY VALUE, CORRECTIONS NOT APPLIED and MODIFIED CODE EXPORTED IN messages registered below
            tempo_cat_table <- base::paste0(utils::capture.output(output, file = NULL, append = FALSE, type = "output", split = FALSE), collapse = "\n", recycle0 = FALSE)
            base::on.exit(expr = base::message(base::paste0(tempo_cat_table, "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE), add = TRUE, after = TRUE)
        }
        # modification of the submitted function with the missing arguments
        output_insert <- output
        output_insert$NO_INSERT <- no_insert # proposals not inserted, with their reason (see above)
        tempo_insert <- .all_args_insert(
            x = x,
            out = out,
            output = output_insert,
            lib_path = lib_path,
            error_text = embed_error_text
        )
        # recap of the args with empty value proposed in the STATUS column, i.e., the potential flaws of the code: default value that cannot be copied, or absence of value meaningful. Only for the proposals inserted into the modified function: the other ones are in the CORRECTIONS NOT APPLIED message below
        empty_args <- empty_args[empty_keys %in% tempo_insert$inserted]
        if(base::length(x = empty_args) > 0){
            tempo_cat_empty <- base::paste0(
                "ARGUMENTS WITH EMPTY VALUE (DEFAULT VALUE THAT CANNOT BE COPIED, OR NO DEFAULT VALUE):\n",
                base::paste0(empty_args, collapse = "\n", recycle0 = FALSE),
                "\nUSE \"= ,\" OR \"= )\" TO SEEK EACH CASE IN THE CODE, AND, IF POSSIBLE, TEST THAT THE DEFAULT VALUES ARE UNCHANGED, FOR INSTANCE:\ntestthat::expect_identical(object = base::formals(fun = utils::getFromNamespace)$envir, expected = base::quote(expr = as.environment(pos)))\n\n",
                collapse = NULL,
                recycle0 = FALSE
            )
            base::on.exit(expr = base::message(tempo_cat_empty, domain = NULL, appendLF = TRUE), add = TRUE, after = TRUE)
        }
        if(base::length(x = tempo_insert$left) > 0){ # corrections that could not be applied: added to the final message
            # tempo_cat_left and not tempo_cat: the on.exit() expressions are evaluated at exit, so that reusing tempo_cat would also replace the message of the "ARGUMENTS ARE MISSING" on.exit() registered above
            tempo_cat_left <- base::paste0(
                "CORRECTIONS NOT APPLIED:\n",
                base::paste0(tempo_insert$left, collapse = "\n", recycle0 = FALSE),
                "\n\n",
                collapse = NULL,
                recycle0 = FALSE
            )
            base::on.exit(expr = base::message(base::paste0(tempo_cat_left, collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE), add = TRUE, after = TRUE)
        }
        if(fun_export == TRUE & base::length(x = tempo_insert$inserted) > 0){
            tempo_fun_file <- base::paste0(out_dir, "/", tempo_fun_file_name, collapse = NULL, recycle0 = FALSE)
            if(overwrite == FALSE & base::file.exists(tempo_fun_file)){
                # nocov start
                # codecov inactivated because the file existence check has already been performed in the other checkings section: this branch can only be reached if the file was created between the two checks
                tempo_cat <- base::paste0(
                    error_text_start,
                    "FILE NAME ALREADY EXISTS AT THE INDICATED PATH:\n",
                    tempo_fun_file,
                    collapse = NULL,
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                # nocov end
            }
            # file content: assignment of the modified function, so that base::source()ing the file recreates it. tempo_insert$fun_txt is the bare "function(...) ..." writing of the modified function, so the assignment name is the name of the submitted function with _safer added at the end
            base::writeLines(text = base::paste0(tempo_fun_name, "_safer <- ", tempo_insert$fun_txt, collapse = NULL, recycle0 = FALSE), con = tempo_fun_file, sep = "\n", useBytes = FALSE)
            # the export message is printed with on.exit(), after the messages of the main code: it only claims an export that actually happened, because the file writing is performed just above and no on.exit() is registered when no modification is inserted
            base::on.exit(expr = base::message(base::paste0("MODIFIED CODE EXPORTED IN:\n", base::normalizePath(path = tempo_fun_file, winslash = "/", mustWork = FALSE), "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE), add = TRUE, after = TRUE)
        }
        if(table_export == TRUE){
            utils::write.table(x = output, file = out_path, row.names = FALSE, col.names = TRUE, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "", dec = ".", qmethod = base::c("escape", "double"), fileEncoding = "")
        }
        if(fun_export == FALSE & base::length(x = tempo_insert$inserted) > 0){
            output_fun <- tempo_insert$fun # the modified function is returned (not when exported)
        }
    }
    # end two new columns for arg proposal
    #### end main code

    #### warning output
    # must be before return()
    if( ! base::is.null(x = warn)){
        base::on.exit(
            expr = base::warning(
                base::paste0(
                    base::sub(pattern = "^ERROR IN ", replacement = "FROM ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE),
                    warn,
                    collapse = NULL,
                    recycle0 = FALSE
                ), call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL
            ), add = TRUE, after = FALSE
        ) # after = FALSE: the warning is emitted before the restoration of warning.length, registered in the warning initiation section, so that it is not truncated
    }
    # warning.length restored by the on.exit() of the warning initiation section
    #### end warning output

    #### output
    # the table is never returned: message if table_export = FALSE, file if table_export = TRUE
    if(base::is.null(x = output_fun)){
        base::return(base::invisible(x = NULL)) # NULL if no modification has been applied, or if the modified function is exported
    }else{
        base::return(output_fun) # the modified function
    }
    #### end output
}
