#' @title All Arguments Here
#' @description
#' Verify that all the functions used inside a function are written with all their arguments. For instance: \code{base::paste0(letters[1:2], collapse = NULL, recycle0 = FALSE)} and not \code{paste0(letters[1:2])}.
#' @param x Function name, written without quotes and brackets.
#' @param table_export Single logical value. Export the result data frame into a \code{.tsv} file? If \code{TRUE}, the data frame is not returned by the function but only exported.
#' @param fun_export Single logical value. Export the modified function into a \code{.R} file? If \code{TRUE}, the corrected code (missing arguments added into the detected function calls, as proposed in the \code{$STATUS} column) is not returned by the function but only exported into a file made of the name of the submitted function with \code{_safer} added at the end (e.g., \code{test_safer.R}). The file contains an assignment of the modified function: \code{test_safer <- function(...) ...}, so that \code{base::source()}ing it recreates the modified function. An error is returned if the file already exists in \code{out_path} and \code{overwrite} is \code{FALSE}, and if the tested function is not designated by a function name (like \code{test} or \code{package::test}), i.e., written by an inline definition (like \code{function(x)\{x\}}) or by a component writing (like \code{FUN_LIST$fun}). Ignored if no correction has been applied.
#' @param out_path Single character string indicating the absolute pathway of the folder where to export the data frame or the modified function. \code{out_path = "."} (default) means the R working directory set by the user. Ignored if \code{table_export} and \code{fun_export} are \code{FALSE}.
#' @param df_name Single character string indicating the name of the exported data frame file. Ignored if \code{table_export} is \code{FALSE}.
#' @param overwrite Single logical value. Overwrite potential files already existing in \code{out_path} (\code{df_name} for the table export, and the function name with \code{_safer} added at the end for the \code{fun_export})? Ignored if \code{table_export} and \code{fun_export} are \code{FALSE}.
#' @param safer_check Single logical value. Perform the "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) correct \code{lib_path} argument value 2) required functions and related packages effectively present in local R libraries and 3) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Warning: must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{c(lib_path, .libPaths())}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns 
#' A message saying whether arguments are missing or everything seems fine, and a data frame indicating the missing arguments (see the column descriptions below).
#' If \code{table_export} argument is \code{FALSE}, the data frame is returned as a message. If \code{TRUE}, it is exported as \code{df_name} in \code{out_path}.
#' If \code{fun_export} argument is \code{FALSE}, the modified function (an R function object rebuilt from the corrected code, with its comments and indentation preserved if the submitted function carries its source references, see the details section) is returned, or \code{NULL} (invisible) if no modification has been applied. If \code{TRUE}, the modified function is exported as \code{<FUNCTION_NAME>_safer.R} in \code{out_path} and \code{NULL} is returned (invisible), or nothing is exported if no modification has been applied.
#' Column descriptions: 
#' \itemize{
#'   \item \code{$LINE_NB}: the line number in the function code (starting at the \code{"<- function"} line, i.e., without counting the \code{#'} header lines).
#'   \item \code{$FUN_NAME}: the function name.
#'   \item \code{$FUN_ARGS}: the written arguments of \code{FUN_NAME}. \code{"NOT_CONSIDERED"} means that the function is between quotes or after \code{$}.
#'   \item \code{$FUN_POS}: the position of the first character of the function name in the \code{$LINE_NB} line.
#'   \item \code{$DEF_ARGS}: the defaults arguments of \code{FUN_NAME}. \code{"NO_ARGS"} means that the function has no arguments. \code{"INTERNAL_FUNCTION"} means that the function has been created inside the checked function. \code{"SKIPPED"} means that the function is not analyzed for the reason indicated in the "details" section.
#'   \item \code{$MISSING_ARG_NAMES}: the missing argument names in \code{FUN_ARGS}.
#'   \item \code{$MISSING_ARGS}: the missing arguments with their values in \code{FUN_ARGS}.
#'   \item \code{$STATUS}: either \code{"GOOD"}, meaning that all the arguments are already written, or a new proposal of arguments writing, or indicates if some arguments are not fully written (abbreviation is discouraged), or nothing.
#' }
#' 
#' An additional message \code{"EVERYTHING SEEMS CLEAN"} if the \code{$STATUS} column is only made of \code{""} and \code{"GOOD"}.
#' @details
#' More precisely, \code{all_args_here()} verifies that all the strings before an opening bracket \code{(} are written with all their arguments. Thus, it cannot check function names written without brackets, like in the \code{FUN} argument of some functions, e.g., \code{sapply(1:3, FUN = as.character)}.
#' 
#' The Perl regex used to detect a function name is: \code{"([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*\\("}.
#' 
#' Currently, \code{all_args_here()} cannot detect functions written between quotes, like \code{"+"()} or \code{"rownames<-"(x, "a")}.
#' 
#' Function names preceded by \code{$} are not considered.
#'  
#' The following R functions are skipped: \code{function}, \code{if}, \code{for}, \code{while}, \code{repeat} and \code{else}.
#' 
#' Most of the time, \code{all_args_here()} does not check inside comments, but some unexpected writing could dupe \code{all_args_here()}. Please, report \href{https://github.com/safer-r/saferDev/issues}{here} if it is the case.
#' 
#' The returned line numbers are indicative, depending on which source is checked. For instance, \code{saferDev::report} (compiled) has not the same line numbers as its \href{https://github.com/safer-r/saferDev/blob/main/R/report.R}{source file}. Notably, compiled functions do not have comments anymore, compared to the same source function sourced into the working environment. In addition, the counting starts at the \code{"<- function"} line, i.e., without counting the \code{#'} header lines potentially present in source files.
#' 
#' The function works first by replacing in the code: 1) the three consecutive characters \code{'"'} or \code{'"'} by three spaces, 2) \code{"\"'"} and \code{'\'"'} by four spaces and 3) escape quotes like \code{\"} or \code{\'} by two spaces.
#' 
#' Pre-check: the submitted function is first checked by \code{lambda_check()}: if some lambda functions are written with the \code{\\(x)} shorthand, an error is returned asking to run \code{saferDev::lambda_check()} first, because the \code{\\} writings are not managed by the text-based detection of \code{all_args_here()}. As in \code{lambda_check()}, the \code{\\(x)} writing is only detectable in a function carrying its source references (attribute \code{srcref}): a compiled or loaded package function prints \code{function(x)}, so that the pre-check silently passes in that case.
#' 
#' Known limitation: a string literal containing an escaped backslash-quote, like \code{"'\\'"}, desynchronizes the in-quotes detection of \code{all_args_here()}, which can return an internal error (see the \code{dev/repro_all_args_here_internal_error_3.R} script of the package repository). Please, report \href{https://github.com/safer-r/saferDev/issues}{here} if it is the case.
#' 
#' Comments and indentation of the modified function (\code{fun_export} argument and returned function): the missing arguments are inserted into the printed code of the submitted function, so that its comments and indentation are kept. This requires that the submitted function carries its source references (attribute \code{srcref}, i.e., \code{options(keep.source = TRUE)} when the function was created, the default in interactive sessions). Otherwise (e.g., a function of an installed package, or a function created in an \code{Rscript} session), the printed code is the deparsed code, without comments and with a standard indentation. In addition, a proposal is not applied (and is indicated in the final message) if the corrected call contains a string with consecutive spaces (e.g., \code{"a  b"}), because the proposals are built from a code where consecutive spaces are collapsed.
#' 
#' See more examples \href{https://safer-r.github.io/saferDev/articles/all_args_here.html}{here}.
#' 
#' Warnings:
#' \enumerate{
#'   \item The following R functions are also skipped (as indicated by \code{"SKIPPED"} in the \code{DEF_ARGS} column of the returned data frame): \code{as.environment()} (see the R bug report 18849 in the R bug tracker).
#'   \item Some functions, like \code{rownames()}, have different arguments depending on whether something is assigned to it (e.g., \code{rownames(x) <- "a"}) or not. The \code{all_args_here()} function always proposes the arguments defined in the help page without assignment, meaning that it cannot detect the assignment. A way to bypass this is to use the "exact" writing of the function. For instance, \code{base::"rownames<-"(x, "a")} instead of using \code{base::rownames(x) <- "a"}, and use \code{getAnywhere("rownames<-")} to see the arguments of the function.
#'   \item The function could not properly work if any comma is present in default argument values. Please, report \href{https://github.com/safer-r/saferDev/issues}{here} if it is the case.
#'   \item Proposals in the \code{STATUS} column are only suggestions, as it is difficult to anticipate all the exceptions with arguments writing.
#' }
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @author Haiding Wang  
#' @author Yushi Han
#' @examples
#' # Warning: these examples may not work well when using the "Run examples" link 
#' # because of a particular environment. Please, copy-paste in a local environment.
#' # See also https://safer-r.github.io/saferDev/articles/all_args_here.html
#' fun1 <- function(x){
#'     base::paste0(x, "a") # collapse and recycle0 arguments not written
#' }
#' fun1_safer <- saferDev::all_args_here(x = fun1, safer_check = FALSE) # arguments added
#' fun1_safer # corrected function
#' # Write the table (res.tsv) and the corrected function (fun1_safer.R) in a new temporary folder:
#' tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(), fileext = "")
#' base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
#' saferDev::all_args_here(x = fun1, table_export = TRUE, fun_export = TRUE, out_path = tempo_dir,
#'     safer_check = FALSE)
#'
#' @export
all_args_here <- function(
    x,
    table_export = FALSE,
    fun_export = FALSE,
    out_path = ".",
    df_name = "res.tsv",
    overwrite = FALSE,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
){
    # DEBUGGING
    # FUN1 <- function(x){base::length(x)} ; x = FUN1 ; table_export = FALSE ; fun_export = FALSE ; out_path = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- "all_args_here" ; arg_user_setting = base::list(x = as.name(x = "FUN1"), table_export = FALSE, fun_export = FALSE, out_path = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # x = .expand_R_libs_env_var ; table_export = FALSE ; fun_export = FALSE ; out_path = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- "all_args_here" ; arg_user_setting = base::list(x = as.name(x = ".expand_R_libs_env_var"), table_export = FALSE, fun_export = FALSE, out_path = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # library(saferGraph) ; x = close2 ; table_export = FALSE ; fun_export = FALSE ; out_path = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\get_message.R") ; x = get_message ; table_export = TRUE ; fun_export = FALSE ; out_path = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # library(saferDev) ; x = get_message ; table_export = TRUE ; fun_export = FALSE ; out_path = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\all_args_here.R") ; x = all_args_here ; table_export = TRUE ; fun_export = FALSE ; out_path = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # arg_user_setting = base::list(x = as.name(x = "test2"), table_export = TRUE, fun_export = FALSE, out_path = ".",  df_name = "res.tsv",  overwrite = TRUE,  lib_path = NULL,  safer_check = TRUE, error_text = "")
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test2.R") ; x = test2 ; table_export = TRUE ; fun_export = FALSE ; out_path = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = "" # use the folling line before out <-
    # function_name <- "all_args_here" ; arg_user_setting = base::list(x = as.name(x = "test2"), table_export = TRUE, fun_export = FALSE, out_path = ".",  df_name = "res.tsv",  overwrite = TRUE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\.github\\profile\\backbone.R") ; x = BACKBONE ; table_export = TRUE ; fun_export = TRUE ; out_path = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = "" # use the folling line before out <-
    # function_name <- "all_args_here" ; arg_user_setting = base::list(x = as.name(x = "BACKBONE"), table_export = TRUE, fun_export = TRUE, out_path = ".",  df_name = "res.tsv",  overwrite = TRUE, lib_path = NULL, safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # FUN1 <- function(x, y){middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, quote = FALSE, envir = base::parent.frame())} ; x = FUN1 ; table_export = FALSE ; fun_export = FALSE ; out_path = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- "all_args_here" ; arg_user_setting = base::list(x = as.name(x = "FUN1"), table_export = FALSE, fun_export = FALSE, out_path = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # x = saferDev::colons_check ; table_export = FALSE ; fun_export = FALSE ; out_path = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- "colons.check" ; arg_user_setting = base::list(x = as.name(x = "colons.check"), table_export = FALSE, fun_export = FALSE, out_path = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # x = saferDev::all_args_here ; table_export = FALSE ; fun_export = FALSE ; out_path = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- "all_args_here" ; arg_user_setting = base::list(x = as.name(x = "all_args_here"), table_export = FALSE, fun_export = FALSE, out_path = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # x = .functions_detect ; table_export = FALSE ; fun_export = FALSE ; out_path = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- ".functions_detect" ; arg_user_setting = base::list(x = as.name(x = ".functions_detect"), table_export = FALSE, fun_export = FALSE, out_path = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # fun1 <- function(){utils::installed.packages(lib.loc = lib_path)} ; x = fun1 ; table_export = FALSE ; fun_export = FALSE ; out_path = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- "fun1" ; arg_user_setting = base::list(x = as.name(x = "fun1"), table_export = FALSE, fun_export = FALSE, out_path = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")
    # x = .colons_check_message ; table_export = FALSE ; fun_export = FALSE ; out_path = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- ".colons_check_message" ; arg_user_setting = base::list(x = as.name(x = ".colons_check_message"), table_export = FALSE, fun_export = FALSE, out_path = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "table_export", "fun_export", "out_path",  "df_name",  "overwrite", "lib_path", "safer_check", "error_text")


    #### package name
    package_name <- "saferDev" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message
    #### end internal error report link

    #### function name
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L))) # warning: I have written n = 0 to avoid error when a safer function is inside another functions. In addition, arguments values retrieved are not evaluated base::match.call, but this is solved with get() below
    function_name <- base::paste0(tempo_settings[[1]], "()", collapse = NULL, recycle0 = FALSE) 
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
    if(base::any(x = tempo_log, na.rm = TRUE)) {
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
        "out_path",
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
                    base::all(x = x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK
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
    if(safer_check == TRUE){ # this line must be inactivated if you want to use lib_path in the main code (other than in safer functions present in the main code) 
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
                base::on.exit(expr = base::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
                base::.libPaths(new = base::sub(x = base::c(ini_lib_path, lib_path), pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
                lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            }
        }else{
            lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base::.libPaths(new = lib_path) # or base::.libPaths(new = base::c(base:::.libPaths(), lib_path))
        }
    }  # this line must be inactivated if you want to use lib_path in the main code (other than in safer functions present in the main code) 
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check <- utils::getFromNamespace(x = ".pack_and_function_check", ns = "saferDev", pos = , envir = )
        .pack_and_function_check(
            fun = base::c(
                # functions required in this code
                "saferDev::arg_check", # also in internal functions
                "saferDev::is_function_here", 
                "saferDev::lambda_check", 
                # end functions required in this code
                # internal functions required in this code
                "saferDev:::.base_op_check", # also in internal functions
                "saferDev:::.functions_detect", # requires saferDev::arg_check, saferDev:::.extract_all_fun_names, saferDev:::.has_odd_number_of_quotes
                "saferDev:::.in_quotes_replacement", # requires saferDev::arg_check, saferDev:::.has_odd_number_of_quotes
                "saferDev:::.fun_args_pos", # requires saferDev::arg_check
                "saferDev:::.extract_all_fun_names", # requires saferDev::arg_check
                "saferDev:::.in_parenthesis_replacement", # requires saferDev::arg_check
                "saferDev:::.all_args_here_fill", # requires saferDev::arg_check
                "saferDev:::.all_args_insert", # requires saferDev::arg_check
                # end internal functions required in this code
                # functions required in internal functions above (i.e., :::.FUNCTION_NAME), because presence not checked in internal functions
                "saferDev:::.has_odd_number_of_quotes" # from saferDev:::.functions_detect, saferDev:::.in_quotes_replacement
                # end functions required in internal functions above (i.e., :::.FUNCTION_NAME), because presence not checked in internal functions
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev", pos = , envir = )
    .functions_detect <- utils::getFromNamespace(x = ".functions_detect", ns = "saferDev", pos = , envir = )
    .in_quotes_replacement <- utils::getFromNamespace(x = ".in_quotes_replacement", ns = "saferDev", pos = , envir = )
    .fun_args_pos <- utils::getFromNamespace(x = ".fun_args_pos", ns = "saferDev", pos = , envir = )
    .extract_all_fun_names <- utils::getFromNamespace(x = ".extract_all_fun_names", ns = "saferDev", pos = , envir = )
    .in_parenthesis_replacement <- utils::getFromNamespace(x = ".in_parenthesis_replacement", ns = "saferDev", pos = , envir = )
    .all_args_here_fill <- utils::getFromNamespace(x = ".all_args_here_fill", ns = "saferDev", pos = , envir = )
    .has_odd_number_of_quotes <- utils::getFromNamespace(x = ".has_odd_number_of_quotes", ns = "saferDev", pos = , envir = )
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
    tempo <- saferDev::arg_check(data = out_path, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
    reserved_words <- base::c("NOT_CONSIDERED")
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
    # capture of the folder path for the exports (out_path is modified below by the folding of df_name)
    out_dir <- out_path
    if(table_export == TRUE | fun_export == TRUE){
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
                "DIRECTORY PATH INDICATED IN THE out_path ARGUMENT DOES NOT EXISTS:\n",
                out_path,
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        out_dir <- out_path # folder path after trailing / or \\ removal
        if(fun_export == TRUE){
            # determination of the file name: name of the submitted function, as written by the user, with _safer added at the end
            # arg_user_setting$x is a language object: as.name(x = "test") or the parsed call "saferDev::test". deparse() recovers the written form
            if(base::as.character(x = arg_user_setting$x)[1] == "::" | base::as.character(x = arg_user_setting$x)[1] == ":::"){
                tempo_fun_name <- base::as.character(x = arg_user_setting$x)[3] # form "package::function" or "package:::function"
            }else if(base::is.symbol(x = arg_user_setting$x)){
                tempo_fun_name <- base::as.character(x = arg_user_setting$x)[1]
            }else{
                # multi-component written name (like FUN_LIST$fun), or inline definition (like function(x){x}), or other unexpected writing: cannot determine a file name reliably. FUN_LIST$fun would give fun_safer.R, but a same name can designate different functions depending on the object browsed by $: the written form is not reliable for the file name
                tempo_cat <- base::paste0(
                    error_text_start,
                    "THE x ARGUMENT MUST BE DESIGNATED BY A FUNCTION NAME (NOT BY AN INLINE FUNCTION DEFINITION, NOR BY A COMPONENT WRITING LIKE obj$fun) WHEN THE fun_export ARGUMENT IS TRUE.\nHERE IT IS:\n",
                    base::paste0(base::deparse(expr = arg_user_setting$x, width.cutoff = 60L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = "\n", recycle0 = FALSE),
                    collapse = NULL,
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            tempo_fun_file_name <- base::paste0(tempo_fun_name, "_safer.R", collapse = NULL, recycle0 = FALSE)
            # the file existence check is performed here, like in the other safer functions having the export argument: the check is performed before the main code, so a pre-existing file is never silently overwritten (except when overwrite = TRUE)
            if(overwrite == FALSE & base::file.exists(x = base::paste0(out_path, "/", tempo_fun_file_name, collapse = NULL, recycle0 = FALSE))){
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

    # arg_user_setting$x <- base::as.character(arg_user_setting$x)
    arg_user_setting$x <- base::deparse(expr = arg_user_setting$x, width.cutoff = 60L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L) # because arg_user_setting$x is str(arg_user_setting$x) "language saferDev::colons_check". When I use it as string, like as.character(arg_user_setting$x), it splits  "::"           "saferDev"     "colons_check"
    # pre-check of the lambda writings: lambda_check() must be run first, because the \\ writings are not managed by the text-based detection of all_args_here(). Warning: out_path is inert here (export = FALSE: no file writing), so that the default "." writing is used
    tempo_lambda <- base::suppressMessages(expr = saferDev::lambda_check(x = x, export = FALSE, out_path = ".", safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text), classes = "message")
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
    for(i1 in 1:base::length(x = fun_names)){
        tempo_pos_in_code <- base::as.integer(x = base::sub(pattern = "^c", replacement = "", x = base::names(x = fun_names)[i1], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE))
        tempo_which <- base::which(x = code_line_nb %in% tempo_pos_in_code, arr.ind = FALSE, useNames = TRUE)
        for(i2 in 1:base::length(x = fun_names[[i1]])){
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
                tempo_pos <- .fun_args_pos(
                    text = fun_1_line_replace, 
                    pattern = base::paste0(fun_names[[i1]][i2], "[\\s\\r\\n]*\\(", collapse = NULL, recycle0 = FALSE), 
                    lib_path = lib_path, 
                    error_text = embed_error_text
                ) # positions of 1st letter of the function name and opening and closing brackets # Warning: fun_1_line_replace used because the input string must be cleaned form brackets between quotes
                tempo_str_before <- base::substr(x = fun_1_line_replace, start = 1, stop = fun_pos_start - 1)
                tempo_log <- base::grepl(x = tempo_str_before, pattern = "\\$ *$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
                if(tempo_log){ # remove functions preceeded by $, like a$fun()
                    arg_string_for_col3[[i1]][i2] <- ""
                    fun_names[[i1]][i2] <- ""
                    arg_string[[i1]][i2] <- ""
                    base::substr(x = fun_1_line_replace, start = 1, stop = fun_pos_start - 1) <- base::paste(base::rep(x = " ", fun_pos_start - 1), collapse = "", recycle0 = FALSE, sep = " ")
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
    for(i1 in 1:base::length(x = fun_names)){
        for(i2 in 1:base::length(x = fun_names[[i1]])){
            pattern2 <- base::paste0(fun_names[[i1]][i2], "[\\s\\r\\n]*\\(", collapse = NULL, recycle0 = FALSE) # function detection in 
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
    # removal of detected function preceeded by $, which are "" in col2
    tempo_log <- col2 == "" 
    if(base::any(tempo_log, na.rm = TRUE)){
        code_for_col <- code_for_col[ ! tempo_log]
        col1 <- col1[ ! tempo_log]
        col2 <- col2[ ! tempo_log]
        col3 <- col3[ ! tempo_log]
        col4 <- col4[ ! tempo_log]
        middle_bracket <- middle_bracket[ ! tempo_log]
        middle_bracket_open_in_col3 <- middle_bracket_open_in_col3[ ! tempo_log]
        middle_bracket_close_in_col3 <- middle_bracket_close_in_col3[ ! tempo_log]
    }
    # end removal of detected function preceeded by $, which are "" in col2
    # end preparation of columns
    # two new columns for arg proposal
    if( (base::length(x = col1) == 0)){
        base::message(base::paste0("\n\nINSIDE ", base::as.character(x = out$arg_user_setting$x), "(), EVERYTHING SEEMS CLEAN, AS NO ANALYSABLE FUNCTION DETECTED.\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE)
        #### warning output
        if( ! base::is.null(x = warn)){
            base::on.exit(
                expr = base::warning(
                    base::paste0(
                        base::sub(pattern = "^ERROR IN ", replacement = "FROM ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
                        warn, 
                        collapse = NULL, 
                        recycle0 = FALSE
                    ), call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL
                ), add = TRUE, after = TRUE
            )
        }
        base::on.exit(expr = base::options(warning.length = ini_warning_length), add = TRUE, after = TRUE)
        #### end warning output
    }else{
        col5 <- NULL # all arguments of the function with default value
        col6 <- NULL # missing arg names
        col7 <- NULL # potential missing args with values
        col8 <- NULL # reconstructed function with all arg
        for(i2 in 1:base::length(x = col1)){
            if(col3[i2] != reserved_words){
                # check if internal function
                tempo_log <- base::ifelse(test = base::length(x = out$internal_fun_names) > 0, yes = base::ifelse(test = base::any(out$internal_fun_names %in% col2[i2], na.rm = TRUE), yes = TRUE, no = FALSE), no = FALSE)
                if(tempo_log){
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
                        "\n\nPLEASE, RUN saferDev::colons_check(", arg_user_setting$x, ") FIRST,\nAND RERUN saferDev::all_args_here(", arg_user_setting$x, 
                        ").", 
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
                    }else{
                        # nocov start
                        # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
                        tempo_cat <- base::paste0("INTERNAL ERROR 4 IN ", intern_error_text_start,"CANNOT GET THE ARGUMENTS OF A FUNCTION THAT IS NOT CORRECTLY ASSOCIATED TO ITS PACKAGE IN LINE ", col1[i2], ":\n\n", base::paste0(base::paste0(base::substr(x = code_for_col[i2], start = 1, stop = col4[i2] - 1), col3[i2], collapse = NULL, recycle0 = FALSE), collapse = "\n", recycle0 = FALSE), "\n\nPLEASE, RUN utils::getAnywhere('", col2[i2], "')$where FIRST,\nCORRECT THE LINE ", col1[i2], " IN ", arg_user_setting$x, ",\nSAVE AND REIMPORT ", arg_user_setting$x, "\nAND RERUN saferDev::all_args_here(", arg_user_setting$x, ").", intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                        # nocov end
                    }
                    if(base::is.primitive(x = tempo_fun2)){
                        tempo_all_args <- base::as.list(x = base::suppressWarnings(expr = base::formals(fun = base::args(name = col2[i2]), envir = base::asNamespace(ns = tempo_package_name, base.OK = TRUE)), classes = "warning"))
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
                        tempo <- base::sapply(X = arg_full, FUN = function(x){base::paste0(base::ifelse(test = base::all(base::typeof(x = x) == "symbol", na.rm =TRUE), yes = "", no = " = "), base::deparse(expr = x, width.cutoff = 60L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = NULL, recycle0 = FALSE)}, simplify = TRUE, USE.NAMES = TRUE)
                        tempo <- base::paste(base::paste0(base::names(x = tempo), tempo, collapse = NULL, recycle0 = FALSE), collapse = ", ", recycle0 = FALSE, sep = " ")
                        col5 <- base::c(col5, tempo)
                        # end all arguments of the function with default value in col5
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
                            for(i6 in 1:base::length(x = middle_bracket_open_in_col3[[i2]])){
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
                            for(i6 in 1:base::length(x = tempo_split)){
                                tempo_log <- pos_rep2 >= 1 & pos_rep2 <= base::nchar(x = tempo_split[i6], type = "chars", allowNA = FALSE, keepNA = NA)
                                if(base::any(base::is.na(x = tempo_log), na.rm = TRUE)){
                                    # nocov start
                                    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
                                    tempo_cat <- base::paste0("INTERNAL ERROR 7 IN ", intern_error_text_start, "tempo_log CONTAINS NA:\n", base::paste0(tempo_log, collapse = " ", recycle0 = FALSE), intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                                    # nocov end
                                }
                                if(base::any(tempo_log, na.rm = TRUE)){
                                    for(i7 in 1:base::length(x = tempo_log)){
                                        if(tempo_log[i7]){
                                            base::substr(x = tempo_split[i6], start = pos_rep2[i7], stop = pos_rep2[i7]) <- ","
                                        }
                                    }
                                }
                                pos_rep2 <- pos_rep2 - base::nchar(x = tempo_split[i6], type = "chars", allowNA = FALSE, keepNA = NA) - 1 # -1 because of the comma that separates each tempo_split
                            }
                        }
                        # end rewriting the commas inside args
                        # working on each observed arg
                        arg_full_names <-  base::names(x = arg_full) # NULL if arg_full is empty list()
                        three_dots_log <- if(base::length(x = arg_full) > 0){
                            arg_full_names == "..."
                        }else{
                            FALSE
                        }
                        # checking 
                        if(base::length(x = tempo_split) > base::length(x = arg_full_names) & ! base::any(three_dots_log, na.rm = TRUE)){
                            # nocov start
                            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
                            tempo_cat <- base::paste0("INTERNAL ERROR 8 IN ", intern_error_text_start, "LENGTH OF tempo_split MUST LOWER OR EQUAL TO LENGTH OF arg_full_names IF ... IS NOT AN ARGUMENT OF THE FUNCTION.\n\nPLEASE, CHECK FIRST THAT ARGUMENTS ARE CORRECTLY ADDED INTO THE FUNCTION.\n\nFUNCTION: ", col2[i2], "\n\ntempo_split (", base::length(x = tempo_split), "):\n", base::paste0(tempo_split, collapse = "\n", recycle0 = FALSE), "\n\narg_full_names (", base::length(x = arg_full_names), "):\n", base::paste0(arg_full_names, collapse = "\n", recycle0 = FALSE), "\n\ni2:\n", i2, "\n\nIF ARGUMENTS ARE CORRECTLY ADDED, THEN ",intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                            # nocov end
                        }
                        # end checking
                        tempo_out <- .all_args_here_fill(
                            arg_full = arg_full, 
                            arg_full_names = arg_full_names, 
                            tempo_split = tempo_split, 
                            three_dots_log = three_dots_log, 
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
                ), add = TRUE, after = TRUE
            )
        }
        base::on.exit(expr = base::options(warning.length = ini_warning_length), add = TRUE, after = TRUE)
        #### end warning output

        #### output
        output <- base::data.frame(LINE_NB = col1, FUN_NAME = col2, FUN_ARGS = col3, FUN_POS = col4, DEF_ARGS = col5, MISSING_ARG_NAMES = col6, MISSING_ARGS = col7, STATUS = col8, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
        if(table_export == FALSE){
            # the table is returned as a message with on.exit(), after the EVERYTHING SEEMS CLEAN or ARGUMENTS ARE MISSING message registered above, and before the CORRECTIONS NOT APPLIED and MODIFIED CODE EXPORTED IN messages registered below
            tempo_cat_table <- base::paste0(utils::capture.output(output, file = NULL, append = FALSE, type = "output", split = FALSE), collapse = "\n", recycle0 = FALSE)
            base::on.exit(expr = base::message(base::paste0(tempo_cat_table, "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE), add = TRUE, after = TRUE)
        }
        # modification of the submitted function with the missing arguments
        .all_args_insert <- utils::getFromNamespace(x = ".all_args_insert", ns = "saferDev", pos = , envir = )
        tempo_insert <- .all_args_insert(
            x = x,
            out = out,
            output = output,
            lib_path = lib_path,
            error_text = embed_error_text
        )
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
            if(overwrite == FALSE & base::file.exists(x = tempo_fun_file)){
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
        #### end output

        #### return
        # the table is never returned: message if table_export = FALSE, file if table_export = TRUE
        if(fun_export == TRUE & base::length(x = tempo_insert$inserted) > 0){
            base::return(base::invisible(x = NULL)) # the modified function is not returned when exported
        }else if(base::length(x = tempo_insert$inserted) > 0){
            base::return(tempo_insert$fun) # return the modified function
        }else{
            base::return(base::invisible(x = NULL)) # return NULL if no modification has been applied
        }
        #### end return
    }
    # end two new columns for arg proposal
}
