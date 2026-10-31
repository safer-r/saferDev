#' @title Colons Check
#' @description
#' Verify that all the functions used inside a function are preceded by a package attribution. For instance: \code{base::mean()} and not \code{mean()}, or \code{saferDev:::.base_op_check()} and not \code{.base_op_check()}. Warning: does not check that the package is the good one. Use \code{all_args_here()} for that.
#' @param x a function name, written without quotes and brackets.
#' @param export Single logical value. Export the modified code into a \code{.R} file? If \code{TRUE}, the modified function is not returned but only exported. The name of the file is the name of the tested function, with \code{_safer} added at the end (e.g., \code{test_safer.R}). The written code is the code of the submitted function, with its comments and indentation, only modified by the added package attributions (see the \code{Details} section). An error is returned if the file already exists in \code{out_path}, and if the tested function is not designated by a function name (like \code{test} or \code{package::test}), i.e., written by an inline definition (like \code{function(x)\{x\}}) or by a component writing (like \code{FUN_LIST$fun}). If \code{FALSE} (default), the modified function is not written but returned. 
#' @param out_path Single character string indicating the pathway of the folder where to export the modified code. \code{out_path = "."} (default) means the R working directory set by the user. A non-existing directory, \code{NULL} or an empty string (\code{""}) returns an error. Ignored if \code{export} is \code{FALSE}.
#' @param safer_check Single logical value. Perform the "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) required functions and related packages effectively present in local R libraries and 2) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Warning: must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default): only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns 
#' A table-like text message indicating the missing \code{::} or \code{:::} or a message saying that everything seems fine.
#' 
#' The message is always printed (see the \code{Details} section).
#'
#' A warning and \code{NULL} are returned if the submitted function is empty of function, or only made of comments, or only made of \code{function}, \code{if}, \code{for}, \code{while}, \code{repeat}, \code{else}.
#'
#' In addition, if the \code{export} argument is \code{FALSE} (default), the function returns \code{NULL} if no basic function required a package attribution, or the modified function if basic functions were modified (see the \code{Details} section).
#' 
#' If the \code{export} argument is \code{TRUE}, the modified code is exported in the \code{.R} file specified by the \code{out_path} argument, instead of being returned, and a message indicating where the modified code has been exported is printed.
#' 
#' Table-like: column 1, the line number in the function code (starting at the \code{"<- function"} line, i.e., without counting the \code{#'} header lines); column 2, the function name; column 3, the code preceding the function name.
#' 
#' With missing \code{::} or \code{:::}, the message also indicates if internal functions are created inside the checked function code, since these functions cannot have \code{::} or \code{:::}.
#' 
#' @details
#' The result is used to modify the code inside the tested function like this: \code{<PACKAGE>::<FUNCTION>} (or \code{<PACKAGE>:::<FUNCTION>} for function names starting by a dot).
#' 
#' Since \code{saferDev} v3.0.0, the modified function is returned: the package attributions of the basic R functions, as indicated in the \code{"INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE BASIC FUNCTIONS"} section of the returned message, are added to the submitted function code, and the modified function is returned using \code{return()}. The package attributions of the other functions, as indicated in the \code{"INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE OTHER FUNCTIONS"} section of the returned message, are NOT added automatically, because the correct package cannot be determined reliably: a \code{base::warning()} is issued instead (in addition to the printed message). \code{NULL} is returned if no basic function required a package attribution.
#' 
#' The modification is only performed on the code lines indicated in the \code{"INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE BASIC FUNCTIONS"} section of the message, which is the same analysis as the one performed by \code{colons_check()} itself: functions written between quotes or preceded by \code{$} are not modified, and internal functions (created inside the tested function) are not modified either. The analysis is performed on the code with comments removed and multiple spaces collapsed, but the package attributions are added to the code of the submitted function as printed: the returned (and exported) function therefore keeps the comments and the indentation of the submitted function. Warning: comments are only present when the submitted function carries its source references (attribute \code{srcref}, e.g., a function defined in the console or in a file loaded with \code{keep.source = TRUE}). Otherwise, as for compiled package functions, the code printed by \code{R} (without comments and reindented) is used. The modified function is returned with the environment of the submitted function, so that the functions used inside it still resolve the same way. If the modified code does not parse (a rare situation, e.g., a function name split across lines in a way the analyzer could not handle correctly), this is an internal bug: an internal error is returned when \code{export = TRUE}, and the submitted function is returned unchanged, with no error, when \code{export = FALSE}.
#' 
#' The name of the exported file (when \code{export = TRUE}) is the name of the submitted function, as written by the user, with \code{_safer} added at the end, e.g., \code{test_safer.R} for \code{colons_check(x = test)}. If the submitted function was written as \code{package::function}, like \code{colons_check(x = saferDev::test)}, the name is \code{test_safer.R} (the package name is not part of the file name). The exported file contains an assignment of the modified function: \code{test_safer <- function(...) ...}, so that \code{base::source()}ing it recreates the modified function. Warning: the tested function must be designated by a function name (like \code{test} or \code{package::test}) when \code{export = TRUE}: an inline definition (like \code{function(x)\{x\}}) or a component writing (like \code{FUN_LIST$fun}) returns an error, because the name of the exported file could not be determined reliably.
#' 
#' Pre-check: the submitted function is first checked by \code{lambda_check()}: if some lambda functions are written with the \code{\\(x)} shorthand, an error is returned asking to run \code{saferDev::lambda_check()} first, because the \code{\\} writings are not managed by the text-based detection of \code{colons_check()}. As in \code{lambda_check()}, the \code{\\(x)} writing is only detectable in a function carrying its source references (attribute \code{srcref}): a compiled or loaded package function prints \code{function(x)}, so that the pre-check silently passes in that case.
#' 
#' More precisely, \code{colons_check()} verifies that all the strings before an opening bracket \code{(} are preceded by \code{::}.
#' 
#' \code{:::} are not checked per se, because incorrect writing, like \code{saferDev::.colons_check_message()} returns an error when executing the tested function, and because \code{base:::sum()} is as ok as \code{base::sum()}. In the same manner, more than three colons are not checked because it returns an error when executing the tested function.
#' 
#' Warning: \code{colons_check()} cannot check function names written without brackets, like in the \code{FUN} argument of some functions, e.g., \code{sapply(1:3, FUN = as.character)}.
#' 
#' The Perl regex used to detect a function name is: \code{"([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*\\("}.
#' 
#' Currently, \code{colons_check()} cannot detect functions written between quotes, like \code{"+"()} or \code{"rownames<-"(x, "a")}.
#' 
#' Function names preceded by \code{$} are not considered.
#'  
#' The following R functions are skipped: \code{"function"}, \code{"if"}, \code{"for"}, \code{"while"}, \code{"repeat"} and \code{"else"}.
#' 
#' Most of the time, \code{colons_check()} does not check inside comments, but some unexpected writing could dupe \code{colons_check()}. Please, report \href{https://github.com/safer-r/saferDev/issues}{here} if it is the case.
#' 
#' The returned line numbers are indicative, depending on which source is checked. For instance, \code{saferDev::report} (compiled) has not the same line numbers as its source file (\href{https://github.com/safer-r/saferDev/blob/main/R/report.R}{source file}). Notably, compiled functions do not have comments anymore, compared to the same source function sourced into the working environment. In addition, the counting starts at the \code{"<- function"} line, i.e., without counting the \code{#'} header lines potentially present in source files.
#' 
#' Of note, during package creation, the \code{devtools::check()} command tells which functions where wrongly attributed to package. Example: 
#' \preformatted{
#' checking dependencies in R code ... WARNING
#'   '::' or ':::' import not declared from: 'sbase'
#'   Missing or unexported objects:
#'     'base::dev.off' 'base::graphics.off' 'base::hcl' 'base::par' 'base::read.table' 'saferGG::report'
#' }
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @author Haiding Wang  
#' @author Yushi Han
#' @examples
#' # Warning: these examples may not work well when using the "Run examples" link 
#' # because of a particular environment. Please, copy-paste in a local environment.
#' # See also https://safer-r.github.io/saferDev/articles/colons_check.html
#' fun1 <- function(x){
#'     y <- mean(x) # mean() written without base::
#'     base::return(y)
#' }
#' fun1_safer <- saferDev::colons_check(fun1) # base:: added before mean()
#' fun1_safer # corrected function
#' # Write the corrected function in the fun1_safer.R file of a new temporary folder:
#' tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(), fileext = "")
#' base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
#' saferDev::colons_check(x = fun1, export = TRUE, out_path = tempo_dir)
#' 
#' @export
colons_check <- function(
    x, 
    export = FALSE, 
    out_path = ".", 
    safer_check = TRUE, 
    lib_path = NULL, 
    error_text = ""
){
    # DEBUGGING
    # x = mean ; export = FALSE ; out_path = "." ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A"
    # function_name <- "mean" ; arg_user_setting = base::list(x = as.name(x = "mean"), export = FALSE, out_path = ".", lib_path = NULL , safer_check = TRUE, error_text = " INSIDE A")
    # x = .expand_R_libs_env_var ; export = FALSE ; out_path = "." ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A"
    # library(saferGraph) ; x = close2 ; export = FALSE ; out_path = "." ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" 
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\get_message.R") ; x = get_message ; export = FALSE ; out_path = "." ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: x = saferDev::get_message does not return the same number of code lines
    # library(saferDev) ; x = get_message ; export = FALSE ; out_path = "." ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: does not return the same number of code lines than the previsou example
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\colons_check.R") ; x = colons_check ; export = FALSE ; out_path = "." ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: x = saferDev::get_message does not return the same number of code lines
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") ; x = test ; export = FALSE ; out_path = "." ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: x = saferDev::get_message does not return the same number of code lines
    # function_name <- "colons_check" ; arg_user_setting = base::list(x = as.name(x = "test"), export = FALSE, out_path = ".", lib_path = NULL , safer_check = TRUE , error_text = " INSIDE A")
    # FUN1 <- function(x, y){ifelse(base::is.null(x = warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE))} ; x = FUN1 ; export = FALSE ; out_path = "." ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A"
    # function_name <- "colons_check" ; arg_user_setting = base::list(x = as.name(x = "FUN1"), export = FALSE, out_path = ".", lib_path = NULL , safer_check = TRUE , error_text = " INSIDE A")

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
        "export", 
        "out_path", 
        # "lib_path", # inactivated because can be NULL
        "safer_check"
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
        "export", 
        "out_path", 
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
            base::on.exit(expr = base::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
            base::.libPaths(new = base::sub(x = base::c(ini_lib_path, lib_path), pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
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
                "saferDev::lambda_check",
                # end functions required in this code
                # internal functions required in this code
                "saferDev:::.base_op_check", # also in internal functions
                "saferDev:::.functions_detect",
                "saferDev:::.colons_check_message"
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
    .colons_check_message <- utils::getFromNamespace(x = ".colons_check_message", ns = "saferDev", pos = -1, envir = )

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
    tempo <- saferDev::arg_check(data = x, class = "function", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = export, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = out_path, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
        "out_path"
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
    if(export == TRUE){
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
                "THE x ARGUMENT MUST BE DESIGNATED BY A FUNCTION NAME (NOT BY AN INLINE FUNCTION DEFINITION, NOR BY A COMPONENT WRITING LIKE obj$fun) WHEN THE export ARGUMENT IS TRUE.\nHERE IT IS:\n",
                base::paste0(base::deparse(expr = arg_user_setting$x, width.cutoff = 60L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = "\n", recycle0 = FALSE),
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        out_name <- base::paste0(tempo_fun_name, "_safer.R", collapse = NULL, recycle0 = FALSE)
        out_file_name <- base::paste0(out_path, "/", out_name, collapse = NULL, recycle0 = FALSE) # base::file.path(x = out_path, y = out_name, fsep = "/") equivalent, kept for OS consistency
        if(base::file.exists(out_file_name)){
            tempo_cat <- base::paste0(
                error_text_start,
                "FILE NAME ALREADY EXISTS AT THE INDICATED PATH:\n",
                out_file_name,
                "\n\nPLEASE, RENAME OR REMOVE THIS FILE, OR CHANGE THE out_path ARGUMENT, AND RERUN ",
                package_function_name,
                ".",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # reminder: the returned function is the code modified by adding the package attributions of the basic R functions
    # the message is printed below, after the modification logic, because the modification does not change the analysis results
    # (the positions in the table-like message are the ones of the initial analysis, performed by .functions_detect() on the submitted function)
    skipped_base <- base::c("function", "if", "for", "while", "repeat", "else", "in") # skipped function and elements. in must be here because for(j in (n - 1L):2L) breaks the code
    # bare name of the submitted function, as written by the user (without the "()" decoration added below), for the exclusion of the recursive calls in the modification logic below
    if(base::as.character(x = arg_user_setting$x)[1] == "::" | base::as.character(x = arg_user_setting$x)[1] == ":::"){
        fun_bare_name <- base::as.character(x = arg_user_setting$x)[3] # form "package::function" or "package:::function"
    }else{
        fun_bare_name <- base::paste0(arg_user_setting$x, collapse = NULL, recycle0 = FALSE) # bare name of a single symbol
    }
    # pre-check of the lambda writings: lambda_check() must be run first, because the \\ writings are not managed by the text-based detection of colons_check(). Warning: out_path is inert here (export = FALSE: no file writing), so that the default "." writing is used
    arg_user_setting_x_ini <- base::deparse(expr = arg_user_setting$x, width.cutoff = 60L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L) # name of the submitted function as written by the user, for the error message below (recovered before the modification of arg_user_setting$x)
    tempo_lambda <- base::suppressMessages(expr = saferDev::lambda_check(x = x, export = FALSE, out_path = ".", safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text), classes = "message")
    if( ! base::is.null(x = tempo_lambda)){
        tempo_cat <- base::paste0(
            error_text_start,
            "SOME LAMBDA FUNCTIONS (WRITTEN \\(x)) ARE DETECTED IN THE CODE OF ",
            arg_user_setting_x_ini,
            ".\n\nPLEASE, RUN saferDev::lambda_check(",
            arg_user_setting_x_ini,
            ") FIRST,\nUSE THE CORRECTED FUNCTION RETURNED BY lambda_check() (OR EXPORT IT WITH export = TRUE),\nAND RERUN saferDev::colons_check(",
            arg_user_setting_x_ini,
            ").",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # modification of arg_user_setting$x for clean messages
    if(base::as.character(x = arg_user_setting$x)[1] == "::" | base::as.character(x = arg_user_setting$x)[1] == ":::"){
        arg_user_setting$x <- base::paste0(
            base::as.character(x = arg_user_setting$x)[2], 
            base::as.character(x = arg_user_setting$x)[1], 
            base::as.character(x = arg_user_setting$x)[3], 
            "()",
            collapse = NULL, 
            recycle0 = FALSE
        )
    }else{
        arg_user_setting$x <- base::paste0(
            arg_user_setting$x, 
            "()",
            collapse = NULL, 
            recycle0 = FALSE
        )
    }
    # end modification of arg_user_setting$x for clean messages
    out <- .functions_detect(
        x = x, 
        skipped_base = skipped_base, 
        arg_user_setting2 = arg_user_setting, 
        lib_path = lib_path, 
        error_text = embed_error_text
    )
    if(base::is.null(x = out)){
        base::return(NULL) # tested function empty of function: warning already returned by .functions_detect()
    }
    if( ! (base::all(base::typeof(x = out$fun_names) == "list", na.rm = TRUE) & base::all(base::typeof(x = out$fun_names_pos) == "list", na.rm = TRUE))){
        # nocov start
        # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
        tempo_cat <- base::paste0("INTERNAL ERROR 1 IN ", intern_error_text_start, "out$fun_names AND out$fun_names_pos MUST BE TYPE list\nout$fun_names:\n", out$fun_names, "\nout$fun_names_pos:\n", out$fun_names_pos, intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
    }
    # basic function names in x
    # selection of basic functions
    tempo_log <- base::lapply(X = out$fun_names, FUN = function(x){x %in% out$all_basic_funs}) #  are names basic functions used in x?
    in_basic_fun <- base::mapply(FUN = function(x, y){x[y]}, x = out$fun_names, y = tempo_log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    in_basic_fun_names_pos <-  base::mapply(FUN = function(x, y){x[y]}, x = out$fun_names_pos, y = tempo_log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    # end selection of basic functions
    if(base::length(x = base::unlist(x = in_basic_fun, recursive = TRUE, use.names = TRUE)) != 0){ # check if basic fun present
        # removal of string with empty function names
        tempo_log <- base::sapply(X = in_basic_fun, FUN = function(x){base::length(x = x) == 0}, simplify = TRUE, USE.NAMES = TRUE) # detection of string with empty function names
        in_basic_fun <- in_basic_fun[ ! tempo_log] # removal of empty string
        in_basic_fun_names_pos <- in_basic_fun_names_pos[ ! tempo_log]
        in_basic_code_line_nb <- out$code_line_nb[ ! tempo_log]
        if( ! (base::length(x = in_basic_fun) == base::length(x = in_basic_fun_names_pos) & base::length(x = in_basic_fun) == base::length(x = in_basic_code_line_nb))){
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
            tempo_cat <- base::paste0("INTERNAL ERROR 2 IN ", intern_error_text_start, "LENGTHS SHOULD BE IDENTICAL\nin_basic_fun: ", base::length(x = in_basic_fun), "\nin_basic_fun_names_pos: ", base::length(x = in_basic_fun_names_pos), "\nin_basic_code_line_nb: ", base::length(x = in_basic_code_line_nb), intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        # end removal of string with empty function names
        in_basic_fun_uni <- base::unlist(x = base::unique(x = in_basic_fun, incomparables = FALSE), recursive = TRUE, use.names = TRUE) #  names of unique basic functions used in x
        # end basic function names in x
        # analyse of :: before basic functions in x
        if(base::length(x = in_basic_fun_uni) > 0){
            tempo <- .colons_check_message(
                list_fun = in_basic_fun, 
                list_fun_pos = in_basic_fun_names_pos, 
                line_nb = in_basic_code_line_nb, 
                ini = out$code, 
                arg_user_setting2 = out$arg_user_setting, 
                text = "BASIC", 
                internal_fun_names = out$internal_fun_names,
                lib_path = lib_path, 
                error_text = embed_error_text
            )
            log_basic <- tempo$colon_not_here
            cat_basic <- tempo$output.cat
            basic_col1 <- tempo$col1
            basic_col2 <- tempo$col2
            basic_col3 <- tempo$col3
        }else{
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values. Because empty function, etc., are already checked above
            log_basic <- FALSE
            cat_basic <- NULL
            basic_col1 <- base::integer(length = 0)
            basic_col2 <- base::character(length = 0)
            basic_col3 <- base::character(length = 0)
            # nocov end
        }
        # end analyse of :: before basic functions in x
    }else{
        log_basic <- FALSE
        cat_basic <- NULL
        basic_col1 <- base::integer(length = 0)
        basic_col2 <- base::character(length = 0)
        basic_col3 <- base::character(length = 0)
    }
    # other function names in x
    # selection of other functions
    tempo_log <- base::lapply(X = out$fun_names, FUN = function(x){ ! x %in% base::c(out$all_basic_funs, out$arg_user_setting$x)}) #  names of all the other functions used in x, except the one tested (arg_user_setting$x), because can be in error messages
    in_other_fun <- base::mapply(FUN = function(x, y){x[y]}, x = out$fun_names, y = tempo_log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    in_other_fun_names_pos <-  base::mapply(FUN = function(x, y){x[y]}, x = out$fun_names_pos, y = tempo_log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    # end selection of other functions
    if(base::length(x = base::unlist(x = in_other_fun, recursive = TRUE, use.names = TRUE)) != 0){ # check if other fun present
        # removal of string with empty function names
        tempo_log <- base::sapply(X = in_other_fun, FUN = function(x){base::length(x = x) == 0}, simplify = TRUE, USE.NAMES = TRUE) # detection of string with empty function names
        in_other_fun <- in_other_fun[ ! tempo_log] # removal of empty string
        in_other_fun_names_pos <- in_other_fun_names_pos[ ! tempo_log]
        in_other_code_line_nb <- out$code_line_nb[ ! tempo_log]
        if( ! (base::length(x = in_other_fun) == base::length(x = in_other_fun_names_pos) & base::length(x = in_other_fun) == base::length(x = in_other_code_line_nb))){
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
            tempo_cat <- base::paste0("INTERNAL ERROR 3 IN ", intern_error_text_start, "LENGTHS SHOULD BE IDENTICAL\nin_other_fun: ", base::length(x = in_other_fun), "\nin_other_fun_names_pos: ", base::length(x = in_other_fun_names_pos), "\nin_other_code_line_nb: ", base::length(x = in_other_code_line_nb), intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        # end removal of string with empty function names
        in_other_fun_uni <- base::unlist(x = base::unique(x = in_other_fun, incomparables = FALSE), recursive = TRUE, use.names = TRUE) #  names of unique other functions used in x
        # end other function names in x
        # analyse of :: before other functions in x
        if(base::length(x = in_other_fun_uni) > 0){
            tempo <- .colons_check_message(
                list_fun = in_other_fun, 
                list_fun_pos = in_other_fun_names_pos, 
                line_nb = in_other_code_line_nb, 
                ini = out$code, 
                arg_user_setting2 = out$arg_user_setting, 
                text = "OTHER", 
                internal_fun_names = out$internal_fun_names,
                lib_path = lib_path, 
                error_text = embed_error_text
            )
            log_other <- tempo$colon_not_here
            cat_other <- tempo$output.cat
            other_col1 <- tempo$col1
            other_col2 <- tempo$col2
            other_col3 <- tempo$col3
        }else{
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values. Because .colons_check_message() (with text = "OTHER") removes all the reported occurrences between quotes or preceded by $ (see the .noclean_functions() call inside), col1, col2 and col3 are empty when no clean other function is reported, even when in_other_fun_uni is not empty
            log_other <- FALSE
            cat_other <- NULL
            other_col1 <- base::integer(length = 0)
            other_col2 <- base::character(length = 0)
            other_col3 <- base::character(length = 0)
            # nocov end
        }
    }else{
        log_other <- FALSE
        cat_other <- NULL
        other_col1 <- base::integer(length = 0)
        other_col2 <- base::character(length = 0)
        other_col3 <- base::character(length = 0)
    }
    # end analyse of :: before basic functions in x
    tempo_cat <- base::paste0(
        "AFTER RUNNING ", 
        base::ifelse(
            test = base::is.null(x = package_name), 
            yes = "", 
            no = base::paste0(
                package_name, 
                base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"),
                collapse = NULL, 
                recycle0 = FALSE
            )
        ), 
        function_name, 
        ".\nINSIDE ", 
        base::as.character(x = arg_user_setting$x), 
        collapse = NULL, 
        recycle0 = FALSE
    )
    if(base::all( ! log_basic, na.rm = TRUE) & base::all( ! log_other, na.rm = TRUE)){ # log_basic TRUE means a problem, ! log_basic means all(FALSE) becomes TRUE. Idem for log_other
        tempo_cat <- base::paste0(tempo_cat, ", EVERYTHING SEEMS CLEAN.", collapse = NULL, recycle0 = FALSE)
    }else{
        tempo_cat <- base::paste0(
            base::ifelse(
                test = base::is.null(x = cat_basic), 
                yes = base::paste0(
                    tempo_cat, 
                    ", EVERYTHING SEEMS CLEAN",
                    base::ifelse(test = base::is.null(x = cat_other), yes =  ".", no =  " FOR R BASIC FUNCTIONS.\n\n"), 
                    collapse = NULL, 
                    recycle0 = FALSE
                ), 
                no = base::paste0(
                    cat_basic, 
                    base::ifelse(test = base::is.null(x = cat_other), yes =  "", no =  "\n\n"), 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
            ), 
            cat_other, 
            collapse = NULL, 
            recycle0 = FALSE
        )
    }
    base::message(base::paste0("\n\n", tempo_cat, "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE)
    # warning if functions other than basic R functions need a package attribution (manual modification required)
    if(base::length(x = other_col1) > 0){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count, ") INSIDE ", base::as.character(x = arg_user_setting$x), ", SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS:\n\n", "LINE\tFUN\t\tSTRING_BEFORE\n", base::paste0(base::paste0(other_col1, "\t", other_col2, "\t\t", other_col3, collapse = NULL, recycle0 = FALSE), collapse = "\n", recycle0 = FALSE), "\n\nTHE PACKAGE ATTRIBUTIONS OF THESE FUNCTIONS CANNOT BE ADDED AUTOMATICALLY, BECAUSE THE CORRECT PACKAGE CANNOT BE DETERMINED RELIABLY.\nPLEASE, MODIFY THE CODE MANUALLY, WITH THE HELP OF THE MESSAGE PRINTED ABOVE.", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
    }
    # modification of the basic functions, only when some basic functions require a package attribution
    # the positions of the function names in out$code are recovered from the STRING_BEFORE column: the name starts just after the STRING_BEFORE string
    output <- NULL # NULL returned if no modification is done
    replaced_log <- FALSE # becomes TRUE as soon as at least one package attribution has been added
    if(base::length(x = basic_col1) > 0){
        # retrieval of the package of each basic R function, from the basic R scopes (same scopes as in .functions_detect())
        all_basic_funs <- base::unname(obj = out$all_basic_funs, force = FALSE) # names of all the basic R functions, with the scopes they belong to (unnamed, because the names of the vector are the scopes, which would break the indexing below)
        fun_package <- base::character(length = base::length(x = all_basic_funs))
        basic_scopes <- base::c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "Autoloads", "package:base")
        for(i3 in base::seq_along(along.with = basic_scopes)){
            in_scope_log <- base::vapply(X = all_basic_funs, FUN = function(x){base::exists(x = x, where = basic_scopes[i3], envir = , frame = , mode = "any", inherits = FALSE)}, FUN.VALUE = base::logical(length = 1), USE.NAMES = FALSE) # vectorized base::exists(): the functions of the scope
            fun_package[in_scope_log & fun_package == ""] <- basic_scopes[i3] # fun_package == "" to keep the first scope found, like the R scope order
        }
        fun_package <- base::sub(pattern = "^package:", replacement = "", x = fun_package, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # removal of the "package:" prefix
        package_of_flagged <- fun_package[base::match(x = basic_col2, table = all_basic_funs, nomatch = NA_integer_, incomparables = NULL)] # package of each flagged function (NA if not found: impossible since flagged functions are in all_basic_funs)
        # exclusion of the flagged occurrences that must not be modified:
        # - calls to internal functions created inside the tested function (a same name can be an internal function and a basic R function)
        # - recursive calls to the tested function itself (a same name can be the tested function and a basic R function)
        skip_log <- basic_col2 %in% out$internal_fun_names | basic_col2 == fun_bare_name # basic_col2 contains the bare function names (e.g. "mean"), so the comparison with fun_bare_name (bare name of the submitted function, as written by the user) excludes the recursive calls to the tested function itself (a same name can be the tested function and a basic R function)
        # modification of the code lines: insert <PACKAGE>:: (or <PACKAGE>::: for names starting by a dot) just before the function name, from the end of each line to the beginning, so that the positions of the not yet modified occurrences remain valid
        # the strings of basic_col3 are the code before each function name in the analyzed code (out$code), i.e., the name of the i3-th flagged occurrence starts at the position base::nchar(x = basic_col3[i3]) + 1 of the line basic_col1[i3]
        # the package attributions are inserted in the code of the submitted function as printed (comments and indentation kept), and not in the analyzed code (out$code: comments removed and multiple spaces collapsed), so that the returned or exported function keeps the comments and the indentation of the submitted function
        # the line i of out$code is the line i of the printed code (same utils::capture.output() call as in .functions_detect(), which never removes lines), with the comments removed and the multiple spaces collapsed: the positions in out$code are converted below into positions in the printed code
        code_ini <- utils::capture.output(x, file = NULL, append = FALSE, type = NULL, split = FALSE) # printed code of the submitted function, not modified (used for the conversion of the positions)
        if(base::length(x = code_ini) != base::length(x = out$code)){
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values, since the same utils::capture.output() call is used in .functions_detect()
            tempo_cat <- base::paste0("INTERNAL ERROR 4 IN ", intern_error_text_start, "THE PRINTED CODE AND THE ANALYZED CODE OF THE SUBMITTED FUNCTION SHOULD HAVE THE SAME NUMBER OF LINES\nPRINTED CODE: ", base::length(x = code_ini), "\nANALYZED CODE: ", base::length(x = out$code), intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        code_mod <- code_ini # code to modify
        name_start_pos <- base::nchar(x = basic_col3, type = "chars", allowNA = FALSE, keepNA = NA) + 1L
        ord_flagged <- base::order(basic_col1, -name_start_pos, na.last = TRUE, decreasing = FALSE, method = "auto") # lines first, then reversed positions within a line
        for(i4 in base::seq_along(along.with = ord_flagged)){
            i3 <- ord_flagged[i4]
            if(skip_log[i3]){
                next
            }
            line_i3 <- basic_col1[i3]
            pos_i3 <- name_start_pos[i3]
            fun_i3 <- basic_col2[i3]
            nchar_fun_i3 <- base::nchar(x = fun_i3, type = "chars", allowNA = FALSE, keepNA = NA)
            # security check: the name must be present at the expected position of the line in the analyzed code (out$code, never modified)
            if(pos_i3 + nchar_fun_i3 - 1L > base::nchar(x = out$code[line_i3], type = "chars", allowNA = FALSE, keepNA = NA)){
                # nocov start
                # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values, since positions come from the same analyzed code
                next
                # nocov end
            }
            # conversion of the position in the analyzed line into the position in the printed line. The analyzed line is the printed line with: 1) each run of spaces collapsed into its first space, 2) the leading space removed, 3) the comment removed (end of the line) and 4) possibly a "(" added at the end. Steps 3 and 4 do not change the positions of the remaining characters
            chars_i3 <- base::strsplit(x = code_ini[line_i3], split = "", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]] # characters of the printed line, not modified
            kept_pos_i3 <- base::which(x = ! (chars_i3 == " " & base::c(FALSE, chars_i3[-base::length(x = chars_i3)] == " ")), arr.ind = FALSE, useNames = TRUE) # positions of the characters kept by the collapsing of the multiple spaces (first space of each run kept)
            if(base::length(x = kept_pos_i3) > 0){
                if(chars_i3[kept_pos_i3[1]] == " "){
                    kept_pos_i3 <- kept_pos_i3[-1] # removal of the leading space
                }
            }
            if(pos_i3 + nchar_fun_i3 - 1L > base::length(x = kept_pos_i3)){
                # nocov start
                # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values, since the analyzed line comes from the printed line
                next
                # nocov end
            }
            ini_pos_i3 <- kept_pos_i3[pos_i3] # position of the function name in the printed line (the printed line is modified from the end to the beginning, so that this position, computed on the not modified line, remains valid)
            if(base::substr(x = out$code[line_i3], start = pos_i3, stop = pos_i3 + nchar_fun_i3 - 1L) == fun_i3 & base::substr(x = code_mod[line_i3], start = ini_pos_i3, stop = ini_pos_i3 + nchar_fun_i3 - 1L) == fun_i3){
                colons_i3 <- base::ifelse(test = base::grepl(x = fun_i3, pattern = "^\\.", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::")
                if(base::is.na(x = package_of_flagged[i3]) | package_of_flagged[i3] == ""){
                    # nocov start
                    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values, since all the basic functions are in the basic scopes
                    base::stop(base::paste0("\n\n================\n\n", intern_error_text_start, "THE PACKAGE OF THE BASIC FUNCTION ", fun_i3, " COULD NOT BE DETERMINED. PLEASE, REPORT THIS PROBLEM HERE: ", internal_error_report_link, ".", intern_error_text_end, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    # nocov end
                }
                code_mod[line_i3] <- base::paste0(
                    base::substr(x = code_mod[line_i3], start = 1, stop = ini_pos_i3 - 1L),
                    package_of_flagged[i3],
                    colons_i3,
                    base::substr(x = code_mod[line_i3], start = ini_pos_i3, stop = base::nchar(x = code_mod[line_i3], type = "chars", allowNA = FALSE, keepNA = NA)),
                    collapse = NULL,
                    recycle0 = FALSE
                )
                replaced_log <- TRUE
            }
        }
    }
    if(replaced_log == TRUE){
        # removal of the lines that appear when the submitted function has already been executed (R JIT compiler):
        # the deparse of a compiled function body starts with the "<bytecode: 0x...>" line, and can contain a "<environment: ...>" line
        # these lines are not R code and break the reconstruction below, so they are removed (the line numbers of the message table-like part, based on the initial analysis, are not affected because the removal is done after the modification)
        code_mod <- code_mod[ ! base::grepl(x = code_mod, pattern = "^\\s*<(bytecode|environment):", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)]
        # reconstruction of the modified function
        # warning: keep.source = TRUE and an explicit srcfile are required to keep the source references (attribute srcref) of the returned function, i.e., its comments and indentation when printed. With srcfile = NULL, or with keep.source = FALSE (default of Rscript), the returned function is printed deparsed: comments removed and code reindented
        code_mod_text <- base::paste0(code_mod, collapse = "\n", recycle0 = FALSE)
        tempo_parse <- base::try(expr = base::parse(text = code_mod_text, file = "", n = NULL, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = code_mod_text, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown"), silent = TRUE, outFile = base::getOption(x = "try.outFile", default = base::stderr()))
        # nocov start
        # codecov inactivated because it is an internal control of the modification, impossible to cover with argument values (a modified code that does not parse would indicate a replacement bug)
        if(base::inherits(x = tempo_parse, what = "try-error", which = FALSE) | base::length(x = tempo_parse) != 1){
            # the modified code does not parse: rare situation (e.g., a function name split across lines in a way the analyzer could not handle correctly, see the Details section of the roxygen header)
            if(export == TRUE){
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "THE MODIFIED CODE DOES NOT PARSE. THE MODIFIED FUNCTION CANNOT BE EXPORTED NOR RETURNED.\nPLEASE, REPORT THIS PROBLEM HERE: ", 
                    internal_error_report_link, 
                    ".", 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                # the submitted function is returned unchanged, with no error
                output <- x
            }
        }else{
        # nocov end
            output_fun <- base::eval(expr = tempo_parse[[1]], envir = base::environment(fun = x), enclos = base::environment(fun = x)) # environment of the submitted function, so that the functions used inside the modified function still resolve the same way
            if(export == TRUE){
                # the file name is the name of the submitted function with _safer added at the end (already determined in the other checkings section)
                tempo_content <- base::paste0(
                    tempo_fun_name,
                    "_safer <- ",
                    base::paste0(code_mod, collapse = "\n", recycle0 = FALSE),
                    collapse = NULL,
                    recycle0 = FALSE
                )
                base::writeLines(text = tempo_content, con = out_file_name, sep = "\n", useBytes = FALSE)
                # the message is printed with on.exit(), so that it is printed at the end of the function, after the messages issued earlier by the main code (all the message() calls of the main code are issued before this one)
                base::on.exit(expr = base::message(base::paste0("MODIFIED CODE EXPORTED IN:\n", base::normalizePath(path = base::paste0(out_path, "/", out_name, collapse = NULL, recycle0 = FALSE), winslash = "/", mustWork = FALSE), "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE), add = TRUE, after = TRUE)
            }else{
                output <- output_fun # the modified function is returned
            }
        }
    }
    # end modification of the basic functions
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
    if(export == TRUE & replaced_log == TRUE){
        base::return(base::invisible(x = NULL)) # the modified code is not returned but saved (export argument)
    }else{
        base::return(output) # NULL if no modification done
    }
    #### end output
}

