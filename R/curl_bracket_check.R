#' @title Curl Brackets Check
#' @description
#' Verify that all the bodies of the \code{if}, \code{else}, \code{for}, \code{while}, \code{repeat} and \code{function} (including lambda \code{\\(x)}) constructs of a function are delimited by curly brackets \code{\{ \}}. For instance: \code{if(x > 1)\{ base::return(x) \}} and not \code{if(x > 1) base::return(x)}. The safer-r rule requires curly brackets for every body, even single-statement ones.
#' @param x a function name, written without quotes and brackets.
#' @param safer_check Single logical value. Perform some "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) correct \code{lib_path} argument value 2) required functions and related packages effectively present in local R libraries and 3) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns 
#' A table-like text message indicating the missing curly brackets \code{\{ \}} or a message saying that everything seems fine.
#' 
#' Table-like: column 1, the line number in the function code (starting at the \code{"<- function"} line, i.e., without counting the \code{#'} header lines); column 2, the construct (\code{if}, \code{else}, \code{for}, \code{while}, \code{repeat} or \code{function}); column 3, the code preceding the construct body.
#' @details
#' The result is used to modify the code inside the tested function like this: \code{\{ <CODE> \}}.
#' 
#' More precisely, \code{curl_bracket_check()} verifies that the first code element of the body of each \code{if}, \code{else}, \code{for}, \code{while}, \code{repeat} and \code{function} construct (after the \code{()} of the condition or of the arguments, when present) is a curly bracket \code{\{}. Comments are ignored during this verification.
#' 
#' In \code{else if} chains, an \code{else} part followed by an \code{if()} construct is not reported, because the chained \code{if()} is checked on its own.
#' 
#' Function bodies reduced to a single \code{.Primitive(...)} call are skipped, because primitive functions cannot be delimited by curly brackets. For instance, the body of \code{base::`+`} is \code{.Primitive("+")} and is not reported. Contrary to these ones, \code{UseMethod(...)} bodies are reported (e.g., \code{base::mean}).
#' 
#' Warning: \code{curl_bracket_check()} cannot check constructs written with backquotes, like \code{`if`(x > 1, base::return(x), base::return(0))}. Please, use the standard writing.
#' 
#' The returned line numbers are indicative, depending on which source is checked. For instance, \code{saferDev::colons_check} (compiled) has not the same line numbers as its source file. Notably, compiled functions do not have comments anymore, compared to the same source function sourced into the working environment. In addition, the counting starts at the \code{"<- function"} line, i.e., without counting the \code{#'} header lines potentially present in source files.
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @author Haiding Wang  
#' @author Yushi Han
#' @examples
#' # Warning: these examples may not work well when using the "Run examples" link 
#' # because of a particular environment. Please, copy-paste in a local environment.
#' # See also https://safer-r.github.io/saferDev/articles/curl_bracket_check.html
#' fun1 <- function(x){ if(x > 1){ base::return(x) } else { base::return(0) } }
#' saferDev::curl_bracket_check(fun1)
#' fun2 <- function(x){ if(x > 1) base::return(x) } ; saferDev::curl_bracket_check(fun2)
#' saferDev::curl_bracket_check(mean)
#' 
#' @export
curl_bracket_check <- function(
    x, 
    safer_check = TRUE, 
    lib_path = NULL, 
    error_text = ""
){
    # DEBUGGING
    # x = mean ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A"
    # function_name <- "curl_bracket_check" ; arg_user_setting = base::list(x = as.name(x = "mean"), lib_path = NULL, safer_check = TRUE, error_text = " INSIDE A")
    # x = .base_op_check ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # internal function with function body made of a single .Primitive() call
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\curl_bracket_check.R") ; x = curl_bracket_check ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: x = saferDev::curl_bracket_check does not return the same number of code lines

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
    # always executed: lib_path is always checked, whatever the value of safer_check
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
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check <- utils::getFromNamespace(x = ".pack_and_function_check", ns = "saferDev", pos = , envir = )
        .pack_and_function_check(
            fun = base::c(
                # functions required in this code
                "saferDev::arg_check",
                # end functions required in this code
                # internal functions required in this code
                "saferDev:::.base_op_check"
                # end internal functions required in this code
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev", pos = , envir = )

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
    # not required
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## reserved words
    ######## end reserved words

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ######## end warning initiation

    ######## graphic device checking
    ######## end graphic device checking

    ######## other checkings
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # recover the code of the checked function
    code <- utils::capture.output(x = x, file = NULL, append = FALSE, type = NULL, split = FALSE)
    # removal of the attribute lines printed for compiled functions (e.g., "<bytecode: 0x...>", "<environment: ...>", "<srcref: ...>"). Warning: the pattern cannot be simply a line starting by "<", because a valid R code line can start by "<" (binary operator continuation)
    code <- code[ ! base::grepl(x = code, pattern = "^<(bytecode|environment|srcref): ", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)]
    pd <- utils::getParseData(x = base::parse(text = base::paste0(code, collapse = "\n", recycle0 = FALSE), keep.source = TRUE))
    # terminal tokens (comments excluded), sorted by source position
    term_rows <- base::which(x = pd$terminal == TRUE & pd$token != "COMMENT", arr.ind = FALSE, useNames = TRUE)
    term_rows <- term_rows[base::order(x = pd$line1[term_rows], pd$col1[term_rows])]
    # positions in term_rows of the terminal tokens of the checked constructs (keyword tokens). Warning: the lambda function \(x) gives the "'\\\\'" token (and not the LAMBDA token) in the R versions checked so far
    kid_idx <- base::which(x = pd$token[term_rows] %in% base::c("IF", "FOR", "WHILE", "REPEAT", "FUNCTION", "LAMBDA", "'\\\\'"), arr.ind = FALSE, useNames = TRUE)
    findings_line <- base::integer(length = 0)
    findings_kind <- base::character(length = 0)
    findings_code <- base::character(length = 0)
    for(i in kid_idx){
        kid <- term_rows[i]
        if(pd$token[kid] != "REPEAT"){
            # search of the parentheses of the condition (if, for, while) or of the arguments (function), by scanning the terminal tokens located after the keyword in source order. Warning: the search cannot be done through the parse tree, because the forcond expr of for() owns the parentheses, and because the structure of lambda functions is different
            depth <- 0
            lp <- NULL
            rp <- NULL
            if(i < base::length(x = term_rows)){ # guard to avoid a descending sequence in the for() below if the keyword token were the last terminal token (unreachable with parseable R code, but secured)
                for(j in (i + 1):base::length(x = term_rows)){
                    if(pd$token[term_rows[j]] == "'('"){
                        depth <- depth + 1
                        if(depth == 1){
                            lp <- term_rows[j]
                        }
                    }else if(pd$token[term_rows[j]] == "')'"){
                        depth <- depth - 1
                        if(depth == 0){
                            rp <- term_rows[j]
                            break
                        }
                    }
                }
            }
            if(base::is.null(x = lp) == TRUE || base::is.null(x = rp) == TRUE){
                # nocov start
                # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values (R code without () after if, for, while and function cannot be parsed).
                tempo_cat <- base::paste0("INTERNAL ERROR 2 IN ", intern_error_text_start, "NO () FOUND AFTER THE KEYWORD TOKEN\ntoken: ", pd$token[kid], intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                # nocov end
            }
            body_start <- rp
        }else{
            body_start <- kid
        }
        body_start_pos <- base::which(x = term_rows == body_start, arr.ind = FALSE, useNames = TRUE)[1]
        if(body_start_pos == base::length(x = term_rows)){
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values (R code without body after if, else, for, while, repeat and function cannot be parsed).
            tempo_cat <- base::paste0("INTERNAL ERROR 3 IN ", intern_error_text_start, "NO BODY FOUND AFTER THE KEYWORD TOKEN\ntoken: ", pd$token[kid], intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        ft_token <- pd$token[term_rows[body_start_pos + 1]] # first terminal token (comments excluded) of the body
        ft_text <- pd$text[term_rows[body_start_pos + 1]]
        # detection of bodies made of a single .Primitive() call: these bodies are skipped, because primitive functions cannot be delimited by curly brackets. The .Primitive() call must be the whole body: the terminal token following the ) of the .Primitive() call must belong to another expr (otherwise, the body contains more code, like in function(x) .Primitive('+') + 1, which must be reported)
        prim_log <- FALSE
        if((pd$token[kid] == "FUNCTION" || pd$token[kid] == "LAMBDA" || pd$token[kid] == "'\\\\'") && ft_token == "SYMBOL_FUNCTION_CALL" && ft_text == ".Primitive"){
            prim_call_close_pos <- body_start_pos + 1
            depth <- 0
            k <- body_start_pos + 1 # the scan includes the .Primitive token itself, so that the ( of the call is found
            while(k <= base::length(x = term_rows)){
                if(pd$token[term_rows[k]] == "'('"){
                    depth <- depth + 1
                }else if(pd$token[term_rows[k]] == "')'"){
                    depth <- depth - 1
                    if(depth == 0){
                        prim_call_close_pos <- k
                        break
                    }
                }
                k <- k + 1
            }
            if(depth != 0){ # nocov start
                # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values (R code with unbalanced parentheses cannot be parsed).
                tempo_cat <- base::paste0("INTERNAL ERROR 5 IN ", intern_error_text_start, "UNBALANCED PARENTHESES DETECTED AFTER .Primitive", intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                # nocov end
            }else{
                # the whole body expr is the largest expr starting at the first body terminal token. If its end is the ) of the .Primitive() call, the body is exactly the .Primitive() call (e.g., function(x) .Primitive('+')). Otherwise, the body contains more code (e.g., function(x) .Primitive('+') + 1, which must be reported)
                body_term_pos1 <- pd$line1[term_rows[body_start_pos + 1]] * 1000000 + pd$col1[term_rows[body_start_pos + 1]]
                cand <- pd[base::which(x = pd$token == "expr" & pd$line1 * 1000000 + pd$col1 == body_term_pos1, arr.ind = FALSE, useNames = TRUE), ]
                body_end_pos <- cand[base::order(x = cand$line2 * 1000000 + cand$col2, decreasing = TRUE), ][1, "line2"] * 1000000 + cand[base::order(x = cand$line2 * 1000000 + cand$col2, decreasing = TRUE), ][1, "col2"]
                prim_log <- body_end_pos == pd$line1[term_rows[prim_call_close_pos]] * 1000000 + pd$col1[term_rows[prim_call_close_pos]]
            }
        }
        if(prim_log == FALSE){
            if(ft_token != "'{'"){
                findings_line <- base::c(findings_line, pd$line1[kid], recursive = TRUE, use.names = TRUE)
                findings_kind <- base::c(findings_kind, base::ifelse(test = pd$token[kid] %in% base::c("LAMBDA", "'\\\\'"), yes = "function", no = pd$text[kid]), recursive = TRUE, use.names = TRUE)
                findings_code <- base::c(findings_code, base::trimws(x = code[pd$line1[kid]], which = "left", whitespace = "[ \\t]"), recursive = TRUE, use.names = TRUE)
            }
        }
        # else branch of if(), detected through the parse tree (the else of an if is a sibling token of this if). Warning: an else followed by an if() is not reported, because the chained if() is checked on its own
        if(pd$token[kid] == "IF"){
            else_kids <- base::which(x = pd$parent == pd$parent[kid] & pd$token == "ELSE", arr.ind = FALSE, useNames = TRUE)
            if(base::length(x = else_kids) != 0){
                else_tok <- else_kids[1]
                else_pos <- base::which(x = term_rows == else_tok, arr.ind = FALSE, useNames = TRUE)[1]
                if(else_pos == base::length(x = term_rows)){
                    # nocov start
                    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values (R code without body after else cannot be parsed).
                    tempo_cat <- base::paste0("INTERNAL ERROR 4 IN ", intern_error_text_start, "NO BODY FOUND AFTER THE ELSE TOKEN", intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    # nocov end
                }
                ft2_token <- pd$token[term_rows[else_pos + 1]] # first terminal token (comments excluded) of the else body
                if(ft2_token != "IF"){
                    if(ft2_token != "'{'"){
                        findings_line <- base::c(findings_line, pd$line1[else_tok], recursive = TRUE, use.names = TRUE)
                        findings_kind <- base::c(findings_kind, "else", recursive = TRUE, use.names = TRUE)
                        findings_code <- base::c(findings_code, base::trimws(x = code[pd$line1[else_tok]], which = "left", whitespace = "[ \\t]"), recursive = TRUE, use.names = TRUE)
                    }
                }
            }
        }
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
    # message
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
        ".", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    if(base::length(x = findings_line) == 0){
        tempo_cat <- base::paste0(
            tempo_cat, 
            "\nINSIDE ", 
            base::as.character(x = arg_user_setting$x), 
            ", EVERYTHING SEEMS CLEAN.", 
            collapse = NULL, 
            recycle0 = FALSE
        )
    }else{
        tempo_cat <- base::paste0(
            "\nINSIDE ", 
            base::as.character(x = arg_user_setting$x), 
            ", SOME CURLY BRACKETS {} ARE MISSING AFTER:\n\n", 
            "LINE\tCONSTRUCT\t\tCODE\n", 
            base::paste0(base::paste0(findings_line, "\t", findings_kind, "\t\t", findings_code), collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
    }
    base::message(base::paste0("\n\n", tempo_cat, "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE)
    #### end main code

    #### warning output
    #### end warning output

    #### output
    #### end output
}
