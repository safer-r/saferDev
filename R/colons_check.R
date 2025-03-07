#' @title colons_check
#' @description
#' Verify that all the functions used inside a function are preceeded by a package attribution. For instance: \code{base::mean()} and not mean(), or saferDev:::.base_op_check() and not .base_op_check(). Warning: does not check that the package is the good one. Use all_args_here() for that.
#' @param x a function name, written without quotes and brackets.
#' @param safer_check Single logical value. Perform some "safer" checks? If TRUE, checkings are performed before main code running (see https://github.com/safer-r): 1) correct lib_path argument value 2) required functions and related packages effectively present in local R lybraries and 3) R classical operators (like "<-") not overwritten by another package because of the R scope. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R package are not installed in the default directories because of lack of admin rights.  More precisely, lib_path is passed through the new argument of .libPaths() so that the new library paths are unique(c(new, .Library.site, .Library)). Warning: .libPaths() is restored to the initial paths, after function execution. Ignored if NULL (default) or if the safer_check argument is FALSE: only the pathways specified by the current .libPaths() are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>.". If NULL, converted into "".
#' @returns 
#' A table-like message indicating the missing :: or ::: or a message saying that everything seems fine.
#' 
#' Table-like: column 1, the line number in the function code (starting at the "<- function" line, i.e., without counting the #' header lines); column 2,  the function name; column 3, the code preceeding the function name.
#' 
#' With missing :: or :::, the message also indicates if internal functions are created inside the checked function code, since these functions cannot have :: or :::.
#' @details
#'Use the result to modify the code of the function like this: <PACKAGE>::<FUNCTION> (OR <PACKAGE>:::<FUNCTION> for function names starting by a dot)
#' 
#'More precisely, colons_check() verifies that all the strings before an opening bracket "(" are preceeded by "::" 
#' 
#'":::" are not checked per se, because incorrect writting, like saferDev::.colons_check_message() returns an error, and because base:::sum() is as ok as base::sum(). In the same manner, more than three colons are not checked because it returns an error.
#' 
#'Warning: the function cannot check function names written without brackets, like in the FUN argument of some functions, e.g., sapply(1:3, FUN = as.character).
#' 
#' The perl regex used to detect a function name is: "([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*\\(".
#' 
#' Currently,  all_args_here() cannot detect functions written between quotes, like "+"() or "rownames<-"(x, "a").
#' 
#' Function names preceeded by $ are not considered.
#'  
#' The following R functions are skipped: "function", "if", "for", "while", "repeat" and "else".
#' 
#' Most of the time, all_args_here() does not check inside comments, but some unexpected writting could dupe all_args_here(). Please, report here https://github.com/safer-r/saferDev/issues if it is the case.
#' 
#' The returned line numbers are indicative, depending on which source is checked. For instance, saferDev::report (compiled) has not the same line numbers as its source file (https://github.com/safer-r/saferDev/blob/main/R/report.R). Notably, compiled functions do not have comments anymore, compared to the same source function sourced into the working environment. In addition, the counting starts at the "<- function" line, i.e., without counting the #' header lines potentially present in source files.
#' 
#'Of note, during package creation, the devtools::check() command tells which functions where wrongly attributed to package. Example: 
#'     checking dependencies in R code ... WARNING
#'       '::' or ':::' import not declared from: 'sbase'
#'       Missing or unexported objects:
#'         'base::dev.off' 'base::graphics.off' 'base::hcl' 'base::par' 'base::read.table' 'saferGG::report'
#' @author \href{gael.millot@pasteur.fr}{Gael Millot}
#' @author \href{yushi.han2000@gmail.com}{Yushi Han}
#' @author \href{wanghaiding442@gmail.com}{Haiding Wang}  
#' @examples
#' colons_check(mean)
#' colons_check(colons_check)
#' source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") ; colons_check(test)
#' 
#' @export
colons_check <- function(
    x, 
    safer_check = TRUE, 
    lib_path = NULL, 
    error_text = ""
){
    # DEBUGGING
    # x = mean ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A"
    # function_name <- "mean" ; arg_user_setting = base::list(x = as.name(x = "mean"), lib_path = NULL , safer_check = TRUE, error_text = " INSIDE A")
    # x = .expand_R_libs_env_var ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A"
    # library(saferGraph) ; x = close2 ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" 
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\get_message.R") ; x = get_message ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: x = saferDev::get_message does not return the same number of code lines
    # library(saferDev) ; x = get_message ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: does not return the same number of code lines than the previsou example
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\colons_check.R") ; x = colons_check ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: x = saferDev::get_message does not return the same number of code lines
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") ; x = test ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: x = saferDev::get_message does not return the same number of code lines
    # function_name <- "colons_check" ; arg_user_setting = base::list(x = as.name(x = "test"), lib_path = NULL , safer_check = TRUE , error_text = " INSIDE A")
    # FUN1 <- function(x, y){ifelse(base::is.null(x = warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE))} ; x = FUN1 ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A"
    # function_name <- "colons_check" ; arg_user_setting = base::list(x = as.name(x = "FUN1"), lib_path = NULL , safer_check = TRUE , error_text = " INSIDE A")

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
    }
    # end evaluation of values if they are expression, call, etc.
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 2)), envir = base::parent.frame(n = 1))) # names of all the arguments
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

    ######## arg with no default values
    mandat_args <- base::c(
        "x"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", 
            base::paste0(mandat_args[tempo], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
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
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
    if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA with base::is.null()
        tempo_cat <- base::paste0(
            error_text_start, 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
            " CANNOT BE NULL:\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
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
        ) # no argument provided by the user can be empty non NULL object. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even is 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE AN EMPTY NON NULL OBJECT:\n", 
                base::paste0(tempo_arg_user_setting_eval[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
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
        ) # no argument provided by the user can be just made of NA. is.na(NULL) returns logical(0), the reason why base::length(x = x) > 0 is used # warning: all(x = x, na.rm = TRUE) but normally no NA because base::is.na() used here. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even is 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE MADE OF NA ONLY:\n", 
                base::paste0(arg_user_setting_names[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    if( ! (base::all(base::typeof(x = safer_check) == "logical", na.rm = TRUE) & base::length(x = safer_check) == 1)){ # no need to test NA because NA only already managed above and base::length(x = safer_check) == 1)
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).\nHERE IT IS:\n", 
            base::ifelse(test = base::length(x = safer_check) == 0 | base::all(safer_check == base::quote(expr = ), na.rm = TRUE) | base::all(safer_check == "", na.rm = TRUE), yes = "<NULL, \"\", EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(safer_check, collapse = "\n", recycle0 = FALSE)),
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end safer_check argument checking

    ######## check of lib_path
    # must be before any :: or ::: non basic package calling
    if(safer_check == TRUE){
        if( ! base::is.null(x = lib_path)){ #  is.null(NA) returns FALSE so OK.
            if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = TRUE)){ # na.rm = TRUE but no NA returned with typeof (typeof(NA) == "character" returns FALSE)
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\nHERE IT IS:\n", 
                    base::ifelse(test = base::length(x = lib_path) == 0 | base::all(lib_path == base::quote(expr = ), na.rm = TRUE), yes = "<NULL, EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(lib_path, collapse = "\n", recycle0 = FALSE)),
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
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
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
            }else{
                ini_lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
                base::on.exit(expr = base:::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
                base:::.libPaths(new = base::sub(x = lib_path, pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base:::.libPaths(new = ) add path to default path. BEWARE: base:::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
                lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            }
        }else{
            lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base:::.libPaths(new = lib_path) # or base:::.libPaths(new = base::c(base:::.libPaths(), lib_path))
        }
    }
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        saferDev:::.pack_and_function_check(
            fun = base::c(
                # functions required in this code
                "saferDev::arg_check", # also in internal functions
                # end functions required in this code
                # internal functions required in this code
                "saferDev:::.base_op_check", # also in internal functions
                "saferDev:::.functions_detect", # requires saferDev::arg_check, saferDev:::.extract_all_fun_names, saferDev:::.has_odd_number_of_quotes
                "saferDev:::.colons_check_message", # requires saferDev::arg_check, saferDev:::.noclean_functions
                # end internal functions required in this code
                # functions required in internal functions above (i.e., :::.FUNCTION_NAME), because presence not checked in internal functions
                "saferDev:::.extract_all_fun_names", # requires saferDev::arg_check
                "saferDev:::.has_odd_number_of_quotes", # from saferDev:::.functions_detect
                "saferDev:::.noclean_functions" # requires saferDev::arg_check, saferDev:::.has_odd_number_of_quotes
                # end functions required in internal functions above (i.e., :::.FUNCTION_NAME), because presence not checked in internal functions
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    if(safer_check == TRUE){
        saferDev:::.base_op_check(
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
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))
    # add as many lines as below, for each of your arguments of your function in development
    tempo <- saferDev::arg_check(data = x, class = "function", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    # lib_path already checked above
    # safer_check already checked above
    # error_text already checked above
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
    skipped_base <- base::c("function", "if", "for", "while", "repeat", "else") # skipped function
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
    out <- saferDev:::.functions_detect(
        x = x, 
        skipped_base = skipped_base, 
        arg_user_setting2 = arg_user_setting, 
        lib_path = lib_path, 
        error_text = embed_error_text
    )
    if( ! (base::all(base::typeof(x = out$fun_names) == "list", na.rm = TRUE) & base::all(base::typeof(x = out$fun_names_pos) == "list", na.rm = TRUE))){
        tempo_cat <- base::paste0(
            "INTERNAL ERROR 1 IN ", 
            intern_error_text_start,
            "out$fun_names AND out$fun_names_pos MUST BE TYPE list\nout$fun_names:\n", 
            out$fun_names, 
            "\nout$fun_names_pos:\n", 
            out$fun_names_pos, 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    # basic function names in x
    # selection of basic functions
    tempo.log <- base::lapply(X = out$fun_names, FUN = function(x){x %in% out$all_basic_funs}) #  are names basic functions used in x?
    in_basic_fun <- base::mapply(FUN = function(x, y){x[y]}, x = out$fun_names, y = tempo.log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    in_basic_fun_names_pos <-  base::mapply(FUN = function(x, y){x[y]}, x = out$fun_names_pos, y = tempo.log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    # end selection of basic functions
    if(base::length(x = base::unlist(x = in_basic_fun, recursive = TRUE, use.names = TRUE)) != 0){ # check if basic fun present
        # removal of string with empty function names
        tempo.log <- base::sapply(X = in_basic_fun, FUN = function(x){base::length(x = x) == 0}, simplify = TRUE, USE.NAMES = TRUE) # detection of string with empty function names
        in_basic_fun <- in_basic_fun[ ! tempo.log] # removal of empty string
        in_basic_fun_names_pos <- in_basic_fun_names_pos[ ! tempo.log]
        in_basic_code_line_nb <- out$code_line_nb[ ! tempo.log]
        if( ! (base::length(x = in_basic_fun) == base::length(x = in_basic_fun_names_pos) & base::length(x = in_basic_fun) == base::length(x = in_basic_code_line_nb))){
            tempo_cat <- base::paste0(
                "INTERNAL ERROR 2 IN ", 
                intern_error_text_start,
                "LENGTHS SHOULD BE IDENTICAL\nin_basic_fun: ", 
                base::length(x = in_basic_fun), 
                "\nin_basic_fun_names_pos: ", 
                base::length(x = in_basic_fun_names_pos), 
                "\nin_basic_code_line_nb: ", 
                base::length(x = in_basic_code_line_nb), 
                intern_error_text_end, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
        # end removal of string with empty function names
        in_basic_fun_uni <- base::unlist(x = base::unique(x = in_basic_fun, incomparables = FALSE), recursive = TRUE, use.names = TRUE) #  names of unique basic functions used in x
        # end basic function names in x
        # analyse of :: before basic functions in x
        if(base::length(x = in_basic_fun_uni) > 0){
            tempo <- saferDev:::.colons_check_message(
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
        }else{
            log_basic <- FALSE
            cat_basic <- NULL
        }
        # end analyse of :: before basic functions in x
    }else{
        log_basic <- FALSE
        cat_basic <- NULL
    }
    # other function names in x
    # selection of other functions
    tempo.log <- base::lapply(X = out$fun_names, FUN = function(x){ ! x %in% base::c(out$all_basic_funs, out$arg_user_setting$x)}) #  names of all the other functions used in x, except the one tested (arg_user_setting$x), because can be in error messages
    in_other_fun <- base::mapply(FUN = function(x, y){x[y]}, x = out$fun_names, y = tempo.log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    in_other_fun_names_pos <-  base::mapply(FUN = function(x, y){x[y]}, x = out$fun_names_pos, y = tempo.log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    # end selection of other functions
    if(base::length(x = base::unlist(x = in_other_fun, recursive = TRUE, use.names = TRUE)) != 0){ # check if other fun present
        # removal of string with empty function names
        tempo.log <- base::sapply(X = in_other_fun, FUN = function(x){base::length(x = x) == 0}, simplify = TRUE, USE.NAMES = TRUE) # detection of string with empty function names
        in_other_fun <- in_other_fun[ ! tempo.log] # removal of empty string
        in_other_fun_names_pos <- in_other_fun_names_pos[ ! tempo.log]
        in_other_code_line_nb <- out$code_line_nb[ ! tempo.log]
        if( ! (base::length(x = in_other_fun) == base::length(x = in_other_fun_names_pos) & base::length(x = in_other_fun) == base::length(x = in_other_code_line_nb))){
            tempo_cat <- base::paste0(
                "INTERNAL ERROR 3 IN ", 
                intern_error_text_start,
                "LENGTHS SHOULD BE IDENTICAL\nin_other_fun: ", 
                base::length(x = in_other_fun), 
                "\nin_other_fun_names_pos: ", 
                base::length(x = in_other_fun_names_pos), 
                "\nin_other_code_line_nb: ", 
                base::length(x = in_other_code_line_nb), 
                intern_error_text_end, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
        # end removal of string with empty function names
        in_other_fun_uni <- base::unlist(x = base::unique(x = in_other_fun, incomparables = FALSE), recursive = TRUE, use.names = TRUE) #  names of unique other functions used in x
        # end other function names in x
        # analyse of :: before other functions in x
        if(base::length(x = in_other_fun_uni) > 0){
            tempo <- saferDev:::.colons_check_message(
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
        }else{
            log_other <- FALSE
            cat_other <- NULL
        }
    }else{
        log_other <- FALSE
        cat_other <- NULL
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
    base::cat(base::paste0("\n\n", tempo_cat, "\n\n", collapse = NULL, recycle0 = FALSE), file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE)
    #### end main code

    #### output
    #### end output

    #### warning output
    #### end warning output
}


