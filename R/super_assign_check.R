#' @title Check That No Superassignment Is Used
#' @description
#' Checks that the superassignment operators \code{<<-} and \code{->>} are not used in a function. Returns an error with a table of the lines to rewrite if any, since these operators are not allowed in safer-r functions.
#' @param x Function to check, written without quotes and brackets (e.g., \code{fun1} or \code{pkg::fun1}).
#' @inheritParams is_function_here
#' @returns
#' If no superassignment operator is detected: a message saying that everything seems clean, and \code{NULL}, invisibly.
#'
#' Otherwise: an error, with a table-like message made of one row per reported operator (column 1: line number; column 2: symbol; column 3: code of the line).
#' @details
#' Why an error: \code{<<-} and \code{->>} assign into the first enclosing environment of the function where the name exists, or into the global environment. What they modify thus depends on where the function is defined, not on its code, and cannot be corrected automatically: rewrite the code, for instance by returning the value, or by using \code{base::assign()} with an explicit \code{envir} argument.
#'
#' Reported: every \code{<<-} and \code{->>} operator written in the submitted function, also when called as a function (e.g., \code{`<<-`(y, 1)} or \code{base::`<<-`(y, 1)}), including inside the functions defined in the submitted function (e.g., a \code{FUN} argument of \code{base::lapply()} or the handlers of \code{base::tryCatch()}).
#'
#' Not reported: the \code{<-}, \code{->} and \code{=} assignment operators, and the \code{<<-} or \code{->>} writings inside strings, comments or backquoted names (e.g., \code{c("<<-")} or \code{`a<<-b`}), apart from the calls above.
#'
#' Source references: line 1 is the line of the \code{function} keyword. Functions created with \code{options(keep.source = FALSE)} (default of Rscript) and functions of installed packages (e.g., \code{saferDev::colons_check}) have no source references (attribute \code{srcref}): their deparsed code is analyzed, without comments, with line numbers that differ from the source file, and with \code{->>} reported as \code{<<-}.
#' @seealso The code checkers of saferDev, in the recommended order of use: \code{\link{super_assign_check}}, \code{\link{lambda_check}}, \code{\link{colons_check}}, \code{\link{all_args_here}}, \code{\link{curly_bracket_check}}, \code{\link{true_false_check}}, \code{\link{for_loop_nb_check}} (see \code{vignette("saferDev")}).
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @examples
#' # Without source references ("Run examples" link, Rscript), line numbers and comments differ.
#' # See also https://safer-r.github.io/saferDev/articles/super_assign_check.html
#' fun1 <- function(x){ y <- x ; base::return(y) }
#' saferDev::super_assign_check(x = fun1, safer_check = TRUE, lib_path = NULL,
#'     error_text = "") # no superassignment: NULL returned, invisibly
#' fun2 <- function(x){ y <<- x ; base::return(y) }
#' base::try(expr = saferDev::super_assign_check(x = fun2, safer_check = TRUE, lib_path = NULL,
#'     error_text = ""), silent = FALSE,
#'     outFile = base::getOption(x = "try.outFile", default = base::stderr())) # error: <<- reported
#' # A <<- written in a string or in a comment is not an operator: not reported.
#' fun3 <- function(x){
#'     y <- "the <<- writing is just a string" # a <<- in a comment
#'     x
#' }
#' saferDev::super_assign_check(x = fun3, safer_check = TRUE, lib_path = NULL,
#'     error_text = "") # no superassignment: NULL returned, invisibly
#'
#' @export
super_assign_check <- function(
    x,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
){
    # DEBUGGING
    # x = fun1 ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A"
    # function_name <- "super_assign_check" ; arg_user_setting = base::list(x = as.name(x = "fun1"), lib_path = NULL, safer_check = TRUE, error_text = " INSIDE A")
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\super_assign_check.R") ; x = super_assign_check ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: x = saferDev::super_assign_check does not return the same number of code lines

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
    tempo_arg <- base::c(
        "x",
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
    tempo_arg <- base::c(
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
                "saferDev::arg_check",
                # end functions required in this code
                # internal functions required in this code
                "saferDev:::.base_op_check" # also in internal functions
                # end internal functions required in this code
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev", pos = -1, envir = )

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
    # x forwarded through the ... argument of another function (e.g., fun2 <- function(...){saferDev::super_assign_check(...)} called with x = fun1): base::match.call() writes x ..1 (or ..2, etc.) in arg_user_setting, instead of its writing in the call. This writing is recovered from the promise of x by base::substitute() (x is not reassigned before this line)
    if(base::is.symbol(x = arg_user_setting$x) && base::grepl(x = base::as.character(x = arg_user_setting$x), pattern = "^\\.\\.[0-9]+$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
        arg_user_setting$x <- base::substitute(expr = x, env = base::environment(fun = NULL))
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # attributes of x printed by R after its code (all the attributes except srcref, srcfile, wholeSrcref and comment, which print() does not show): the code of x is recovered below as printed, so that the printed attribute values would be analyzed as code of x, silently when they parse (e.g., a formula, a function or a call, which could even be run), or with the parse error below. Same message in super_assign_check(), lambda_check(), curly_bracket_check(), true_false_check() and for_loop_nb_check()
    tempo_attr <- base::setdiff(x = base::names(x = base::attributes(x = x)), y = base::c("srcref", "srcfile", "wholeSrcref", "comment"))
    if(base::length(x = tempo_attr) > 0){
        tempo_cat <- base::paste0(
            error_text_start,
            "THE FUNCTION OF THE x ARGUMENT CANNOT BE CHECKED: ITS ATTRIBUTES, PRINTED BY R AFTER ITS CODE, WOULD BE ANALYZED AS CODE.\nATTRIBUTES TO REMOVE BEFORE THE CHECK (E.G., base::attr(x = fun, which = \"note\") <- NULL):\n",
            base::paste0(tempo_attr, collapse = "\n", recycle0 = FALSE),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # recover the code of the checked function
    code <- utils::capture.output(x, file = NULL, append = FALSE, type = NULL, split = FALSE)
    # removal of the attribute lines printed for compiled functions (e.g., "<bytecode: 0x...>", "<environment: ...>", "<srcref: ...>"). Warning: the pattern cannot be simply a line starting by "<", because a valid R code line can start by "<" (binary operator continuation)
    code <- code[ ! base::grepl(x = code, pattern = "^<(bytecode|environment|srcref): ", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)]
    # warning: R only stores the parse data when the option keep.parse.data is TRUE (see lambda_check()). The option is set here and restored on exit
    oop <- base::options(keep.parse.data = TRUE)
    base::on.exit(expr = base::options(oop), add = TRUE, after = TRUE)
    code_text <- base::paste0(code, collapse = "\n", recycle0 = FALSE)
    pd <- base::tryCatch(
        expr = utils::getParseData(x = base::parse(file = "", n = NULL, text = code_text, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = code_text, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown"), includeText = NA), # warning: an explicit srcfile is required, because srcfile = NULL removes the source references of the parsed code, so that utils::getParseData() returns NULL (same writing as in lambda_check())
        error = function(e){e}, # code of x, as printed by R, that does not parse: error below
        finally =
    )
    if(base::inherits(x = pd, what = "error", which = FALSE)){
        # the printed code of x, without attributes (checked above), does not parse when an object that is not printed as R code is inserted in its body (e.g., an environment, printed <environment>). Same message in super_assign_check(), lambda_check(), curly_bracket_check(), true_false_check() and for_loop_nb_check()
        tempo_cat <- base::paste0(
            error_text_start,
            "THE FUNCTION OF THE x ARGUMENT CANNOT BE CHECKED: ITS CODE, AS PRINTED BY R, DOES NOT PARSE (E.G., AN OBJECT INSERTED IN ITS BODY THAT IS NOT PRINTED AS R CODE, LIKE AN ENVIRONMENT, PRINTED <environment>).\n\nERROR MESSAGE OF THE R PARSER:\n",
            base::conditionMessage(c = pd),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # tokens of the superassignment operators, sorted by source position. Strings, comments and backquoted names are other tokens (STR_CONST, COMMENT, SYMBOL), so that a <<- written inside them is never reported
    sa_rows <- base::which(x = (pd$token == "LEFT_ASSIGN" & pd$text == "<<-") | (pd$token == "RIGHT_ASSIGN" & pd$text == "->>"), arr.ind = FALSE, useNames = TRUE)
    # calls of the operator written as a function name, backquoted or between quotes (e.g., `<<-`(y, 1), base::`<<-`(y, 1) or "<<-"(y, 1)): the same superassignment, printed y <<- 1 when x has no source references (except after base::). Warning: the name must be the function of its call: followed by the ( of this call, i.e., a ( whose parent is the call expression, which is the parent of the expression of the name (not a string elsewhere, e.g., in c("<<-")), and not preceded by $ (not a component of a list, e.g., x$`<<-`(1))
    call_rows <- base::which(x = pd$token %in% base::c("SYMBOL_FUNCTION_CALL", "STR_CONST") & pd$text %in% base::c("\x60<<-\x60", "\"<<-\"", "\x27<<-\x27"), arr.ind = FALSE, useNames = TRUE)
    if(base::length(x = call_rows) > 0){
        term_rows <- base::which(x = pd$terminal == TRUE & pd$token != "COMMENT", arr.ind = FALSE, useNames = TRUE)
        term_rows <- term_rows[base::order(pd$line1[term_rows], pd$col1[term_rows], method = "radix", na.last = TRUE, decreasing = FALSE)] # terminal tokens in the order of the code
        call_log <- base::logical(length = base::length(x = call_rows))
        for(i1 in base::seq_along(along.with = call_rows)){
            tempo_pos <- base::match(x = call_rows[i1], table = term_rows, nomatch = NA_integer_, incomparables = NULL) # position of the name among the terminal tokens, never the first one (function keyword)
            tempo_next <- term_rows[tempo_pos + 1L] # NA if the name is the last token (e.g., function(x) "<<-")
            call_log[i1] <- base::isTRUE(x = pd$text[tempo_next] == "(" & pd$parent[tempo_next] == pd$parent[pd$id == pd$parent[call_rows[i1]]] & pd$text[term_rows[tempo_pos - 1L]] != "$")
        }
        sa_rows <- base::c(sa_rows, call_rows[call_log], recursive = FALSE, use.names = FALSE)
    }
    sa_rows <- sa_rows[base::order(pd$line1[sa_rows], pd$col1[sa_rows], method = "radix", na.last = TRUE, decreasing = FALSE)]
    findings_line <- pd$line1[sa_rows]
    findings_symbol <- pd$text[sa_rows]
    findings_symbol[pd$token[sa_rows] %in% base::c("SYMBOL_FUNCTION_CALL", "STR_CONST")] <- "<<-" # operator called as a function: symbol shown without its backquotes or quotes
    findings_code <- base::trimws(x = code[findings_line], which = "left", whitespace = "[ \\t]")
    # modification of arg_user_setting$x for clean messages: a single string "<name>()", with <name> the writing of x in the call (name, pkg::name, pkg:::name, or other expression deparsed on one line, like dots[[1L]][[1L]] through base::Map()), or "x" for a function object, which has no name (e.g., x = fun1 in the args argument of base::do.call())
    if(base::is.function(x = arg_user_setting$x)){
        arg_user_setting$x <- "x()"
    }else if(base::as.character(x = arg_user_setting$x)[1] == "::" | base::as.character(x = arg_user_setting$x)[1] == ":::"){
        arg_user_setting$x <- base::paste0(
            base::as.character(x = arg_user_setting$x)[2],
            base::as.character(x = arg_user_setting$x)[1],
            base::as.character(x = arg_user_setting$x)[3],
            "()",
            collapse = NULL,
            recycle0 = FALSE
        )
    }else if(base::is.symbol(x = arg_user_setting$x)){
        arg_user_setting$x <- base::paste0(
            arg_user_setting$x,
            "()",
            collapse = NULL,
            recycle0 = FALSE
        )
    }else{
        arg_user_setting$x <- base::paste0(
            base::paste0(base::trimws(x = base::deparse(expr = arg_user_setting$x, width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), which = "left", whitespace = "[ \\t]"), collapse = " ", recycle0 = FALSE),
            "()",
            collapse = NULL,
            recycle0 = FALSE
        )
    }
    # end modification of arg_user_setting$x for clean messages
    # error or message
    if(base::length(x = findings_line) != 0){
        tempo_cat <- base::paste0(
            error_text_start,
            "INSIDE ",
            base::as.character(x = arg_user_setting$x),
            ", THE SUPERASSIGNMENT OPERATORS <<- AND ->> ARE NOT ALLOWED IN SAFER-R FUNCTIONS:\nPLEASE, REWRITE THE CODE ACCORDINGLY.\n\n",
            "LINE\tSYMBOL\t\tCODE\n",
            base::paste0(base::paste0(findings_line, "\t", findings_symbol, "\t\t", findings_code, collapse = NULL, recycle0 = FALSE), collapse = "\n", recycle0 = FALSE),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    tempo_cat <- base::paste0(
        "AFTER RUNNING ",
        package_function_name,
        ".\nINSIDE ",
        base::as.character(x = arg_user_setting$x),
        ", EVERYTHING SEEMS CLEAN.",
        collapse = NULL,
        recycle0 = FALSE
    )
    base::message(base::paste0("\n\n", tempo_cat, "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE)
    #### end main code

    #### warning output
    #### end warning output

    #### output
    base::return(base::invisible(x = NULL)) # NULL if everything is clean, but invisible meaning that not shown but can be assigned
    #### end output

}
