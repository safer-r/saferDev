#' @title Functions Detection
#' @description
#' Detect the names of the functions called in the code of a function.
#' @param x Function to analyze.
#' @param skipped_base Vector of strings: names not reported (e.g., \code{"function"} or \code{"if"}).
#' @param arg_user_setting2 List of the arguments written in the call of the calling function (\code{arg_user_setting} of the calling function). Its \code{x} compartment, the name of the analyzed function, is used in the messages and for the recursive calls (see \code{fun_names_internal} below).
#' @param lib_path Vector of character strings or \code{NULL}: the \code{lib_path} value of the calling exported function, only passed on to \code{arg_check()} (directly or through the internal functions called). No default value.
#' @param error_text Single character string added to the error messages to name the calling functions (\code{embed_error_text} of the calling function). \code{NULL} is converted into \code{""}. No default value.
#' @returns
#' A list:
#' \itemize{
#'   \item \code{code}: vector of strings, the lines of the printed code of \code{x}, with the runs of spaces collapsed into one space, and the leading space and the comments removed.
#'   \item \code{all_basic_funs}: vector of strings, names of all the objects of the basic R environments of the search path (packages stats, graphics, grDevices, utils, datasets, methods and base, and Autoloads), which must all be attached, otherwise an error is returned.
#'   \item \code{fun_names}: list of vectors of strings, the names followed by \code{(} in each line of \code{code}, and the names of called functions ending a line, their \code{(} being on a next line (names given as calls by the parse data or, if the printed code does not parse, any name ending a line followed by a line starting with \code{(}), except the names of \code{skipped_base}. Only lines with names are present, the compartment names indicating the line numbers (e.g., \code{"c3"} for line 3).
#'   \item \code{fun_names_pos}: list as \code{fun_names}, with the position of the first character of each name in its line.
#'   \item \code{code_line_nb}: vector of integers, the line numbers of the compartments of \code{fun_names}.
#'   \item \code{arg_user_setting}: \code{arg_user_setting2}, with \code{x} written \code{"pkg::fun()"} if \code{x} is written \code{pkg::fun} in the call.
#'   \item \code{fun_names_skip}: list as \code{fun_names}, with \code{TRUE} for the names not to check: names inside a formula or inside the arguments of a call of \code{quote()}, \code{bquote()}, \code{expression()}, \code{alist()} or \code{substitute()} (written without package or after \code{base::} or \code{base:::}, possibly between quotes, also with the value piped into it), except the code that R evaluates (arguments of the \code{.()} and, if \code{splice} is \code{TRUE} or \code{T}, \code{..()} calls of \code{bquote()}, \code{where} of \code{bquote()} and \code{env} of \code{substitute()}, none if R cannot match their arguments), and name of the function called by \code{.Internal()} (e.g., \code{toupper} in \code{.Internal(toupper(x))}).
#'   \item \code{fun_names_internal}: list as \code{fun_names}, with \code{TRUE} for the calls of internal functions inside their scope: formal arguments of \code{x} and name of \code{x} (\code{x} compartment of \code{arg_user_setting2}, possibly written \code{pkg::name} or followed by \code{()}, if the function of this name found from the environment of \code{x} is \code{x}: recursive calls) in the whole code of \code{x}, formal arguments of a function defined in \code{x} inside this definition (not for a definition in quoted code, see \code{fun_names_skip}), and names assigned a function definition or a \code{utils::getFromNamespace()} result (\code{<-}, \code{=}, \code{->}) inside the innermost function definition containing the assignment (the innermost one outside the quoted code if the assignment is not quoted, none if this definition is in quoted code, and the whole code of \code{x} for \code{<<-}, \code{->>}).
#' }
#'
#' \code{NULL}, with a warning, if \code{x} is a primitive function (no R code), or if the code of \code{x} has no name followed by \code{(} other than the names of \code{skipped_base}.
#' @details
#' The names between quotes (e.g., \code{"paste(x)"}) are reported. The comments are removed beforehand, the quotes being read as the R parser does (see \code{.in_quotes_replacement()}).
#'
#' The existence of the reported functions is not checked.
#'
#' The internal functions, the formulas, the calls of \code{quote()}, \code{bquote()}, \code{expression()}, \code{alist()} and \code{substitute()}, and the \code{.Internal()} calls are found in the parse data of the printed code of \code{x}. If this code does not parse (e.g., an environment object inserted in the body of \code{x}), the formulas, these quoting calls and the \code{.Internal()} calls are not detected, and the internal functions are only the name of \code{x} and the names written \code{name <- function(} or \code{name <- utils::getFromNamespace(} at the beginning of a line, in the whole code of \code{x}.
#'
#' Warnings:
#' \itemize{
#'   \item Requires \code{saferDev::arg_check}, \code{.extract_all_fun_names}, \code{.in_quotes_replacement}.
#'   \item In the safer Backbone section \code{"######## check of the required functions from the required packages"}, add also these 3 functions when checking for the presence of \code{.functions_detect}.
#' }
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @author Haiding Wang
#' @author Yushi Han
#' @keywords internal
.functions_detect <- function(
    # in internal functions, all arguments are without value on purpose
    x, 
    skipped_base, 
    arg_user_setting2, 
    lib_path, # required because of saferDev::arg_check()
    error_text # warning: in internal functions, error_text without default value returns a R classical non traced error message (specific of internal functions since classical functions are error_text = "")
){
    # EXAMPLE (because the CRAN does not accept examples for unexported functions)
    # .functions_detect(x = test, arg_user_setting2 = base::list(x = as.name(x = "test")), skipped_base = c("function", "if", "for", "while", "repeat", "else"), lib_path = NULL, error_text = " INSIDE P1::F1")
    # DEBUGGING
    # x = x ; skipped_base = base::c("function", "if", "for", "while", "repeat", "else") ; arg_user_setting2 = arg_user_setting ; lib_path = lib_path ; error_text = ""
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\dev\\other\\test2.R") ; x = test2 ; skipped_base = base::c("function", "if", "for", "while", "repeat", "else") ; arg_user_setting2 = base::list(x = as.name(x = "test2"), skipped_base = base::c("function", "if", "for", "while", "repeat", "else"), lib_path = NULL, error_text = " INSIDE P1::F1") ; lib_path = NULL ; error_text = " INSIDE P1::F1"
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\.github\\profile\\backbone.R") ; x = BACKBONE ; skipped_base = base::c("function", "if", "for", "while", "repeat", "else") ; arg_user_setting = base::list(x = as.name(x = "BACKBONE"), skipped_base = base::c("function", "if", "for", "while", "repeat", "else"), lib_path = NULL, error_text = " INSIDE P1::F1") ; lib_path = NULL ; error_text = " INSIDE P1::F1"
    # FUN1 <- function(x, y){middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, quote = FALSE, envir = base::parent.frame())} ; x = FUN1 ; skipped_base = base::c("function", "if", "for", "while", "repeat", "else") ; arg_user_setting = base::list(x = as.name(x = "FUN1"), skipped_base = base::c("function", "if", "for", "while", "repeat", "else"), lib_path = NULL, error_text = " INSIDE P1::F1") ; lib_path = NULL ; error_text = " INSIDE P1::F1"
    # FUN1 <- function(x, y){FUN2 <- function(x){x = 1}} ; x = FUN1 ; skipped_base = base::c("function", "if", "for", "while", "repeat", "else") ; arg_user_setting2 = base::list(x = as.name(x = "FUN1"), skipped_base = skipped_base, lib_path = NULL, error_text = " INSIDE P1::F1") ; lib_path = NULL ; error_text = " INSIDE P1::F1"
    # function_name <- ".functions_detect" ; arg_user_setting = base::list(x = as.name(x = "FUN1"), lib_path = NULL, error_text = "") ; arg_names <- c("x", "skipped_base", "arg_user_setting2", "lib_path", "error_text")

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
    # personalized for internal functions: lib_path and error_text are mandatory; safer_check does not exist in internal functions
    mandat_args <- base::c("lib_path", "error_text")
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
        "x", 
        "skipped_base", 
        "arg_user_setting2", 
        "lib_path"
        # "error_text" # inactivated because error_text already used above. Specific of my internal functions that error_text has no default value
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
        "skipped_base", 
        "arg_user_setting2"
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
        "skipped_base", 
        "arg_user_setting2", 
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
    # not used here
    ######## end safer_check argument checking

    ######## check of lib_path
    # check already done in the main safer function
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    # saferDev::arg_check is required here
    # .extract_all_fun_names is required here
    # .in_quotes_replacement is required here
    # but check already done in the main safer function
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .extract_all_fun_names <- utils::getFromNamespace(x = ".extract_all_fun_names", ns = "saferDev", pos = -1, envir = )
    .in_quotes_replacement <- utils::getFromNamespace(x = ".in_quotes_replacement", ns = "saferDev", pos = -1, envir = )

    ######## end escaping CRAN submission NOTE for internal functions

    ######## critical operator checking
    # check already done in the main safer function
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
    tempo <- saferDev::arg_check(data = x, class = NULL, typeof = NULL, mode = "function", length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = skipped_base, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = arg_user_setting2, class = NULL, typeof = NULL, mode = "list", length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    # lib_path already checked above
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
    # not used here
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
    # modification of arg_user_setting2$x for clean messages
    if(base::as.character(x = arg_user_setting2$x)[1] == "::" | base::as.character(x = arg_user_setting2$x)[1] == ":::"){
        arg_user_setting2$x <- base::paste0(base::as.character(x = arg_user_setting2$x)[2], base::as.character(x = arg_user_setting2$x)[1], base::as.character(x = arg_user_setting2$x)[3], "()", collapse = NULL, recycle0 = FALSE)
    }
    # end modification of arg_user_setting2$x for clean messages
    # primitive function (e.g., base::sum): no R code to check. Its printed code, like function (..., na.rm = FALSE)  .Primitive("sum"), is not its code: a function rebuilt from it would return the primitive function instead of its result
    if(base::is.primitive(x = x)){
        # warning and NULL returned, as for a function without call below
        tempo_warn <- base::paste0(
            base::sub(pattern = "^ERROR IN ", replacement = "FROM ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE),
            "THE TESTED FUNCTION ",
            arg_user_setting2$x,
            " IS A PRIMITIVE FUNCTION, WHICH HAS NO R CODE.\n\nNOTHING TO CHECK.",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::warning(tempo_warn, call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
        base::return(NULL)
    }
    # end primitive function
    # recovering the basic functions of R
    s <- base::c("package:stats", "package:graphics",  "package:grDevices", "package:utils", "package:datasets", "package:methods", "Autoloads", "package:base") # basic base::search() scope
    if(base::any( ! s %in% base::search(), na.rm = TRUE)){
        tempo <- s[ ! s %in% base::search()]
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE default base::search() SCOPE OF R HAS CHANGED.\nTHE PROBLEM IS:\n",
            base::paste0(tempo, collapse = "\n", recycle0 = FALSE), 
            "\nTHE FUNCTION CANNOT WORK WITH ",
            base::ifelse(test = base::length(x = tempo) == 1, yes = "THIS NAME SPACE DETACHED. PLEASE, ATTACH IT.", no = "THESE NAME SPACES DETACHED. PLEASE, ATTACH THEM."), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    fun <- base::unlist(x = base::sapply(X = s, FUN = function(x){base::ls(name = x, pos = -1L, envir = , all.names = TRUE, pattern = , sorted = TRUE)}, simplify = TRUE, USE.NAMES = TRUE), recursive = TRUE, use.names = TRUE) # all the basic functions of R in all the scope
    # end recovering the basic functions of R
    # recovering the input function string
    code <- utils::capture.output(x, file = NULL, append = FALSE, type = NULL, split = FALSE) # no lines must be removed because it is to catch the lines of the full code
    code_print <- code # printed code of x, not modified (lines of the parse data below)
    code_line_nb <- 1:base::length(x = code)
    # code <- base::paste0(code, collapse = " \\n ") # recovering as single string separated by \\n (and not \n to avoid the eval(\n) when printing the error message)
    code <- base::gsub(x = code, pattern = " +", replacement = " ", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # removal of multiple spaces
    code <- base::sub(x = code, pattern = "^ +", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # removal of multiple spaces in the beginning od strings
    # end recovering the input function string

    # removal of empty lines
    empty_line_log <- base::grepl(x = code, pattern = "^\\s*$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    # end removal of empty lines

    # removal of comments
    # the # between quotes (string literals and backquoted names, possibly over several lines) are replaced by a space in a copy of the code, made of all the lines separated by \n, so that .in_quotes_replacement() reads the code as the R parser does (an apostrophe inside double quotes, or a # inside a comment, are not taken as quotes). In each line of this copy, the first remaining # starts a comment, which is removed from the line of code
    tempo <- .in_quotes_replacement(
        string = base::paste0(code, collapse = "\n", recycle0 = FALSE), 
        pattern = "#", 
        no_regex_pattern = "#", 
        replacement = " ", 
        perl = FALSE, 
        lib_path = lib_path, 
        error_text = embed_error_text
    )
    line_nchar <- base::nchar(x = code, type = "chars", allowNA = FALSE, keepNA = NA)
    line_start <- base::cumsum(x = base::c(1, line_nchar[-base::length(x = line_nchar)] + 1)) # position of the first character of each line in the copy (+ 1 for each \n)
    code_wo_quoted_hash <- base::substring(text = tempo$string, first = line_start, last = line_start + line_nchar - 1) # lines of the copy
    comment_pos <- base::as.vector(x = base::regexpr(pattern = "#", text = code_wo_quoted_hash, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), mode = "any") # position of the comment in each line, -1 if no comment
    code[comment_pos > 0] <- base::substr(x = code[comment_pos > 0], start = 1, stop = comment_pos[comment_pos > 0] - 1) # removal of the comments
    comment_line_log <- comment_pos > 0 & base::grepl(x = code, pattern = "^\\s*$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # lines only made of a comment
    # no test for empty code here because code style contains "function" as fisrt line of code
    # end removal of comments
    # parse data of the printed code of x: internal functions, formulas, quoting calls and .Internal() calls
    # the attribute lines printed after the code (e.g., "<bytecode: 0x...>", "<environment: ...>") are not R code: they are replaced by empty lines, so that the line numbers of the parse data remain the ones of the printed code
    code_parse <- code_print
    code_parse[base::grepl(x = code_parse, pattern = "^<(bytecode|environment|srcref): ", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)] <- ""
    code_parse_text <- base::paste0(code_parse, collapse = "\n", recycle0 = FALSE)
    # warning: R only stores the parse data when the option keep.parse.data is TRUE (see lambda_check()). The option is set here and restored on exit
    oop <- base::options(keep.parse.data = TRUE)
    base::on.exit(expr = base::options(oop), add = TRUE, after = TRUE)
    pd <- base::tryCatch(
        expr = utils::getParseData(x = base::parse(file = "", n = 1, text = code_parse_text, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = code_parse_text, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown"), includeText = NA), # n = 1: only the first expression, i.e., the function, is parsed, so that the attributes printed after the code of a function (e.g., attr(,"name") and its value) do not prevent the parsing. Warning: an explicit srcfile is required, because srcfile = NULL removes the source references of the parsed code, so that utils::getParseData() returns NULL
        error = function(e){NULL}, # printed code that does not parse (e.g., an environment object, printed <environment>, inserted in the body of x): see below
        finally = 
    )
    internal_fun_names <- base::character(length = 0) # names of the internal functions: formal arguments of x and of the functions defined inside x, names assigned a function definition or a utils::getFromNamespace() result, and x itself (a name can be present several times, with different scopes)
    scope_line1 <- base::numeric(length = 0) # extent of the code where each name of internal_fun_names designates the internal function (scope): line and column of its first and last characters (0, 0, Inf and Inf for the whole code of x)
    scope_col1 <- base::numeric(length = 0)
    scope_line2 <- base::numeric(length = 0)
    scope_col2 <- base::numeric(length = 0)
    skip_line1 <- base::integer(length = 0) # extents of the tokens of the code that R does not evaluate as written (states U, B and S below), whose names are not checked
    skip_col1 <- base::integer(length = 0)
    skip_line2 <- base::integer(length = 0)
    skip_col2 <- base::integer(length = 0)
    internal_call_pos <- base::character(length = 0) # "<line>:<column>" of the function names written as argument of .Internal()
    call_end_lines <- NULL # lines ending with the name of a called function whose ( is on a next line (NULL if unknown, i.e., printed code that does not parse)
    if( ! base::is.null(x = pd)){
        pd <- pd[base::order(pd$line1, pd$col1, na.last = TRUE, decreasing = FALSE, method = "auto"), ] # rows in the order of the code
        # conversion of the col1 and col2 columns of the parse data into character positions of the printed lines: utils::getParseData() counts a TAB as a jump to the next multiple of 8
        tab_lines <- base::which(x = base::grepl(x = code_parse, pattern = "\t", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), arr.ind = FALSE, useNames = TRUE)
        for(i3 in base::seq_along(along.with = tab_lines)){
            tempo_chars <- base::strsplit(x = code_parse[tab_lines[i3]], split = "", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]]
            tempo_cols <- base::integer(length = base::length(x = tempo_chars)) # parse data column of each character of the line
            tempo_col <- 0L
            for(i4 in base::seq_along(along.with = tempo_chars)){
                tempo_col <- tempo_col + 1L
                if(tempo_chars[i4] == "\t"){
                    tempo_col <- ((tempo_col + 7L) %/% 8L) * 8L
                }
                tempo_cols[i4] <- tempo_col
            }
            tempo_log1 <- pd$line1 == tab_lines[i3]
            tempo_log2 <- pd$line2 == tab_lines[i3]
            pd$col1[tempo_log1] <- base::match(x = pd$col1[tempo_log1], table = tempo_cols, nomatch = NA_integer_, incomparables = NULL)
            pd$col2[tempo_log2] <- base::match(x = pd$col2[tempo_log2], table = tempo_cols, nomatch = NA_integer_, incomparables = NULL)
        }
        # end conversion of the col1 and col2 columns of the parse data into character positions of the printed lines
        # parse tree: the pd$id and pd$parent values are converted into pd row numbers
        id2row <- base::seq_len(length.out = base::nrow(x = pd))
        base::names(x = id2row) <- base::as.character(x = pd$id)
        parent_row <- base::as.vector(x = id2row[base::as.character(x = pd$parent)], mode = "any") # row of the parent of each row (NA for the top expressions)
        grand_parent_row <- parent_row[parent_row]
        children_rows <- base::split(x = id2row, f = parent_row, drop = FALSE) # rows of the children of each row, in the order of the code (list compartments named by the parent row)
        # names written without their backquotes or quotes (e.g., quote for `quote` or "quote")
        pd_names <- pd$text
        tempo_log <- base::substr(x = pd_names, start = 1, stop = 1) %in% base::c("\x60", "\"", "\x27")
        pd_names[tempo_log] <- base::substr(x = pd_names[tempo_log], start = 2, stop = base::nchar(x = pd_names[tempo_log], type = "chars", allowNA = FALSE, keepNA = NA) - 1L)
        # states of the code, determined from the top of the parse tree (code of x, evaluated) to the bottom:
        # - "E": code evaluated by R, whose names are checked
        # - "U": code not evaluated as written, whose names are not checked: formulas (whose text is used by the modeling functions, e.g., stats::offset(z) is not recognized by stats::terms()), arguments of quote(), expression() and alist(), and expr argument of substitute() (code used as data, e.g., plotmath labels, symbolic derivatives, deparsed code or formal arguments). Everything inside is in state U
        # - "B": expr argument of an evaluated bquote() ("S" when its splice argument is written TRUE or T), whose names are not checked, except the argument of the .() calls (and of the ..() calls in state S), evaluated by bquote() at any depth (state E). The where and splice arguments of bquote(), and the env argument of substitute(), are evaluated (state E)
        # the quoting functions are written without package or after base:: or base:::, possibly between backquotes or quotes (e.g., `quote`(x) or "quote"(x)), and their arguments are matched as R does (names, partial names, then positions; if the matching fails, R evaluates none of them: all of them are in state U), the value piped by the native pipe being an argument of the call (x |> f(y) is f(x, y), and x |> f(y, z = _) is f(y, z = x)). The .() and ..() markers of bquote() are written without package
        # state of each expression when the expression containing it is in state E (state_if_e), B or S (state_if_b), or S (state_if_s): NA when unchanged
        state_if_e <- base::rep(x = NA_character_, times = base::nrow(x = pd))
        state_if_b <- state_if_e
        state_if_s <- state_if_e
        # formulas: expression of each ~ (y ~ x, ~ x or `~`(y, x))
        tempo_rows <- base::which(x = pd$terminal == TRUE & pd$text %in% base::c("~", "`~`"), arr.ind = FALSE, useNames = TRUE)
        formula_rows <- parent_row[tempo_rows]
        tempo_log <- pd$token[tempo_rows] == "SYMBOL_FUNCTION_CALL"
        formula_rows[tempo_log] <- grand_parent_row[tempo_rows[tempo_log]] # `~`(y, x) writing: the formula is the call expression
        formula_log <- base::seq_len(length.out = base::nrow(x = pd)) %in% formula_rows # expressions of the formulas
        # calls of the quoting functions and of the markers of bquote()
        tempo_rows <- base::which(x = pd$token %in% base::c("SYMBOL_FUNCTION_CALL", "STR_CONST") & pd_names %in% base::c("quote", "bquote", "expression", "alist", "substitute", ".", ".."), arr.ind = FALSE, useNames = TRUE)
        for(i3 in base::seq_along(along.with = tempo_rows)){
            tempo_name <- pd_names[tempo_rows[i3]]
            tempo_fun_rows <- children_rows[[base::as.character(x = parent_row[tempo_rows[i3]])]] # children of the expression of the called function: its name, possibly preceded by the package and :: or :::
            tempo_fun_rows <- tempo_fun_rows[pd$token[tempo_fun_rows] != "COMMENT"] # comments excluded (e.g., base:: # comment<line break>quote(x))
            tempo_call_row <- grand_parent_row[tempo_rows[i3]] # call expression
            tempo_call_rows <- children_rows[[base::as.character(x = tempo_call_row)]] # children of the call expression: expression of the called function, "(", arguments separated by commas, ")"
            tempo_call_rows <- tempo_call_rows[pd$token[tempo_call_rows] != "COMMENT"]
            tempo_pack_log <- base::length(x = tempo_fun_rows) == 1L || (( ! tempo_name %in% base::c(".", "..")) && base::length(x = tempo_fun_rows) == 3L && pd$token[tempo_fun_rows[1]] == "SYMBOL_PACKAGE" && pd$text[tempo_fun_rows[1]] %in% base::c("base", "\x60base\x60") && pd$token[tempo_fun_rows[2]] %in% base::c("NS_GET", "NS_GET_INT")) # name written without package, or quoting function written after base:: or base::: (not after another package, $ or @)
            if(tempo_pack_log && base::length(x = tempo_call_rows) >= 3L && tempo_call_rows[1] == parent_row[tempo_rows[i3]] && pd$text[tempo_call_rows[2]] == "("){ # name followed by ( (not a string elsewhere, e.g., in c("quote"))
                # arguments of the call: row of the expression of the value of each argument (NA if empty) and name of the argument ("" if not named)
                tempo_values <- base::integer(length = 0)
                tempo_tags <- base::character(length = 0)
                tempo_value <- NA_integer_
                tempo_tag <- ""
                tempo_arg_rows <- tempo_call_rows[-base::c(1L, 2L)]
                for(i4 in base::seq_along(along.with = tempo_arg_rows)){
                    if(pd$token[tempo_arg_rows[i4]] == "EQ_SUB"){
                        tempo_tag <- pd_names[tempo_arg_rows[i4 - 1L]] # name written before =
                    }else if(pd$token[tempo_arg_rows[i4]] == "expr"){
                        tempo_value <- tempo_arg_rows[i4]
                    }else if(pd$text[tempo_arg_rows[i4]] == "," | (pd$text[tempo_arg_rows[i4]] == ")" & i4 > 1L)){ # end of an argument. Warning: compared through the text, because the token names of , and ) are quoted. A call without argument, like f(), has no argument
                        tempo_values <- base::c(tempo_values, tempo_value, recursive = FALSE, use.names = FALSE)
                        tempo_tags <- base::c(tempo_tags, tempo_tag, recursive = FALSE, use.names = FALSE)
                        tempo_value <- NA_integer_
                        tempo_tag <- ""
                    }
                }
                # call at the right of the native pipe: the piped value is the argument written _ (placeholder), or the first argument
                tempo_pipe_rows <- children_rows[[base::as.character(x = parent_row[tempo_call_row])]] # children of the expression containing the call: piped value, |> and call, if the call is at the right of a pipe
                tempo_pipe_rows <- tempo_pipe_rows[pd$token[tempo_pipe_rows] != "COMMENT"]
                if(base::length(x = tempo_pipe_rows) == 3L && pd$token[tempo_pipe_rows[2]] == "PIPE" && tempo_pipe_rows[3] == tempo_call_row){
                    tempo_log <- base::vapply(X = tempo_values, FUN = function(x){( ! base::is.na(x = x)) && base::length(x = children_rows[[base::as.character(x = x)]]) == 1L && pd$token[children_rows[[base::as.character(x = x)]][1]] == "PLACEHOLDER"}, FUN.VALUE = base::logical(length = 1), USE.NAMES = FALSE)
                    if(base::any(tempo_log, na.rm = TRUE)){
                        tempo_values[base::which(x = tempo_log, arr.ind = FALSE, useNames = TRUE)[1]] <- tempo_pipe_rows[1]
                    }else{
                        tempo_values <- base::c(tempo_pipe_rows[1], tempo_values, recursive = FALSE, use.names = FALSE)
                        tempo_tags <- base::c("", tempo_tags, recursive = FALSE, use.names = FALSE)
                    }
                }
                # states of the arguments
                if(tempo_name %in% base::c("bquote", "substitute")){
                    tempo_formals <- base::c("expr", "env") # formal arguments of substitute()
                    tempo_state <- "U" # state of the expr argument
                    if(tempo_name == "bquote"){
                        tempo_formals <- base::c("expr", "where", "splice")
                        tempo_state <- "B"
                    }
                    tempo_match <- base::match(x = tempo_tags, table = tempo_formals, nomatch = NA_integer_, incomparables = NULL) # exact names
                    tempo_exact <- tempo_match
                    tempo_args <- base::which(x = base::is.na(x = tempo_match) & tempo_tags != "", arr.ind = FALSE, useNames = TRUE)
                    for(i4 in base::seq_along(along.with = tempo_args)){
                        tempo_pos <- base::which(x = base::startsWith(x = tempo_formals, prefix = tempo_tags[tempo_args[i4]]) & ! base::seq_along(along.with = tempo_formals) %in% tempo_exact, arr.ind = FALSE, useNames = TRUE) # partial names, among the formal arguments not matched by an exact name
                        if(base::length(x = tempo_pos) == 1L && ! tempo_pos %in% tempo_match){
                            tempo_match[tempo_args[i4]] <- tempo_pos
                        }
                    }
                    tempo_pos <- base::setdiff(x = base::seq_along(along.with = tempo_formals), y = tempo_match) # formal arguments not matched by name, matched by position
                    tempo_args <- base::which(x = tempo_tags == "", arr.ind = FALSE, useNames = TRUE)
                    tempo_nb <- base::min(base::length(x = tempo_pos), base::length(x = tempo_args), na.rm = FALSE)
                    tempo_match[tempo_args[base::seq_len(length.out = tempo_nb)]] <- tempo_pos[base::seq_len(length.out = tempo_nb)]
                    if(base::anyNA(x = tempo_match, recursive = FALSE) || base::anyDuplicated(x = tempo_match, incomparables = FALSE) > 0L){ # matching error of R, which then evaluates no argument: argument not matched (unused argument, or name matching several formal arguments or a formal argument already matched by another partial name), or formal argument matched by several exact names
                        state_if_e[tempo_values[ ! base::is.na(x = tempo_values)]] <- "U"
                    }else{
                        tempo_matched <- tempo_formals[tempo_match] # formal argument of each argument
                        tempo_splice <- tempo_values[base::which(x = tempo_matched == "splice", arr.ind = FALSE, useNames = TRUE)]
                        tempo_splice <- children_rows[[base::as.character(x = tempo_splice[ ! base::is.na(x = tempo_splice)][1])]] # children of the value of splice (NULL if none)
                        if(base::length(x = tempo_splice) == 1L && pd$text[tempo_splice[1]] %in% base::c("TRUE", "T") && pd$token[tempo_splice[1]] %in% base::c("NUM_CONST", "SYMBOL")){
                            tempo_state <- "S" # splice argument written TRUE or T
                        }
                        tempo_log <- ( ! base::is.na(x = tempo_values)) & tempo_matched == "expr"
                        state_if_e[tempo_values[tempo_log]] <- tempo_state # the other arguments remain in state E
                    }
                }else if(tempo_name %in% base::c("quote", "expression", "alist")){
                    state_if_e[tempo_values[ ! base::is.na(x = tempo_values)]] <- "U"
                }else if(tempo_name == "."){
                    state_if_b[tempo_values[1][ ! base::is.na(x = tempo_values[1])]] <- "E" # first argument, the only one evaluated by bquote()
                }else{
                    state_if_s[tempo_values[1][ ! base::is.na(x = tempo_values[1])]] <- "E" # ..()
                }
            }
        }
        # states of all the rows, depth by depth in the parse tree
        depth <- base::rep(x = NA_integer_, times = base::nrow(x = pd))
        depth[base::is.na(x = parent_row)] <- 0L # top expressions
        tempo_depth <- 0L
        while(base::any(base::is.na(x = depth) & depth[parent_row] %in% tempo_depth, na.rm = TRUE)){
            tempo_depth <- tempo_depth + 1L
            depth[base::is.na(x = depth) & depth[parent_row] %in% (tempo_depth - 1L)] <- tempo_depth
        }
        state <- base::rep(x = "E", times = base::nrow(x = pd))
        for(i3 in base::seq_len(length.out = tempo_depth)){
            tempo_rows <- base::which(x = depth == i3, arr.ind = FALSE, useNames = TRUE)
            tempo_parent_state <- state[parent_row[tempo_rows]]
            tempo_state <- tempo_parent_state
            tempo_log <- tempo_parent_state == "E" & ! base::is.na(x = state_if_e[tempo_rows])
            tempo_state[tempo_log] <- state_if_e[tempo_rows][tempo_log]
            tempo_log <- tempo_parent_state %in% base::c("B", "S") & ! base::is.na(x = state_if_b[tempo_rows])
            tempo_state[tempo_log] <- state_if_b[tempo_rows][tempo_log]
            tempo_log <- tempo_parent_state == "S" & ! base::is.na(x = state_if_s[tempo_rows])
            tempo_state[tempo_log] <- state_if_s[tempo_rows][tempo_log]
            tempo_state[tempo_state == "E" & formula_log[tempo_rows]] <- "U" # formula in evaluated code (a formula in state B remains in state B: bquote() evaluates its .() calls)
            state[tempo_rows] <- tempo_state
        }
        tempo_rows <- base::which(x = pd$terminal == TRUE & state != "E", arr.ind = FALSE, useNames = TRUE) # tokens not evaluated as written
        skip_line1 <- pd$line1[tempo_rows]
        skip_col1 <- pd$col1[tempo_rows]
        skip_line2 <- pd$line2[tempo_rows]
        skip_col2 <- pd$col2[tempo_rows]
        # .Internal() calls: position of the name of the function called (e.g., toupper in .Internal(toupper(x)))
        tempo_rows <- base::which(x = pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% base::c(".Internal", "`.Internal`"), arr.ind = FALSE, useNames = TRUE)
        for(i3 in base::seq_along(along.with = tempo_rows)){
            tempo_call_rows <- children_rows[[base::as.character(x = grand_parent_row[tempo_rows[i3]])]] # children of the .Internal() call expression: expression of the .Internal name, "(", argument, ")"
            tempo_arg_row <- tempo_call_rows[pd$token[tempo_call_rows] == "expr"][2] # argument of .Internal(), i.e., the call of the internal function
            tempo_name_rows <- base::which(x = pd$token == "SYMBOL_FUNCTION_CALL" & grand_parent_row == tempo_arg_row, arr.ind = FALSE, useNames = TRUE)
            internal_call_pos <- base::c(internal_call_pos, base::paste0(pd$line1[tempo_name_rows], ":", pd$col1[tempo_name_rows], collapse = NULL, recycle0 = FALSE))
        }
        # lines ending with the name of a called function whose ( is on a next line (e.g., base::c(paste0<line break>(x))), for the trick below. Not the lines ending with a name that is not called, followed by an expression between brackets (e.g., y <- C<line break>(z), C being a variable)
        tempo_pd <- pd[pd$terminal == TRUE & pd$token != "COMMENT", , drop = FALSE]
        tempo_pd <- tempo_pd[base::order(tempo_pd$line2, tempo_pd$col2, na.last = TRUE, decreasing = FALSE, method = "auto"), , drop = FALSE]
        tempo_log <- ! base::duplicated(x = tempo_pd$line2, incomparables = FALSE, fromLast = TRUE) # last token of each line
        call_end_lines <- tempo_pd$line2[tempo_log & tempo_pd$token == "SYMBOL_FUNCTION_CALL"]
        # internal functions: formal arguments of x and of the functions defined inside x (function or \), and names assigned a function definition or a utils::getFromNamespace() result, with <-, =, -> (or <<-, ->>), at any position. Warning: the names assigned another kind of value are not internal functions: in c <- 3 ; c(1), R skips the non function object and calls base::c()
        # scope of each internal function, i.e., the extent of the code where R finds it when running x: the definition of the function whose formal argument it is (formal arguments with their default values, and body), or, for an assigned name, the innermost function definition containing the assignment. The whole code of x for the formal arguments of x and the names assigned in its body (outside the functions defined inside x), and for the names assigned with <<- or ->>, which assign in the enclosing environments
        # a function definition of the code that R does not evaluate as written (states U, B and S above, e.g., in bquote()) creates no function when x runs: its formal arguments, and the names assigned inside it by the code not evaluated as written, are not internal functions, and it is not the scope of the assignments evaluated by R inside it (the .() calls of a function definition quoted by bquote() are evaluated by bquote() in the function that calls it, outside the quoted function)
        fun_def_rows <- parent_row[pd$token == "FUNCTION" | (pd$terminal == TRUE & pd$text == "\\")] # expressions of the function definitions (function or \). Warning: the \ token is compared through its text, because its token name is quoted
        def_rows <- base::c(fun_def_rows, grand_parent_row[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% base::c("getFromNamespace", "`getFromNamespace`")], recursive = FALSE, use.names = FALSE) # expressions of the function definitions and of the utils::getFromNamespace() calls
        target_rows <- base::integer(length = 0) # targets of the assignments of these expressions
        target_scope_rows <- base::integer(length = 0) # scope of each target: row of the innermost function definition containing the assignment (NA for the whole code of x)
        for(i3 in base::seq_along(along.with = def_rows)){
            tempo_row <- def_rows[i3]
            repeat{
                tempo_parent_rows <- children_rows[[base::as.character(x = parent_row[tempo_row])]] # the expression and its siblings
                if(base::length(x = tempo_parent_rows) != 3){
                    break
                }
                tempo_target_row <- NA_integer_
                if(pd$token[tempo_parent_rows[2]] %in% base::c("LEFT_ASSIGN", "EQ_ASSIGN") & tempo_parent_rows[3] == tempo_row){
                    tempo_target_row <- tempo_parent_rows[1] # name <- expression, or name = expression
                }else if(pd$token[tempo_parent_rows[2]] == "RIGHT_ASSIGN" & tempo_parent_rows[1] == tempo_row){
                    tempo_target_row <- tempo_parent_rows[3] # expression -> name
                }else if(pd$text[tempo_parent_rows[1]] != "("){ # warning: compared through the text, because the token name of ( is quoted
                    break # expression not assigned
                }
                if( ! base::is.na(x = tempo_target_row)){
                    tempo_scope_row <- NA_integer_ # <<- or ->>: whole code of x
                    if( ! pd$text[tempo_parent_rows[2]] %in% base::c("<<-", "->>")){
                        tempo_def_rows <- fun_def_rows # assignment in the code that R does not evaluate as written, possibly run later (e.g., by base::eval()): innermost function definition, whatever its state (none if this definition is itself not evaluated, see below)
                        if(state[parent_row[tempo_row]] == "E"){
                            tempo_def_rows <- fun_def_rows[state[fun_def_rows] == "E"] # assignment evaluated by R: innermost function definition evaluated by R (e.g., x for c in base::bquote(function(a) .(c <- function(...){0})))
                        }
                        tempo_scope_row <- parent_row[tempo_row] # assignment, then the expressions containing it, up to the innermost function definition
                        while( ! (base::is.na(x = tempo_scope_row) || tempo_scope_row %in% tempo_def_rows)){ # NA: top expression reached
                            tempo_scope_row <- parent_row[tempo_scope_row]
                        }
                    }
                    if(base::is.na(x = tempo_scope_row) || state[tempo_scope_row] == "E"){ # a name assigned inside a function definition that R does not evaluate as written (e.g., quoted by bquote()) is not an internal function: this function does not exist when x runs, and the code that R evaluates inside it (.() of bquote()) is run outside it (e.g., not c in base::bquote(function(a){c <- function(z){z} ; .(c(1, y))}))
                        target_rows <- base::c(target_rows, tempo_target_row, recursive = FALSE, use.names = FALSE)
                        target_scope_rows <- base::c(target_scope_rows, tempo_scope_row, recursive = FALSE, use.names = FALSE)
                    }
                }
                tempo_row <- parent_row[tempo_row] # assignment or brackets: the enclosing expression is examined, for the chained assignments (f <- g <- function(y){y}) and the expressions between brackets (f <- (function(y){y}))
            }
        }
        tempo_rows <- base::which(x = pd$token %in% base::c("SYMBOL", "STR_CONST") & parent_row %in% target_rows, arr.ind = FALSE, useNames = TRUE) # names in the targets
        tempo_rows <- tempo_rows[base::lengths(x = children_rows[base::as.character(x = parent_row[tempo_rows])], use.names = FALSE) == 1L] # targets made of a single name (not lst$f or f[[1]])
        tempo_formal_rows <- base::which(x = pd$token == "SYMBOL_FORMALS" & state == "E", arr.ind = FALSE, useNames = TRUE) # formal arguments of the function definitions evaluated by R (e.g., not c in base::bquote(function(c) .(c(1, y))), whose c(1, y) is evaluated by bquote() with base::c())
        internal_fun_names <- pd_names[base::c(tempo_formal_rows, tempo_rows, recursive = FALSE, use.names = FALSE)] # names written without their quotes or backquotes
        tempo_scope_rows <- base::c(parent_row[tempo_formal_rows], target_scope_rows[base::match(x = parent_row[tempo_rows], table = target_rows, nomatch = NA_integer_, incomparables = NULL)], recursive = FALSE, use.names = FALSE)
        tempo_scope_rows[base::is.na(x = parent_row[tempo_scope_rows])] <- NA_integer_ # function x itself (top expression): whole code of x
        scope_line1 <- pd$line1[tempo_scope_rows]
        scope_col1 <- pd$col1[tempo_scope_rows]
        scope_line2 <- pd$line2[tempo_scope_rows]
        scope_col2 <- pd$col2[tempo_scope_rows]
        tempo_log <- base::is.na(x = tempo_scope_rows)
        scope_line1[tempo_log] <- 0
        scope_col1[tempo_log] <- 0
        scope_line2[tempo_log] <- Inf
        scope_col2[tempo_log] <- Inf
    }else{
        # printed code that does not parse: the formulas, the quoting calls and the .Internal() calls are not detected, and the internal functions are only the names assigned a function definition or a utils::getFromNamespace() result at the beginning of a line (name <- function( or name <- utils::getFromNamespace(), see the pattern below) and x itself, recognized in the whole code of x. Warning: colons_check() and all_args_here() run lambda_check() before, which stops when the printed code of x does not parse, so that this case is only met when .functions_detect() is called directly
        tempo_names <- base::sub(pattern = "^\\s*(([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*)\\s*<-[\\s\\r\\n]*(function|utils::getFromNamespace)[\\s\\r\\n]*\\(.*", replacement = "\\1", x = code, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) # ([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]* is a function name, written at the beginning of the line (comments already removed from code), and followed by <- and function( or utils::getFromNamespace(
        internal_fun_names <- tempo_names[tempo_names != code]
        scope_line1 <- base::rep(x = 0, times = base::length(x = internal_fun_names))
        scope_col1 <- scope_line1
        scope_line2 <- base::rep(x = Inf, times = base::length(x = internal_fun_names))
        scope_col2 <- scope_line2
    }
    # internal functions: x itself (recursive calls), when x is written as a name, pkg::name or pkg:::name, and when the function found by R from the environment of x with this name, as when running x, is x itself, recognized in the whole code of x. Warning: for a function object (e.g., x = fun1 in the args argument of base::do.call()), the calling functions write the "x" placeholder, which is not the name of x: the calls of a function named x are then not recursive calls
    tempo_name <- base::sub(pattern = "^[a-zA-Z][a-zA-Z0-9.]*:::?", replacement = "", x = base::sub(pattern = "\\(\\)$", replacement = "", x = base::as.character(x = arg_user_setting2$x)[1], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    if(base::grepl(x = tempo_name, pattern = "^([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
        tempo_fun <- base::tryCatch(
            expr = base::get0(x = tempo_name, envir = base::environment(fun = x), mode = "function", inherits = TRUE, ifnotfound = NULL), # function called by the name of x when running x (NULL if none). Warning: x is not a primitive function (see above), so that its environment is not NULL
            error = function(e){NULL}, # an object of this name cannot be evaluated (e.g., a promise that returns an error): the name of x does not designate x
            finally = 
        )
        if(base::identical(x = tempo_fun, y = x, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
            internal_fun_names <- base::c(internal_fun_names, tempo_name, recursive = FALSE, use.names = FALSE)
            scope_line1 <- base::c(scope_line1, 0, recursive = FALSE, use.names = FALSE)
            scope_col1 <- base::c(scope_col1, 0, recursive = FALSE, use.names = FALSE)
            scope_line2 <- base::c(scope_line2, Inf, recursive = FALSE, use.names = FALSE)
            scope_col2 <- base::c(scope_col2, Inf, recursive = FALSE, use.names = FALSE)
        }
    }
    # end parse data of the printed code of x: internal functions, formulas, quoting calls and .Internal() calls
    # trick to deal with end of lines between the name of the function and "("
    # a "(" is added at the end of the lines ending with the name of a called function whose "(" is on a next line (e.g., base::c(paste0<line break>(x))), so that this name is detected below. Warning: the "(" is only added to code_detect, a copy of code used to detect the names: code is returned without it, otherwise its brackets, read by all_args_here(), would be unbalanced
    # when the parse data are available, only the names of called functions (see call_end_lines above), whatever the next lines (empty lines or lines of comments can separate the name from its "("): otherwise, a name ending a line followed by a line starting with ( would be taken for a function call (e.g., C in y <- C<line break>(z), which would get the stats:: prefix). When the printed code does not parse, any name ending a line followed by a line starting with (
    code_detect <- code
    if(base::length(x = code) > 1){
        for(i3 in base::seq_len(length.out = base::length(x = code) - 1)){
            if(base::is.null(x = call_end_lines)){
                tempo_log <- base::grepl(pattern = "^\\s*\\(", x = code[i3 + 1], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # next line starting with (
            }else{
                tempo_log <- i3 %in% call_end_lines
            }
            if(tempo_log & base::grepl(pattern = "([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*$", x = code[i3], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){ # line ending with a name
                code_detect[i3] <- base::paste0(code[i3], "(", collapse = NULL, recycle0 = FALSE)
            }
        }
    }
    # end trick to deal with end of lines between the name of the function and "("
    # all function names in x
    pattern1 <- "([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*\\(" # pattern to detect a function name, a$fun( is removed in .noclean_functions()
    # ([a-zA-Z]|\\.[a-zA-Z._]) is for the begining of R function name: either any single alphabet character or a dot and any single alphabet character or dot (because .. is ok for function name) or underscore (because ._ is ok for function name). Starting "dot and num" or underscore is not authorized for function name
    # [a-zA-Z0-9._]* is The rest of the function name: any several of these characters or nothing
    # \\s*\\( means 0 or any space in perl, and an opening parenthesis
    # I could have used [\\s\\r\\n]* meaning any space or end of line or carriage return between the name and "(" but finally, another strategy used
        # - `this does not work well, as it does not take dots: "\\b[a-zA-Z\\.\\_]{1}[a-zA-Z0-9\\.\\_]+\\b", because of `\\b`: These are word boundaries. It ensures that the pattern matches only a complete word and not a part of a word.
        # - `[a-zA-Z.]{1}`: This portion of the pattern matches any uppercase letter (`A-Z`), lowercase letter (`a-z`), or a period (`.`) a single time ({1}).
        # - `[a-zA-Z0-9._]*`: This part of the pattern matches any uppercase letter (`A-Z`), lowercase letter (`a-z`), number (`0-9`), period (`.`), or underscore (`_`), repeated zero or more times (`*`). This represents the possible characters inside an R function name.
        # - `\\b`: Again, these are word boundaries, making sure the pattern captures the entire word and not just part of it.
        # -  not used: `(?= *\\()`: This is a lookahead assertion. It checks that the preceding pattern is followed by any spaces and a parenthesis (`\\(`), but doesn't include the spaces and parenthesis in the match. This is because, in R code, a function call is usually followed by a parenthesis, but the parenthesis is not part of the function name.

    fun_name <- base::list()
    fun_name_pos <- base::list()
    fun_name_skip <- base::list()
    fun_name_internal <- base::list()
    for(i1 in base::seq_along(along.with = code)){
        tempo <- .extract_all_fun_names(
            text = code_detect[i1], 
            pattern = pattern1, 
            lib_path = lib_path, 
            error_text = embed_error_text
        ) # recover all the function names, followed by "(", present in code, using a perl pattern
        fun_name <- base::c(fun_name, base::list(tempo$string))
        fun_name_pos <- base::c(fun_name_pos, base::list(tempo$pos))
        # names to skip (names of the code not evaluated as written, see the states above, and name of the function called by .Internal()) and names of the internal functions inside their scope. Their positions in the printed line are recovered first: the line i1 of code_detect is the line i1 of code_print with each run of spaces collapsed into its first space and the leading space removed (the removal of the comment and the "(" added at the end of code_detect do not change the positions of the other characters)
        tempo_chars <- base::strsplit(x = code_print[i1], split = "", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]]
        kept_pos <- base::which(x = ! (tempo_chars == " " & base::c(FALSE, tempo_chars[-base::length(x = tempo_chars)] == " ")), arr.ind = FALSE, useNames = FALSE) # positions of the characters kept by the collapsing of the runs of spaces
        kept_pos <- kept_pos[ ! (base::seq_along(along.with = kept_pos) == 1L & tempo_chars[kept_pos] %in% " ")] # removal of the leading space
        print_pos <- kept_pos[tempo$pos[base::seq_along(along.with = tempo$string)]] # positions of the names in code_print[i1] (tempo$pos is -1 when no name)
        tempo_skip <- base::vapply(X = print_pos, FUN = function(x){base::any(((i1 > skip_line1) | (i1 == skip_line1 & x >= skip_col1)) & ((i1 < skip_line2) | (i1 == skip_line2 & x <= skip_col2)), na.rm = TRUE)}, FUN.VALUE = base::logical(length = 1), USE.NAMES = FALSE) # inside a token not evaluated as written
        fun_name_skip <- base::c(fun_name_skip, base::list(tempo_skip | base::paste0(i1, ":", print_pos, collapse = NULL, recycle0 = TRUE) %in% internal_call_pos)) # or name of the function called by .Internal()
        tempo_internal <- base::vapply(X = base::seq_along(along.with = print_pos), FUN = function(x){base::any(internal_fun_names == tempo$string[x] & ((i1 > scope_line1) | (i1 == scope_line1 & print_pos[x] >= scope_col1)) & ((i1 < scope_line2) | (i1 == scope_line2 & print_pos[x] <= scope_col2)), na.rm = TRUE)}, FUN.VALUE = base::logical(length = 1), USE.NAMES = FALSE) # name of an internal function, inside its scope
        fun_name_internal <- base::c(fun_name_internal, base::list(tempo_internal))
    }
    # tempo <- base::lapply(code, FUN = function(x){.extract_all_fun_names(text = x, pattern = pattern1)})
    # removal of special functions
    tempo_log <- base::lapply(X = fun_name, FUN = function(x){ ! x %in% skipped_base})
    fun_name_wo_op <- base::mapply(FUN = function(x, y){x[y]}, x = fun_name, y = tempo_log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    fun_name_pos_wo_op <- base::mapply(FUN = function(x, y){x[y]}, x = fun_name_pos, y = tempo_log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    fun_name_skip_wo_op <- base::mapply(FUN = function(x, y){x[y]}, x = fun_name_skip, y = tempo_log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    fun_name_internal_wo_op <- base::mapply(FUN = function(x, y){x[y]}, x = fun_name_internal, y = tempo_log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    if(base::length(x = base::unlist(x = fun_name_wo_op, recursive = TRUE, use.names = TRUE)) == 0 | base::length(x = base::unlist(x = fun_name_pos_wo_op, recursive = TRUE, use.names = TRUE)) == 0){
        # warning and NULL returned, not an error: nothing to check is not a problem of the tested function
        tempo_warn <- base::paste0(
            base::sub(pattern = "^ERROR IN ", replacement = "FROM ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE),
            "THE TESTED FUNCTION ",
            arg_user_setting2$x,
            " IS EMPTY OF FUNCTION OR ONLY MADE OF COMMENTS OR ONLY MADE OF THE SKIPPED FUNCTIONS:\n",
            base::paste0(skipped_base, collapse = "\n", recycle0 = FALSE),
            "\n\nNOTHING TO CHECK.",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::warning(tempo_warn, call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
        base::return(NULL)
    }
    # end removal of special functions
    # removal of empty string
    tempo_log <- base::sapply(X = fun_name_wo_op, FUN = function(x){base::length(x = x) == 0}, simplify = TRUE, USE.NAMES = TRUE) # detection of string with empty function names
    fun_name_wo_op <- fun_name_wo_op[ ! tempo_log]
    fun_name_pos_wo_op <- fun_name_pos_wo_op[ ! tempo_log]
    fun_name_skip_wo_op <- fun_name_skip_wo_op[ ! tempo_log]
    fun_name_internal_wo_op <- fun_name_internal_wo_op[ ! tempo_log]
    code_line_nb <- code_line_nb[( ! tempo_log) & ( ! comment_line_log) & ( ! empty_line_log)]
    if( ! (base::length(x = fun_name_wo_op) == base::length(x = fun_name_pos_wo_op) & base::length(x = fun_name_wo_op) == base::length(x = code_line_nb))){
        # nocov start
        # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
        tempo_cat <- base::paste0("INTERNAL ERROR 2 IN ", intern_error_text_start,  "LENGTHS SHOULD BE IDENTICAL\nfun_name_wo_op: ",  base::length(x = fun_name_wo_op),  "\nfun_name_pos_wo_op: ",  base::length(x = fun_name_pos_wo_op),  "\ncode_line_nb: ",  base::length(x = code_line_nb),  intern_error_text_end,  collapse = NULL,  recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
    }else if(base::any(base::is.na(x = code_line_nb), na.rm = TRUE)){
        # nocov start
        # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
        tempo_cat <- base::paste0("INTERNAL ERROR 3 IN ", intern_error_text_start, "code_line_nb SHOULD NOT CONTAIN NA.\ncode_line_nb:\n", base::paste0(code_line_nb, collapse = "\n", recycle0 = FALSE), intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
    }else{
        # with that, now the code line of code is indicated in as compartment names
        base::names(x = fun_name_wo_op) <- base::paste0("c", code_line_nb, collapse = NULL, recycle0 = FALSE)
        base::names(x = fun_name_pos_wo_op) <- base::paste0("c", code_line_nb, collapse = NULL, recycle0 = FALSE)
        base::names(x = fun_name_skip_wo_op) <- base::paste0("c", code_line_nb, collapse = NULL, recycle0 = FALSE)
        base::names(x = fun_name_internal_wo_op) <- base::paste0("c", code_line_nb, collapse = NULL, recycle0 = FALSE)
    }
    # end removal of empty string
    test_log <- base::mapply(FUN = function(x, y){base::length(x = x) != base::length(x = y)}, x = fun_name_wo_op, y = fun_name_pos_wo_op, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
    if(base::any(test_log, na.rm = TRUE)){
        # nocov start
        # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
        tempo_cat <- base::paste0("INTERNAL ERROR 4 IN ", intern_error_text_start, "LENGTHS SHOULD BE IDENTICAL IN COMPARTMENTS ", base::paste0(base::which(x = test_log, arr.ind = FALSE, useNames = TRUE), collapse = ", ", recycle0 = FALSE), " OF fun_name_wo_op AND fun_name_pos_wo_op.", intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
    }
    # fun_name_wo_op_uni <- base::unlist(base::unique(fun_name_wo_op)) # in case
    # end all function names in x
    #### end main code

    #### warning output
    #### end warning output

    #### output
    output <- base::list(
        code = code, 
        all_basic_funs = fun, 
        fun_names = fun_name_wo_op, 
        fun_names_pos = fun_name_pos_wo_op, 
        code_line_nb = code_line_nb, 
        arg_user_setting = arg_user_setting2, 
        fun_names_skip = fun_name_skip_wo_op, 
        fun_names_internal = fun_name_internal_wo_op
    )
    base::return(output)
    #### end output
}

