#' @title Replacement Inside Quotes
#' @description
#' Replace each match of a pattern located between quotes (string literals and backquoted names of R code) by a replacement of the same length, and return the positions of the replacements.
#' @param string Single string, for instance R code, with lines separated by \code{"\\n"}.
#' @param pattern Single string: regex matching a single character, like \code{"\\\\("}.
#' @param no_regex_pattern Single string: the character matched by \code{pattern}, without regex escape, like \code{"("}. Used to check the replaced characters.
#' @param replacement Single string replacing each match of \code{pattern} between quotes. Must have the same number of characters as \code{no_regex_pattern}. Not a regex.
#' @param perl Single logical value. Use Perl regex in \code{pattern}?
#' @param lib_path Vector of character strings or \code{NULL}: the \code{lib_path} value of the calling exported function, only passed on to \code{arg_check()}. No default value.
#' @param error_text Single character string added to the error messages to name the calling functions (\code{embed_error_text} of the calling function). \code{NULL} is converted into \code{""}. No default value.
#' @returns A list containing:
#' \itemize{
#'   \item \code{string}: \code{string} with the matches of \code{pattern} between quotes replaced by \code{replacement}.
#'   \item \code{pos}: vector of the positions of the replaced matches. \code{NULL} if no match is between quotes. In that case, \code{string} is identical to the input string.
#' }
#' @details
#' \code{string} is read from left to right, as the R parser reads code: a quote (\verb{"}, \verb{'} or \verb{`}) opens a quoted part that only the same quote closes, a backslash inside quotes escapes the next character (e.g., \verb{\"} or \verb{\\\\}), a raw string (e.g., \code{r"(...)"} or \code{R"--[...]--"}) is only closed by its own closing delimiter, \code{#} outside quotes starts a comment, up to the end of the line, and the \code{\%...\%} operators are skipped. Thus, an apostrophe inside double quotes (\code{"it's"}), or a double quote inside simple quotes, opens nothing. A quoted part that is not closed goes up to the end of \code{string}. The matches inside comments are not replaced.
#'
#' Only the content of a quoted part is replaced, never its quotes or the delimiters of a raw string, so that the output string can be passed again to the function (e.g., to replace \code{(} after \code{)}).
#'
#' Warnings:
#' \itemize{
#'   \item \code{pattern} must match a single character.
#'   \item requires \code{saferDev::arg_check}.
#'   \item In the safer Backbone section \code{"######## check of the required functions from the required packages"}, add also \code{saferDev::arg_check} when checking for the presence of \code{.in_quotes_replacement}.
#' }
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @author Haiding Wang
#' @author Yushi Han
#' @keywords internal
.in_quotes_replacement <- function(
    string, 
    pattern, 
    no_regex_pattern, 
    replacement, 
    perl,
    lib_path, # required because of saferDev::arg_check()
    error_text # warning: in internal functions, error_text without default value returns a R classical non traced error message (specific of internal functions since classical functions are error_text = "")
){
    # EXAMPLE (because the CRAN does not accept examples for unexported functions)
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R")
    # .in_quotes_replacement(string = paste(deparse(test), collapse = ""), pattern = "\\)", no_regex_pattern = ")", replacement = " ", perl = TRUE, lib_path = NULL, error_text = " INSIDE P1::F1")
    # .in_quotes_replacement(string = 'paste0("IAGE((", paste0(1:3, collapse = " "), "A)B()")', pattern = "\\)", no_regex_pattern = ")", replacement = " ", perl = TRUE, lib_path = NULL, error_text = " INSIDE P1::F1")
    # DEBUGGING
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") ; string = paste(deparse(test), collapse = "") ; pattern = "\\)" ; no_regex_pattern = ")" ; replacement = " " ; perl = FALSE ; lib_path = NULL ; error_text = " INSIDE P1::F1"
    # string = 'paste0("IAGE((", paste0(1:3, collapse = " "), "A)B()")' ; pattern = "\\)" ; no_regex_pattern = ")" ; replacement = " " ; perl = FALSE ; lib_path = NULL ; error_text = " INSIDE P1::F1"
    # string = 'paste0("IAGE((", paste0(1:3, collapse = " "), "A)B()' ; pattern = "\\)" ; no_regex_pattern = ")" ; replacement = " " ; perl = FALSE ; lib_path = NULL ; error_text = " INSIDE P1::F1" # last is ) between strings because odds number in front of
    # string = '\"INTERNAL ERROR 4 IN \", function_name, \" OF THE \", package_name, \" PACKAGE\\nLENGTHS OF col1 (\", base::length(roc1()), \"), col2 (\", base::length(roc2), \"), AND col3 (\", base::length(roc3), \"), SHOULD BE EQUAL\\n\"' ; pattern = "," ; no_regex_pattern = "," ; replacement = " " ; perl = FALSE ; lib_path = NULL ; error_text = " INSIDE P1::F1"
    # string = ' "a" ; c("\\") ; paste0("I)", collapse = " ")' ; pattern = "\\)" ; no_regex_pattern = ")" ; replacement = " " ; perl = TRUE ; lib_path = NULL ; error_text = NULL

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
        "string", 
        "pattern", 
        "no_regex_pattern", 
        "replacement", 
        "perl",
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
        "string", 
        "pattern", 
        "no_regex_pattern", 
        "replacement", 
        "perl" 
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
        "string", 
        "pattern", 
        "no_regex_pattern", 
        "replacement", 
        "perl", 
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
    # but check already done in the main safer function
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions
    # not used here: no internal function called
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
    tempo <- saferDev::arg_check(data = string, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = pattern, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = no_regex_pattern, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = replacement, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = perl, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
    tempo_arg <- base::c(
        "string", 
        "pattern", 
        "no_regex_pattern", 
        "replacement"
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
    ######## end warning initiation

    ######## graphic device checking
    ######## end graphic device checking

    ######## other checkings
    if(base::all(string == pattern, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "ARGUMENTS string AND pattern CANNOT BE IDENTICAL.\nstring:\n", 
            base::paste0(string, collapse = "\n", recycle0 = FALSE), 
            "\npattern:\n", 
            base::paste0(pattern, collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    if(base::nchar(x = no_regex_pattern, type = "chars", allowNA = FALSE, keepNA = NA) != base::nchar(x = replacement, type = "chars", allowNA = FALSE, keepNA = NA)){
        tempo_cat <- base::paste0(
            error_text_start,  
            "ARGUMENTS no_regex_pattern AND replacement MUST HAVE THE SAME NUMBER OF CHARACTERS.\nno_regex_pattern (", 
            base::nchar(x = no_regex_pattern, type = "chars", allowNA = FALSE, keepNA = NA), 
            " characters):\n", 
            base::paste0(no_regex_pattern, collapse = "\n", recycle0 = FALSE), 
            "\nreplacement (", 
            base::nchar(x = replacement, type = "chars", allowNA = FALSE, keepNA = NA), 
            " characters):\n", 
            base::paste0(replacement, collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # detection of the characters of string that are between quotes
    # string is read from left to right, as the R parser does, so that each character is interpreted according to what precedes it:
    # - outside quotes, a quote (double, simple or backquote) opens a quoted part (string literal or backquoted name), which is only closed by the same quote. Thus, an apostrophe inside double quotes, or a double quote inside simple quotes, opens nothing
    # - inside quotes, a backslash escapes the next character, which therefore cannot close the quoted part. The escaped backslashes are thus correctly read: in the code "\\" (a string made of one backslash), the first backslash escapes the second one, and the last double quote closes the string
    # - outside quotes, r or R just before a double or simple quote opens a raw string, like r"(...)" or R"--[...]--", which is only closed by its own closing delimiter, like )" or ]--" (no escape inside raw strings)
    # only the content of a quoted part is between quotes, not its quotes or the delimiters of a raw string. Thus, the replacements never modify the quotes and the delimiters, and the output string is read in the same way as string when it is passed again to this function, to replace another pattern (e.g., ( after ) in all_args_here(): if the ) of the closing delimiter of r"(a)" was replaced, the raw string would not be closed anymore in the next reading)
    # - outside quotes, # opens a comment, up to the end of the line: quotes inside comments are not read
    # - outside quotes, % opens a special operator, like %in% or %%, closed by the next % of the line: quotes inside such operators are not read
    # a quoted part that is not closed goes up to the end of string
    # the quote characters and the backslash are written below with their unicode code points, so that the code of this function has no apostrophe and no escaped double quote
    double_quote <- "\u0022"
    simple_quote <- "\u0027"
    back_quote <- "\u0060"
    backslash <- "\u005c"
    chars <- base::strsplit(x = string, split = "", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]] # characters of string
    nb_chars <- base::length(x = chars)
    in_quotes_log <- base::logical(length = nb_chars) # TRUE for the characters between quotes (content of the quoted parts, without their quotes and the delimiters of raw strings)
    # positions of the only characters that can change the reading of string (the other characters are not read one by one)
    event_pos <- base::which(x = chars %in% base::c(double_quote, simple_quote, back_quote, backslash, "#", "%", "\n"), arr.ind = FALSE, useNames = TRUE)
    newline_pos <- base::which(x = chars == "\n", arr.ind = FALSE, useNames = TRUE)
    percent_pos <- base::which(x = chars == "%", arr.ind = FALSE, useNames = TRUE)
    quote_char <- "" # "" outside quotes, otherwise the quote that has opened the quoted part
    quote_start <- 0 # position of the opening quote
    skip_to <- 0 # the characters at positions lower or equal to skip_to are already read (escaped character, comment, raw string or special operator)
    for(i3 in base::seq_along(along.with = event_pos)){
        tempo_pos <- event_pos[i3]
        if(tempo_pos <= skip_to){
            next
        }
        tempo_char <- chars[tempo_pos]
        if(quote_char != ""){
            # inside quotes
            if(tempo_char == backslash){
                skip_to <- tempo_pos + 1 # escaped character
            }else if(tempo_char == quote_char){
                if(tempo_pos > quote_start + 1){
                    in_quotes_log[(quote_start + 1):(tempo_pos - 1)] <- TRUE # content of the quoted part, without its quotes
                }
                quote_char <- ""
            }
            # end inside quotes
        }else if(tempo_char == "#"){
            # comment, up to the end of the line
            tempo_end <- newline_pos[newline_pos > tempo_pos]
            if(base::length(x = tempo_end) == 0){
                skip_to <- nb_chars
            }else{
                skip_to <- tempo_end[1] - 1
            }
            # end comment
        }else if(tempo_char == "%"){
            # special operator, closed by the next % of the line
            tempo_end <- percent_pos[percent_pos > tempo_pos]
            tempo_newline <- newline_pos[newline_pos > tempo_pos]
            if(base::length(x = tempo_end) > 0){
                if(base::length(x = tempo_newline) == 0){
                    skip_to <- tempo_end[1]
                }else if(tempo_end[1] < tempo_newline[1]){
                    skip_to <- tempo_end[1]
                }
            }
            # end special operator
        }else if(tempo_char %in% base::c(double_quote, simple_quote, back_quote)){
            # opening quote or raw string
            raw_end <- 0 # position of the last character of the raw string. 0 if not a raw string
            raw_content_end <- 0 # position of the last character of the content of the raw string
            if(tempo_char != back_quote & tempo_pos > 1){
                if(chars[tempo_pos - 1] %in% base::c("r", "R")){
                    raw_log <- TRUE
                    if(tempo_pos > 2){
                        raw_log <- ! base::grepl(x = chars[tempo_pos - 2], pattern = "[[:alnum:]._]", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # r or R must not be the end of a longer name
                    }
                    if(raw_log == TRUE){
                        string_after <- base::substring(text = string, first = tempo_pos + 1, last = 1000000L)
                        open_delim <- base::regmatches(x = string_after, m = base::regexpr(pattern = "^-*[\\(\\[\\{]", text = string_after, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), invert = FALSE) # opening delimiter: dashes, if any, and an opening bracket
                        if(base::length(x = open_delim) == 1){
                            nb_dashes <- base::nchar(x = open_delim, type = "chars", allowNA = FALSE, keepNA = NA) - 1
                            close_delim <- base::paste0(
                                base::c(")", "]", "}")[base::match(x = base::substr(x = open_delim, start = nb_dashes + 1, stop = nb_dashes + 1), table = base::c("(", "[", "{"), nomatch = NA_integer_, incomparables = NULL)], 
                                base::strrep(x = "-", times = nb_dashes), 
                                tempo_char, 
                                collapse = NULL, 
                                recycle0 = FALSE
                            ) # closing delimiter, like )" or ]--"
                            content_start <- tempo_pos + nb_dashes + 2 # position of the first character after the opening delimiter
                            tempo_close <- base::as.vector(x = base::regexpr(pattern = close_delim, text = base::substring(text = string, first = content_start, last = 1000000L), ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), mode = "any") # position of the closing delimiter after the opening one, -1 if absent
                            if(tempo_close == -1){
                                raw_end <- nb_chars # raw string not closed
                                raw_content_end <- nb_chars
                            }else{
                                raw_end <- content_start + tempo_close + base::nchar(x = close_delim, type = "chars", allowNA = FALSE, keepNA = NA) - 2
                                raw_content_end <- content_start + tempo_close - 2 # last character before the closing delimiter
                            }
                        }
                    }
                }
            }
            if(raw_end > 0){
                if(raw_content_end >= content_start){
                    in_quotes_log[content_start:raw_content_end] <- TRUE # content of the raw string, without its delimiters
                }
                skip_to <- raw_end
            }else{
                quote_char <- tempo_char
                quote_start <- tempo_pos
            }
            # end opening quote or raw string
        }
        # a backslash or a new line outside quotes changes nothing
    }
    if(quote_char != ""){
        if(quote_start < nb_chars){
            in_quotes_log[(quote_start + 1):nb_chars] <- TRUE # quoted part not closed: content up to the end of string
        }
    }
    # end detection of the characters of string that are between quotes
    # replacement of the matches of pattern between quotes
    string_out <- string
    tempo_match <- base::as.vector(x = base::gregexpr(pattern = pattern, text = string, ignore.case = FALSE, perl = perl, fixed = FALSE, useBytes = FALSE)[[1]], mode = "any") # position of the first character of each match of pattern in string, -1 if no match
    tempo_match <- tempo_match[tempo_match > 0]
    pos <- base::as.numeric(x = tempo_match[in_quotes_log[tempo_match]]) # matches between quotes
    for(i3 in base::seq_along(along.with = pos)){
        base::substr(x = string_out, start = pos[i3], stop = pos[i3] + base::nchar(x = replacement, type = "chars", allowNA = FALSE, keepNA = NA) - 1) <- replacement
    }
    if(base::length(x = pos) == 0){
        pos <- NULL
    }
    # end replacement of the matches of pattern between quotes
    if( ! base::is.null(x = pos)){
        tempo <- base::substring(text = string, first =  pos, last =  pos)
        if( ! base::all(base::unique(x = tempo, incomparables = FALSE) == no_regex_pattern, na.rm = TRUE)){
            # possible only if pattern and no_regex_pattern do not describe the same character
            tempo_cat <- base::paste0(
                "INTERNAL ERROR 1 IN ", 
                intern_error_text_start, 
                "ARGUMENT no_regex_pattern NOT CORRECTLY DETECTED\nno_regex_pattern: \"", 
                no_regex_pattern, 
                "\"\nREPLACED CHARACTERS IN string ARGUMENT:\n", 
                base::paste0(tempo, collapse = "\n", recycle0 = FALSE), 
                "\n\nCHECK THE pattern, no_regex_pattern AND replacement ARGUMENTS AND ", 
                intern_error_text_end, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    #### end main code

    #### warning output
    #### end warning output

    #### output
    output <- base::list(string = string_out, pos = pos)
    base::return(output)
    #### end output

}

