
#' @title .colons_check_message
#' @description
#' Create the message for the colons_check() function.
#' @param list.fun List of names of all the functions.
#' @param list.fun.pos List of positions of first character of names of all the functions in ini.
#' @param line.nb Vector of integers of corresponding line numbers.
#' @param ini Vector of strings of the initial function code analyzed.
#' @param arg_user_setting List of arg user settings.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = "INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>".
#' @param text Either "BASIC" or "OTHER".
#' @param internal_fun_names Vector of strings of names of internal functions in the function code analyzed. Can be NULL
#' @returns
#'  A list:
#'  $output.cat: the message (string).
#'  $colon_not_here: logical vector. Does list.fun contain function names without :: or ::: ?
#' @details
#' - Warning: requires saferDev::arg_check, .noclean_functions. In the safer Backbone section "######## check of the required functions from the required packages" add this function when checking for the presence of saferDev:::.colons_check_message.
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function
#' .colons_check_message(list.fun = list(c2 = "UseMethod"), list.fun.pos = list(c2 = 1), line.nb = 2, ini = c("function (x, ...) ", "UseMethod(\"mean\")", "<bytecode: 0x000001969e09e580>", "<environment: namespace:base>"), arg_user_setting = list(x = x, arg_user_setting = arg_user_setting, error_text = "P1::F1", internal_error_report_link = "" ), error_text = " INSIDE P1::F1", internal_error_report_link = "test", text = "BASIC", internal_fun_names = NULL)
#' }
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @keywords internal
#' @rdname internal_function
.colons_check_message <- function(
    list.fun, 
    list.fun.pos, 
    line.nb, 
    ini, 
    arg_user_setting, 
    error_text, 
    text,
    internal_fun_names
){
    # DEBUGGING
    # list.fun = in_basic_fun ; list.fun.pos = in_basic_fun_names_pos ; line.nb = in_basic_code_line_nb ; ini = out$code ; arg_user_setting = out$arg_user_setting ; error_text = " INSIDE P1::F1" ; text = "BASIC" ; internal_fun_names = out$internal_fun_names
    # list.fun = in_other_fun ; list.fun.pos = in_other_fun_names_pos ; line.nb = in_other_code_line_nb ; ini = out$code ; arg_user_setting = out$arg_user_setting ; error_text = " INSIDE P1::F1" ; text = "OTHER" ; internal_fun_names = out$internal_fun_names

    #### package name
    package_name <- "saferDev" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message
    #### end internal error report link

    #### function name
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L))) # warning: I have written n = 0 to avoid error when a safer function is inside another functions
    function_name <- base::paste0(tempo_settings[[1]], "()", collapse = NULL, recycle0 = FALSE) 
    # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function_name[1] == "::()" | function_name[1] == ":::()"){
        function_name <- function_name[3]
    }
    #### end function name

    #### arguments settings
    arg_user_setting <- tempo_settings[-1] # list of the argument settings (excluding default values not provided by the user)
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 2)), envir = base::parent.frame(n = 1))) # names of all the arguments
    #### end arguments settings

    #### error_text initiation

    ######## basic error text start
    error_text_start <- base::paste0(
        "ERROR IN ", 
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)), 
        function_name, 
        collapse = NULL, 
        recycle0 = FALSE
    )
    ######## end basic error text start

    ######## check of the error_text argument
    if( ! (base::all(base::typeof(x = error_text) == "character", na.rm = TRUE) & base::length(x = error_text) == 1)){ # no need to test is.null(error_text) because typeof(x = NULL) == "character" returns FALSE
        tempo_cat <- base::paste0(
            error_text_start, 
            "\nTHE error_text ARGUMENT MUST BE A SINGLE CHARACTER STRING (CAN BE \"\").\nHERE IT IS:\n", 
            base::paste0(error_text, collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end check of the error_text argument

    ######## basic error text start updated
    error_text_start <- base::paste0(
        error_text_start, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    ######## end basic error text start updated

    ######## internal error text
    intern_error_text_start <- base::paste0(
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)), 
        function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    intern_error_text_end <- base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, ".", collapse = NULL, recycle0 = FALSE))
    ######## end internal error text

    #### end error_text initiation

    #### environment checking

    ######## check of lib_path
    ######## end check of lib_path

    ######## safer_check argument checking
    # not required
    ######## end safer_check argument checking

    ######## check of the required functions from the required packages
    # already done in the main function
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    # already done in the main function
    ######## end critical operator checking

    #### end environment checking

    #### argument primary checking

    ######## arg with no default values
    mandat_args <- base::c(
        "list.fun", 
        "list.fun.pos", 
        "line.nb", 
        "ini", 
        "arg_user_setting", 
        "error_text", 
        "internal_error_report_link", 
        "text",
        "internal_fun_names"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = FALSE)){
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

    ######## management of NA arguments
    if(base::length(x = arg_user_setting) != 0){
        tempo_log <- base::suppressWarnings(expr = base::sapply(X = base::lapply(X = arg_user_setting, FUN = function(x){base::is.na(x = x)}), FUN = function(x){base::any(x = x, na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE), classes = "warning") & base::lapply(X = arg_user_setting, FUN = function(x){base::length(x = x)}) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA because base::is.na() used here
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT JUST BE NA:", 
                base::paste0(arg_names[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end management of NA arguments

    ######## management of NULL arguments
    tempo_arg <-base::c(
        "list.fun", 
        "list.fun.pos", 
        "line.nb", 
        "ini", 
        "arg_user_setting", 
        "error_text", 
        # "internal_error_report_link" # inactivated because can be NULL
        "text"
        # "internal_fun_names" # inactivated because can be NULL
    )
    tempo_log <- base::sapply( X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
    if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA with base::is.null()
        tempo_cat <- base::paste0(
            error_text_start, 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            "\nCANNOT BE NULL", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end management of NULL arguments

    ######## argument checking with arg_check()
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))
    # add as many lines as below, for each of your arguments of your function in development
    tempo <- saferDev::arg_check(data = list.fun, class = "list", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = NULL, safer_check = FALSE,  error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    tempo <- saferDev::arg_check(data = list.fun.pos, class = "list", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = NULL, safer_check = FALSE,  error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    tempo <- saferDev::arg_check(data = line.nb, class = "vector", typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = NULL, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    tempo <- saferDev::arg_check(data = ini, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = NULL, safer_check = FALSE,  error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    tempo <- saferDev::arg_check(data = arg_user_setting, class = "list", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = NULL, safer_check = FALSE,  error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    # error_text already checked above
    if( ! base::is.null(x = internal_error_report_link)){ # for all arguments that can be NULL, write like this:
        tempo <- saferDev::arg_check(data = internal_error_report_link, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = NULL, safer_check = FALSE,  error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    }
    tempo <- saferDev::arg_check(data = text, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = c("BASIC", "OTHER"), all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = NULL, safer_check = FALSE,  error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    if( ! base::is.null(x = internal_fun_names)){ # for all arguments that can be NULL, write like this:
        tempo <- saferDev::arg_check(data = internal_fun_names, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = NULL, safer_check = FALSE,  error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    }
    if( ! base::is.null(x = argum_check)){
        if(base::any(argum_check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste0(text_check[argum_check], collapse = "\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # check with r_debugging_tools
    # source("https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    tempo_arg <- base::c(
        # "ini", # inactivated because can be ""
        # "error_text" # inactivated because can be ""
        # "internal_error_report_link" # inactivated because can be ""
        "text",
        "internal_fun_names"

    )
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())  # for character argument that can also be NULL, if NULL -> considered as character
    if(base::any(tempo_log, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            "INTERNAL ERROR IN THE BACKBONE PART OF ", 
            intern_error_text_start, 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS ARE", no = "THIS ARGUMENT IS"), 
            " NOT MODE \"character\":\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = FALSE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
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

    #### end argument primary checking

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
    output.cat <- NULL
    colon_not_here <- FALSE # reminder: no colon problem with internal functions
    # check the identical structure of list.fun and list.fun.pos
    ident_str <- function(list.fun, list.fun.pos, error_nb, intern_error_text_start, intern_error_text_end){
        if( ! (base::length(x = list.fun) == base::length(x = list.fun.pos) & base::all(base::sapply(X = list.fun, FUN = function(x){base::length(x = x)}) == base::sapply(X = list.fun.pos, FUN = function(x){base::length(x = x)}), na.rm = TRUE))){
            tempo.cat <- base::paste0(
                "INTERNAL ERROR ", 
                error_nb, " 
                IN ", 
                intern_error_text_start, 
                "LISTS list.fun AND list.fun.pos SHOULD HAVE IDENTICAL STRUCTURES\nBUT LENGTHS ARE RESPECTIVELY:\n", 
                base::length(x = list.fun), 
                "\n", 
                base::length(x = list.fun.pos), 
                "\nAND NUMBER OF ELEMENT IN EACH COMPARTMENT ARE RESPECTIVELY:\n", 
                base::paste0(base::sapply(X = list.fun, FUN = function(x){base::length(x = x)}), collapse = " ", , recycle0 = FALSE), 
                "\n", 
                base::paste(base::sapply(X = list.fun, FUN = function(x){base::length(x = x)}), collapse = " "), 
                intern_error_text_end, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    # end check the identical structure of list.fun and list.fun.pos
    if(base::length(text) != 1 & base::any( ! text %in% base::c("BASIC", "OTHER"))){
        tempo.cat <- base::paste0("
            INTERNAL ERROR 1 IN ", 
            intern_error_text_start, 
            "THE text ARGUMENT OF .colons_check_message() MUST BE \"BASIC\" OR \"OTHER\".\nTHE PROBLEM IS:\n",
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    ident_str(
        list.fun = list.fun, 
        list.fun.pos = list.fun.pos, 
        error_nb = 2, 
        intern_error_text_start = intern_error_text_start,
        intern_error_text_end = intern_error_text_end
    )
    # remove internal functions in other functions (list.fun and list.fun.pos)
    if(text == "OTHER" & base::length(internal_fun_names) > 0){
        empty_compart_log <- ! logical(length = base::length(x = list.fun)) # all TRUE at the beginning
        for(i2 in 1:base::length(internal_fun_names)){
            tempo_log <- base::lapply(X = list.fun, FUN = function(x){
                x == internal_fun_names[i2]
            })
            if(i2 == 1){
                intern_fun_log <- tempo_log
            }else{
                intern_fun_log <- base::mapply(FUN = function(x, y){x | y}, x = tempo_log, y = intern_fun_log, SIMPLIFY = FALSE)
            }
        }
        # remove internal functions elements
        list.fun <- base::mapply(FUN = function(x, y){y[ ! x]}, x = intern_fun_log, y = list.fun, SIMPLIFY = FALSE)
        list.fun.pos <- base::mapply(FUN = function(x, y){y[ ! x]}, x = intern_fun_log, y = list.fun.pos, SIMPLIFY = FALSE)
        # end remove internal functions elements
        ident_str(
            list.fun = list.fun, 
            list.fun.pos = list.fun.pos, 
            error_nb = 3, 
            intern_error_text_start = intern_error_text_start,
            intern_error_text_end = intern_error_text_end
        )
        # remove empty compartment
        tempo_log2 <- base::sapply(X = list.fun, FUN = function(x){base::length(x = x) == 0}) # test if empty compartment
        list.fun <- list.fun[ ! tempo_log2]
        list.fun.pos <- list.fun.pos[ ! tempo_log2]
        line.nb <- line.nb[ ! tempo_log2]
        # end remove empty compartment
        output.cat <- base::paste0(
            "INSIDE ", arg_user_setting$x, "(), ", base::ifelse(base::length(x = list.fun) == 0, "ONLY", ""), "INTERNAL FUNCTION", base::ifelse(base::length(internal_fun_names) == 1, "", "S"), " DETECTED:\n", 
            base::paste(internal_fun_names, collapse = "\n")
        )
        # reminder: no colon problem with internal functions
    }
    # end remove internal functions in other functions (list.fun and list.fun.pos)
    if(base::length(x = list.fun) != 0){
        # pattern2 <- base::paste(base::paste0("(?<![A-Za-z0-9._])", fun.uni, "\\s*\\("), collapse = "|") # to split string according to function name as splitter. Pattern (?<![A-Za-z0-9._]) means "must not be preceeded by any alphanum or .or _
        # pattern3 <- base::paste(base::paste0("(?<![A-Za-z0-9._])", fun.uni, "\\s*\\($"), collapse = "|") # same as pattern2 but used to know if the seeked function is at the end of the string
        basic_ini <- ini[line.nb]
        if( ! (base::length(list.fun) == base::length(list.fun.pos) & base::length(list.fun) == base::length(line.nb) & base::length(list.fun) == base::length(basic_ini))){
            tempo.cat <- base::paste0(
                "INTERNAL ERROR 4 IN ", 
                intern_error_text_start, 
                "LENGTHS SHOULD BE IDENTICAL\nlist.fun: ", 
                base::length(list.fun), 
                "\nlist.fun.pos: ", 
                base::length(list.fun.pos), 
                "\nline.nb: ", 
                base::length(line.nb), 
                "\nbasic_ini: ", 
                base::length(basic_ini), 
                intern_error_text_end, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        res <- list.fun.pos
        for(i1 in 1:base::length(basic_ini)){
            res[[i1]] <- base::mapply(FUN = function(x , y){z <- base::substr(x = x, start = 1, stop = y - 1)}, x = basic_ini[i1], y = list.fun.pos[[i1]], SIMPLIFY = TRUE, USE.NAMES = FALSE)
        }
        # res <- base::strsplit(x = basic_ini, split = pattern2, perl = TRUE) # in res, all the strings should finish by ::
        # tempo.log <- ! base::grepl(x = basic_ini, pattern = pattern3, perl = TRUE) # strings of basic_ini that does not finish by the function name
        # in each compartment of res, the last split section is removed because nothing to test at the end (end of code)
        # if(base::sum(tempo.log, na.rm = TRUE) > 0){
        #     res[tempo.log] <- base::lapply(X = res[tempo.log], FUN = function(x){x[-base::length(x)]})
        # }
        # end in each compartment of res, the last split section is removed because nothing to test at the end (end of code)
        res2 <- base::lapply(X = res, FUN = function(x){base::substr(x, base::nchar(x)-1, base::nchar(x))}) # base::nchar(x)-1 takes only :: if the strings ends by :::
        base::names(res2) <- NULL
        if( ! base::all(base::sapply(X = res2, FUN = function(x){base::length(x)}) == base::sapply(X = res, FUN = function(x){base::length(x)}))){
            tempo.cat <- base::paste0(
                "INTERNAL ERROR 5 IN ", 
                intern_error_text_start, 
                "LENGTHS SHOULD BE IDENTICAL\nres2: ", 
                base::paste0(base::sapply(X = res2, FUN = function(x){base::length(x)}), collapse = " ", recycle0 = FALSE), 
                "\nres: ", 
                base::paste0(base::sapply(X = res, FUN = function(x){base::length(x)}), collapse = " ", recycle0 = FALSE), 
                intern_error_text_end, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        colon_not_here <- base::lapply(X = res2, FUN = function(x){ ! x %in% "::"}) # no need to check for ":::" because base::nchar(x)-1 takes only :: if the strings ends by :::
        if( ! base::all(base::sapply(X = res2, FUN = function(x){base::length(x)}) == base::sapply(X = colon_not_here, FUN = function(x){base::length(x)}))){
            tempo.cat <- base::paste0(
                "INTERNAL ERROR 6 IN ", 
                intern_error_text_start, 
                "LENGTHS SHOULD BE IDENTICAL\nres2: ", 
                base::paste0(base::sapply(X = res2, FUN = function(x){base::length(x)}), collapse = " ", recycle0 = FALSE), 
                "\ncolon_not_here: ", 
                base::paste0(base::sapply(X = colon_not_here, FUN = function(x){base::length(x)}), collapse = " ", recycle0 = FALSE), 
                intern_error_text_end, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(base::any(base::unlist(colon_not_here))){
            col1 <- base::as.vector(base::unlist(base::mapply(FUN = function(x, y){base::rep(y, base::sum(x))}, x = colon_not_here, y = line.nb, SIMPLIFY = TRUE)))
            col2 <- base::as.vector(base::unlist(base::mapply(FUN = function(x, y){y[x]}, x = colon_not_here, y = list.fun, SIMPLIFY = TRUE)))
            col3 <- base::as.vector(base::unlist(base::mapply(FUN = function(x, y){y[x]}, x = colon_not_here, y = res, SIMPLIFY = TRUE)))
            if( ! (base::length(col1) == base::length(col2) & base::length(col1) == base::length(col3) & base::length(col2) == base::length(col3))){
                tempo.cat <- base::paste0(
                    "INTERNAL ERROR 7 IN ", 
                    intern_error_text_start, 
                    "LENGTHS OF col1 (", 
                    base::length(col1), 
                    "), col2 (", 
                    base::length(col2), 
                    "), AND col3 (", 
                    base::length(col3), 
                    "), SHOULD BE EQUAL\n", 
                    collapse = NULL, 
                    intern_error_text_end, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
            }
            # removal of functions between quotes and after $
            tempo.log <- .noclean_functions(
                col1 = col1, 
                col2 = col2, 
                col3 = col3, 
                ini = ini, 
                error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
            ) # function names are inside quotes or after $ ?
            if(base::sum(tempo.log, na.rm = TRUE) > 0){
                col1 <- col1[ ! tempo.log] # keep clean functions
                col2 <- col2[ ! tempo.log] # keep clean functions
                col3 <- col3[ ! tempo.log] # keep clean functions 
            }
            # end removal of functions between quotes and after $
            if(base::length(col1) > 0){
                tempo.pos <- base::paste0(col1, "\t", col2, "\t\t", col3)
                output.cat <- base::paste0(
                    base::ifelse(test = base::is.null(output.cat), yes = "", no = base::paste0(output.cat, "\n\n")),
                    "INSIDE ", arg_user_setting$x, "(), SOME :: OR ::: ARE MISSING AT ", text, " FUNCTION POSITIONS:\n\n", 
                    "LINE\tFUN\t\tSTRING_BEFORE\n",
                    base::paste(tempo.pos, collapse = "\n")
                )
            }
        }
    }
    base::return(base::list(output.cat = output.cat, colon_not_here = base::unlist(colon_not_here)))
}
