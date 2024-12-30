#' @title .all_args_here_fill
#' @description
#' Get the $MISSING_ARG_NAMES, $MISSING_ARGS and $NEW of all_args_here()
#' @param arg_full List of all arguments of the function with default value
#' @param arg_full_names Vector of strings of the names of the arguments of the function
#' @param tempo_split Vector of strings of the observed argument writting of the function.
#' @param three_dots_log Vector of logical. Is ... present among arg_full_names 
#' @param i2 Single integer value indicating the loop number
#' @param col1_i2 Single integer value indicating the code line number of the checked function
#' @param col2_i2 Single string indicating the name of the checked sub function
#' @param arg_user_setting_x Single string indicating the name of the checked function
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = "INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>".
#' @param internal_error_report_link Single string of the link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message.
#' @param warn warning string.
#' @param warn_count warning count.
#' @returns
#'  A list:
#'    $col6: the $MISSING_ARG_NAMES.
#'    $col7: the $MISSING_ARGS.
#'    $col8: the $STATUS.
#' @details
#' - Warning: requires saferDev::arg_check. In the safer Backbone section "######## check of the required functions from the required packages" add this function when checking for the presence of saferDev:::.all_args_here_fill
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @keywords internal
#' @rdname internal_function
.all_args_here_fill <- function(
    arg_full,
    arg_full_names, 
    tempo_split, 
    three_dots_log, 
    i2, 
    col1_i2, 
    col2_i2,
    arg_user_setting_x, 
    error_text,
    internal_error_report_link, 
    warn,
    warn_count
){
    # DEBUGGING
    # arg_full = arg_full ; arg_full_names = arg_full_names ; tempo_split = tempo_split ; three_dots_log = three_dots_log ; i2 = i2 ; col1_i2 = col1[i2] ; col2_i2 = col2[i2] ; arg_user_setting_x = "FUN1" ; error_text = " INSIDE P1::F1" ; internal_error_report_link = "test" ; warn = "WARNING" ; warn_count = 1  
    #  arg_full = list(definition = "sys.function(sys.parent())", call = "sys.call(sys.parent())", expand.dots = TRUE, envir = "parent.frame(2L)") ; arg_full_names = c("definition", "call", "expand.dots", "envir") ; tempo_split = "expand.dots = FALSE" ;  three_dots_log = c(FALSE, FALSE, FALSE, FALSE) ; col2_i2 = "match.call" ; col3_i2 = "match.call(expand.dots = FALSE)" ; arg_user_setting_x = "FUN1" ; error_text = " INSIDE P1::F1" ; internal_error_report_link = "test" ; warn = "WARNING" ; warn_count = 1 
    #  arg_full = list(definition = sys.function(sys.parent()), call = sys.call(sys.parent()), expand.dots = TRUE, envir = parent.frame(2L)) ; arg_full_names = c("definition", "call", "expand.dots", "envir") ; tempo_split = c("sys.function(sys.parent())", "expand.dots = FALSE", "sys.call(sys.parent())") ;  three_dots_log = c(FALSE, FALSE, FALSE, FALSE) ; col2_i2 = "match.call" ; col3_i2 = "match.call(sys.function(sys.parent()), expand.dots = FALSE, sys.call(sys.parent()))" ; arg_user_setting_x = "FUN1" ; error_text = " INSIDE P1::F1" ; internal_error_report_link = "test" ; warn = "WARNING" ; warn_count = 1 
    #  arg_full = list(... = "", collapse = " ", recycle0 = FALSE) ; arg_full_names = c("...", "collapse", "recycle0") ; tempo_split = c("AA", "collapse = \" \"", "BB", "recycle0 = FALSE") ; three_dots_log = c(TRUE, FALSE, FALSE) ; col2_i2 = "paste0" ; col3_i2 = 'paste0("AA", collapse = " ", "BB", recycle0 = FALSE)' ; arg_user_setting_x = "FUN1" ;error_text = " INSIDE P1::F1" ; internal_error_report_link = "test" ; warn = "WARNING" ; warn_count = 1 
    #  arg_full = list(... = "", collapse = " ", recycle0 = FALSE) ; arg_full_names = c("...", "collapse", "recycle0") ; tempo_split = c("AA", "collapse = \" \"", "BB") ; three_dots_log = c(TRUE, FALSE, FALSE) ; col2_i2 = "paste0" ; col3_i2 = 'paste0("AA", collapse = " ", "BB")' ; arg_user_setting_x = "FUN1" ; error_text = " INSIDE P1::F1" ; internal_error_report_link = "test" ; warn = "WARNING" ; warn_count = 1 

    #### package name
    package_name <- "saferDev" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    # set by the internal_error_report_link argument
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
        "arg_full",
        "arg_full_names", 
        "tempo_split", 
        "three_dots_log", 
        "i2", 
        "col1_i2", 
        "col2_i2",
        "arg_user_setting_x", 
        "error_text",
        "internal_error_report_link", 
        "warn",
        "warn_count"
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

    ######## argument checking with arg_check()
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))
    # add as many lines as below, for each of your arguments of your function in development
    tempo <- saferDev::arg_check(data = arg_full, class = "list", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = NULL, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    tempo <- saferDev::arg_check(data = arg_full_names, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = NULL, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    tempo <- saferDev::arg_check(data = tempo_split, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = NULL, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    tempo <- saferDev::arg_check(data = three_dots_log, class = "vector", typeof = "logical", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = NULL, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    tempo <- saferDev::arg_check(data = i2, class = "vector", typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = NULL, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    tempo <- saferDev::arg_check(data = col1_i2, class = "vector", typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = NULL, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    tempo <- saferDev::arg_check(data = col2_i2, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = NULL, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    tempo <- saferDev::arg_check(data = arg_user_setting_x, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = NULL, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    # error_text already checked above
    if( ! base::is.null(x = internal_error_report_link)){ # for all arguments that can be NULL, write like this:
        tempo <- saferDev::arg_check(data = internal_error_report_link, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = NULL, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    }
    tempo <- saferDev::arg_check(data = warn, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = NULL, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    tempo <- saferDev::arg_check(data = warn_count, class = "vector", typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = NULL, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    if( ! base::is.null(x = argum_check)){
        if(base::any(argum_check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste0(text_check[argum_check], collapse = "\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # check with r_debugging_tools
    # source("https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    ######## end argument checking with arg_check()

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
        "arg_full",
        "arg_full_names", 
        "tempo_split", 
        "three_dots_log", 
        "i2", 
        "col1_i2", 
        "col2_i2",
        "arg_user_setting_x", 
        "error_text",
        # "internal_error_report_link" # inactivated because can be NULL
        "warn",
        "warn_count"
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

    ######## management of "" in arguments of mode character
    tempo_arg <- base::c(
        "arg_full_names", 
        "tempo_split",
        "col2_i2",
        "arg_user_setting_x",
        # "error_text" # inactivated because can be ""
        # "internal_error_report_link" # inactivated because can be ""
        "warn"
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
    pattern1 <- "^\\s*([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*[\\s\\r\\n]*=" # looking for the arg name
    good_args <- NULL
    missing_args <- NULL
    missing_args_names <- NULL
    obs_arg_log <- base::logical()
    if(base::any(three_dots_log, na.rm = TRUE)){
        arg_full_names <- arg_full_names[ ! three_dots_log]
        arg_full <- arg_full[ ! three_dots_log]
    }
    if(base::length(arg_full) == 0){
        # col5 <- base::c(col5, "...") #inactivated because already filled above
        col6 <- ""
        col7 <- ""
        col8 <- ""
    }else{
        # scan for args names present in tempo_split
        good_count <- 0 # to define if all the args are written (not considering ...)
        if(base::length(tempo_split) == 0 & base::length(arg_full_names) > 0){
            missing_args_names <- arg_full_names
        }else{
            obs_arg_log <- base::rep(TRUE, base::length(tempo_split)) # will help for counting the tempo_split args without arg name before. All the remaining TRUE will be values that need an arg name
            for(i3 in 1:base::length(arg_full_names)){
                pattern3 <- base::paste0("^[\\s\\r\\n]*", arg_full_names[i3], "[\\s]*=") # looking for the arg name
                tempo.log <- base::grepl(x = tempo_split, pattern = pattern3, perl = TRUE)
                if(base::sum(tempo.log, na.rm = TRUE) == 1){ # arg i3 has its names written in the args between ()
                    good_args <- base::c(good_args, tempo_split[tempo.log])
                    obs_arg_log <- obs_arg_log & ! tempo.log # remove the position of the taken arg in tempo_split
                    good_count <- good_count + 1
                }else if(base::sum(tempo.log, na.rm = TRUE) == 0){ # arg i3 has not its names written in the args between ()
                    missing_args_names <- base::c(missing_args_names, arg_full_names[i3])
                }else{
                    tempo.cat <- base::paste0(
                        "INTERNAL ERROR 1 IN ",
                        intern_error_text_start, 
                        "pattern3 DETECTED SEVERAL TIMES IN ARGUMENTS:\n\npattern3:\n", 
                        base::paste(pattern3, collapse = "\n"), 
                        "\n\ntempo_split:\n", 
                        base::paste(tempo_split[tempo.log], collapse = "\n"), 
                        "\n\nCHECK IF THE ARGUMENT IS PRESENT SEVERAL TIMES IN LINE ", 
                        col1_i2, 
                        ", INSIDE ", 
                        col2_i2, 
                        intern_error_text_end, 
                        collapse = NULL, 
                        recycle0 = FALSE
                    )
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                }
            }
        }
        # end scan for args names present in tempo_split
        # removal of arguments without arg name before in obs_arg_log
        supp_args_in_three_dots <- NULL
        if(base::any(three_dots_log, na.rm = TRUE)){
            if(base::length(x = obs_arg_log) != 0){ # no need to add & base::length(tempo_split) != 0 because both have the same length
                for(i3 in 1:base::length(tempo_split)){
                    if(base::grepl(x = tempo_split[i3], pattern = pattern1, perl = TRUE) & obs_arg_log[i3] == TRUE){ # obs_arg_log[i3] == TRUE means values that need an arg name but detection of a = with only arg name rule before
                        obs_arg_log[i3] <- FALSE # remove this arg from the args that need an arg name
                        supp_args_in_three_dots <- base::c(supp_args_in_three_dots, tempo_split[i3])
                    }
                }
            }
        }
        # end removal of arguments without arg name before in obs_arg_log
        # detection of arguments that starts by the same string in the sub function
        tempo_col8_end <- NULL
        same_begin <- base::unlist(base::lapply(
            FUN = function(x){
                tempo_log <- base::grepl(x = arg_full_names, pattern = base::paste0("^", x), perl = FALSE)
                if(base::sum(tempo_log, na.rm = TRUE) > 1){ 
                    base::return(arg_full_names[tempo_log][base::which.min(base::nchar(arg_full_names[tempo_log]))])
                }
            }, 
            X = arg_full_names
        ))
        if( ! base::is.null(same_begin)){
            tempo_col8_end <- base::paste0("WARNING: SEVERAL ARGUMENT NAMES OF THE FUNCTION BEGINNING WITH ", base::paste0(same_begin[ ! base::is.null(same_begin)], collapse = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        }
        # end detection of arguments that starts by the same string in the sub function
        # checking if arg name are not fully written
        arg_full_symbol_type <- base::sapply(X = arg_full, FUN = function(x){base::all(base::typeof(x) == "symbol", na.rm =TRUE)}) # to check if any arg without optional value are completed with obs arg values
        if(base::any(arg_full_symbol_type, na.rm =TRUE) & base::length(tempo_split) == 0){
            tempo.cat <- base::paste0(
                error_text_start,
                "THE TESTED FUNCTION ", 
                arg_user_setting_x, 
                " SEEMS TO HAVE A WRITTING ERROR IN LINE ",  
                col1_i2, 
                " AND FUNCTION ", 
                col2_i2, 
                ".\nPLEASE, RUN THE TESTED FUNCTION FIRST.", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        tempo_col8 <- NULL
        if(( ! base::is.null(missing_args_names)) & base::length(tempo_split) != 0){
            for(i3 in 1:base::length(tempo_split)){
                if(base::grepl(x = tempo_split[i3], pattern = pattern1, perl = TRUE)){
                    tempo_arg_name <- base::strsplit(tempo_split[i3], split = "[\\s\\r\\n]*=", perl = TRUE)[[1]][1]
                    tempo_arg_name <- base::gsub(pattern = "^[\\s]*", replacement = "", x = tempo_arg_name) # removing leading space
                    if( ! base::is.null(same_begin)){
                        if( ! tempo_arg_name %in% same_begin){
                            tempo.log <- base::grepl(x = missing_args_names, pattern = base::paste0("^", tempo_arg_name), perl = FALSE)
                            if(base::sum(tempo.log, na.rm = TRUE) > 1){
                                tempo.cat <- base::paste0(
                                    "INTERNAL ERROR 2 IN ", 
                                    intern_error_text_start, 
                                    "IN LINE ", 
                                    i2, 
                                    " IN THE ", 
                                    col2_i2, 
                                    " FUNCTION\ntempo_arg_name DETECTS SEVERAL TIMES ARGUMENT NAMES:\n\ntempo_arg_name:\n", 
                                    tempo_arg_name, 
                                    "\n\nmissing_args_names:\n", 
                                    base::paste(missing_args_names, collapse = "\n"), 
                                    "\n\nmissing_args_names[tempo.log]:\n", 
                                    base::paste(missing_args_names[tempo.log], collapse = "\n"), 
                                    intern_error_text_end, 
                                    collapse = NULL, 
                                    recycle0 = FALSE
                                )
                                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                            }
                            if(base::sum(tempo.log, na.rm = TRUE) == 1){
                                tempo_col8 <- base::c(
                                    tempo_col8, 
                                    base::paste0(
                                        base::ifelse(test = base::is.null(tempo_col8), yes = "", no = " ; "), 
                                        base::paste0(tempo_arg_name, " ARG NAME HAS TO BE FULLY WRITTEN ", missing_args_names[tempo.log])
                                    )
                                )
                            }
                        }
                    }
                }
            }
            if(( ! base::is.null(tempo_col8)) & ! base::is.null(tempo_col8_end)){
                tempo_col8 <- base::paste0(tempo_col8, " & ", tempo_col8_end, collapse = NULL, recycle0 = FALSE)
            }
        }
        # end checking if arg name are not fully written
        # when ... is present or not
            # Of note, when ... is present, argument values must be preceeded by their arg name. This means that values without arg names of the scanned function are ...
            # Otherwise, the first value without names must take the first arg name not already used, the second value without names must take the second, etc., then finish by the none used arg names with their default values
        missing_arg_log <- arg_full_names %in% missing_args_names
        if(base::any(three_dots_log, na.rm = TRUE) & base::all( ! arg_full_symbol_type, na.rm =TRUE)){ # ... present but no mandatory args with value to set 
            missing_args <-  base::unlist(base::mapply(FUN = function(x, y){base::paste0(x, " = ", if(base::is.null(y)){"NULL"}else{y})}, x = arg_full_names[missing_arg_log], y = arg_full[missing_arg_log], SIMPLIFY = TRUE)) # missing arg values with names
            good_args <- base::c(
                tempo_split[ ! tempo_split %in% good_args], # arg values without names
                good_args, # obs arg values with names
                base::paste0(" ", missing_args) # missing arg values with names #a space added to finally have  comma followed by a space
            )
        }else{
            count_good_args <- 0
            final <- NULL
            missing_args <-  NULL
            for(i3 in 1:base::length(arg_full_names)){ # here I cannot have more args than base::length(arg_full_names)
                if(missing_arg_log[i3] == TRUE){
                    if(base::sum(obs_arg_log) > 0){ # this means that remains obs arg with no arg names written
                        tempo <- base::paste0(arg_full_names[i3], " = ", tempo_split[base::which(obs_arg_log == TRUE)[1]])
                        obs_arg_log[base::which(obs_arg_log == TRUE)[1]] <- FALSE
                    }else{
                        tempo <- base::paste0(arg_full_names[i3], " = ", if(base::is.null(base::deparse(arg_full[[i3]]))){"NULL"}else{base::deparse(arg_full[[i3]])})
                    }
                    missing_args <- base::c(missing_args, tempo)
                    final <- base::c(final, base::ifelse(test = i3 == 1, yes = tempo, no = base::paste0(" ", tempo))) # take the first pos always of the args with no arg names
                }else{
                    count_good_args <- count_good_args + 1
                    final <- base::c(final, good_args[count_good_args])
                }
                arg_full_symbol_type[i3] <- FALSE
            }
            good_args <- final
            if(base::any(arg_full_symbol_type)){
                tempo.cat <- base::paste0(
                    "INTERNAL ERROR 3 IN ", 
                    intern_error_text_end, 
                    "ARGUMENT WITHOUT OPTIONAL VALUES (MANDATORY ARGS) CANNOT REMAIN WITHOUT VALUE:\n\narg_full_symbol_type:\n", 
                    base::paste(arg_full_symbol_type, collapse = "\n"), 
                    "\n\narg_full_names:\n", 
                    base::paste(arg_full_names, collapse = "\n"), 
                    intern_error_text_end, 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
            if(( ! base::any(three_dots_log, na.rm = TRUE)) & base::any(obs_arg_log, na.rm =TRUE)){
                tempo.cat <- base::paste0(
                    "INTERNAL ERROR 4 IN ", 
                    intern_error_text_start, 
                    "CANNOT HAVE OBS ARGUMENT NOT INCORPORATED YET IF ! base::any(three_dots_log, na.rm = TRUE) IS TRUE:\n\nthree_dots_log:\n", 
                    base::paste(three_dots_log, collapse = " "), 
                    "\n\nobs_arg_log:\n", 
                    base::paste(obs_arg_log, collapse = " "), 
                    intern_error_text_end, 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
            if(count_good_args > base::length(tempo_split)){
                tempo.cat <- base::paste0(
                    "INTERNAL ERROR 5 IN ", 
                    intern_error_text_start, 
                    "count_good_args + 1 CANNOT BE MORE THAN length(tempo_split):\n\nlength(tempo_split): ", 
                    base::length(tempo_split), 
                    "\n\ncount_good_args + 1: ", 
                    count_good_args + 1, 
                    intern_error_text_end, 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
            if(base::any(three_dots_log, na.rm = TRUE) & base::any(obs_arg_log, na.rm =TRUE)){ # obs values not yet in good_args
                if(count_good_args + 1 <= base::length(tempo_split)){
                    good_args <- base::c(good_args, tempo_split[obs_arg_log])
                }
            }
        }
        # end when ... is present or not
        # col5 done above
        col6 <- base::paste(missing_args_names, collapse = ", ") # if NULL return ""
        col7 <- base::paste(missing_args, collapse = ", ")  # if NULL return ""
        tempo <- base::paste0(
            col2_i2, 
            "(", 
            base::ifelse(test = ! base::is.null(supp_args_in_three_dots), yes = base::gsub(pattern = "^[\\s]*", replacement = "", x = base::paste0(supp_args_in_three_dots, collapse = ","), perl = TRUE), no = ""), 
            base::ifelse(test = ( ! base::is.null(supp_args_in_three_dots)) & ( ! base::is.null(good_args)) , yes = ",", no = ""), 
            base::ifelse(test = ! base::is.null(good_args), yes = base::paste0(good_args, collapse = ","), no = ""),
            ")"
        )
        if(base::length(arg_full_names) == good_count){
            col8 <- "GOOD"
        }else{
            col8 <- tempo
        }
        if( ! base::is.null(tempo_col8)){
            col8 <- tempo_col8
        }
    }
    base::return(base::list(col6 = col6, col7 = col7, col8 = col8))
    #### end main code
}

