
#' @title .colons_check_message
#' @description
#' Create the message for the colons_check() function.
#' @param list_fun List of names of all the functions.
#' @param list_fun_pos List of positions of first character of names of all the functions in ini.
#' @param line_nb Vector of integers of corresponding line numbers.
#' @param ini Vector of strings of the initial function code analyzed.
#' @param arg_user_setting2 List of arg user settings.
#' @param text Either "BASIC" or "OTHER".
#' @param internal_fun_names Vector of strings of names of internal functions in the function code analyzed. Can be NULL.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R package are not installed in the default directories because of lack of admin rights.  More precisely, lib_path is passed through the new argument of .libPaths() so that the new library paths are unique(c(new, .Library.site, .Library)). Warning: .libPaths() is restored to the initial paths, after function execution. Ignored if NULL (default) or if the safer_check argument is FALSE: only the pathways specified by the current .libPaths() are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>.". If NULL, converted into "".
#' @returns
#'  A list:
#'  $output.cat: the message (string).
#'  $colon_not_here: logical vector. Does list_fun contain function names without :: or ::: ?
#' @details
#' - Warning: requires saferDev::arg_check, saferDev:::.noclean_functions. In main safer functions, in the section "######## check of the required functions from the required packages" add these functions when checking for the presence of saferDev:::.colons_check_message.
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function (not found by devtools::check())
#' saferDev:::.colons_check_message(list_fun = list(c2 = "UseMethod"), list_fun_pos = list(c2 = 1), line_nb = 2, ini = c("function (x, ...) ", "UseMethod(\"mean\")"), arg_user_setting2 = list(x = "mean"), text = "BASIC", internal_fun_names = NULL, lib_path = NULL, error_text = " INSIDE P1::F1")
#' }
#' @author \href{gael.millot@pasteur.fr}{Gael Millot}
#' 
#' 
#' @keywords internal
.colons_check_message <- function(
    # in internal functions, all arguments are without value on purpose
    list_fun, 
    list_fun_pos, 
    line_nb, 
    ini, 
    arg_user_setting2, 
    text,
    internal_fun_names,
    lib_path, # required because of saferDev::arg_check()
    error_text # warning: in internal functions, error_text without default value returns a R classical non traced error message (specific of internal functions since classical functions are error_text = "")
){
    # DEBUGGING
    # list_fun = in_basic_fun ; list_fun_pos = in_basic_fun_names_pos ; line_nb = in_basic_code_line_nb ; ini = out$code ; arg_user_setting2 = out$arg_user_setting ; text = "BASIC" ; internal_fun_names = out$internal_fun_names ; lib_path = lib_path ; error_text = " INSIDE P1::F1" 
    # list_fun = in_other_fun ; list_fun_pos = in_other_fun_names_pos ; line_nb = in_other_code_line_nb ; ini = out$code ; arg_user_setting2 = out$arg_user_setting ;  text = "OTHER" ; internal_fun_names = out$internal_fun_names ; lib_path = lib_path ; error_text = " INSIDE P1::F1" 

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
        "list_fun", 
        "list_fun_pos", 
        "line_nb", 
        "ini", 
        "arg_user_setting2", 
        "text",
        "internal_fun_names",
        "lib_path"
        # "error_text" # inactivated because error_text already used above. Specific of my internal functions that error_text has no default value
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
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end arg with no default values

    ######## management of NULL arguments
    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE
    tempo_arg <-base::c(
        "list_fun", 
        "list_fun_pos", 
        "line_nb", 
        "ini", 
        "arg_user_setting2", 
        "text"
        # "internal_fun_names" # inactivated because can be NULL
        # "lib_path", # inactivated because can be NULL
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
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # # before NA checking because is.na(logical()) is logical(0) (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "list_fun", 
        "list_fun_pos", 
        "line_nb", 
        "ini", 
        "arg_user_setting2", 
        "text", 
        "internal_fun_names", 
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
    # not required because not here
    ######## end safer_check argument checking

    ######## check of lib_path
    # check already done in the main safer function
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    # saferDev::arg_check is required
    # saferDev::.noclean_functions is required
    # but check already done in the main safer function
    ######## end check of the required functions from the required packages

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
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))
    # add as many lines as below, for each of your arguments of your function in development
    tempo <- saferDev::arg_check(data = list_fun, class = "list", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    tempo <- saferDev::arg_check(data = list_fun_pos, class = "list", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path,  error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    tempo <- saferDev::arg_check(data = line_nb, class = "vector", typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    tempo <- saferDev::arg_check(data = ini, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path,  error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    tempo <- saferDev::arg_check(data = arg_user_setting2, class = "list", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path,  error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    if( ! base::is.null(x = internal_error_report_link)){ # for all arguments that can be NULL, write like this:
        tempo <- saferDev::arg_check(data = internal_error_report_link, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path,  error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    }
    tempo <- saferDev::arg_check(data = text, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c("BASIC", "OTHER"), all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path,  error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    if( ! base::is.null(x = internal_fun_names)){ # for all arguments that can be NULL, write like this:
        tempo <- saferDev::arg_check(data = internal_fun_names, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path,  error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    }
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
        # "ini", # inactivated because can be ""
        "text",
        "internal_fun_names"
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    # backbone personalized here (base::is.null(x = x)){base::return(TRUE) removed) because the 4 arguments cannot be NULL and codecov must be optimized
    # INTERNAL ERROR IN THE BACKBONE PART section removed because codecov must be optimized. I have checked that these four arguments are tested for mode character upstream
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply()).  # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test is.null()
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
    if(base::length(x = list_fun) != base::length(x = list_fun_pos)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE list_fun AND list_fun_pos ARGUMENTS MUST BE LISTS OF SAME LENGTH.\nTHE PROBLEM IS:\nLENGTH OF list_fun: ", 
            base::paste0(base::length(x = list_fun), collapse = "\n", recycle0 = FALSE), 
            "\nLENGTH OF list_fun_pos: ", 
            base::paste0(base::length(x = list_fun_pos), collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # modification of arg_user_setting2$x for clean messages
    if(base::as.character(x = arg_user_setting2$x)[1] == "::" | base::as.character(x = arg_user_setting2$x)[1] == ":::"){
        arg_user_setting2$x <- base::paste0(base::as.character(x = arg_user_setting2$x)[2], base::as.character(x = arg_user_setting2$x)[1], base::as.character(x = arg_user_setting2$x)[3], "()", collapse = NULL, recycle0 = FALSE)
    }
    # end modification of arg_user_setting2$x for clean messages
    output.cat <- NULL
    colon_not_here <- FALSE # reminder: no colon problem with internal functions
    # check the identical structure of list_fun and list_fun_pos
    ident_str <- function(list_fun, list_fun_pos, error_nb, intern_error_text_start, intern_error_text_end){
        if( ! (base::length(x = list_fun) == base::length(x = list_fun_pos) & base::all(base::sapply(X = list_fun, FUN = function(x){base::length(x = x)}, simplify = TRUE, USE.NAMES = TRUE) == base::sapply(X = list_fun_pos, FUN = function(x){base::length(x = x)}, simplify = TRUE, USE.NAMES = TRUE), na.rm = TRUE))){
            tempo_cat <- base::paste0("INTERNAL ERROR ", error_nb, " IN ", intern_error_text_start, "LISTS list_fun AND list_fun_pos SHOULD HAVE IDENTICAL STRUCTURES\nBUT LENGTHS ARE RESPECTIVELY:\n", base::length(x = list_fun), "\n", base::length(x = list_fun_pos), "\nAND NUMBER OF ELEMENT IN EACH COMPARTMENT ARE RESPECTIVELY:\n", base::paste0(base::sapply(X = list_fun, FUN = function(x){base::length(x = x)}, simplify = TRUE, USE.NAMES = TRUE), collapse = " ", recycle0 = FALSE), "\n", base::paste0(base::sapply(X = list_fun_pos, FUN = function(x){base::length(x = x)}, simplify = TRUE, USE.NAMES = TRUE), collapse = " ", recycle0 = FALSE), intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ident_str(
        list_fun = list_fun, 
        list_fun_pos = list_fun_pos, 
        error_nb = 1, 
        intern_error_text_start = intern_error_text_start,
        intern_error_text_end = intern_error_text_end
    )
    # end check the identical structure of list_fun and list_fun_pos
    # remove internal functions in other functions (list_fun and list_fun_pos)
    if(text == "OTHER" & base::length(x = internal_fun_names) > 0){
        empty_compart_log <- ! base::logical(length = base::length(x = list_fun)) # all TRUE at the beginning
        for(i2 in 1:base::length(x = internal_fun_names)){
            tempo_log <- base::lapply(X = list_fun, FUN = function(x){
                x == internal_fun_names[i2]
            })
            if(i2 == 1){
                intern_fun_log <- tempo_log
            }else{
                intern_fun_log <- base::mapply(FUN = function(x, y){x | y}, x = tempo_log, y = intern_fun_log, SIMPLIFY = FALSE, MoreArgs = NULL, USE.NAMES = TRUE)
            }
        }
        # remove internal functions elements
        list_fun <- base::mapply(FUN = function(x, y){y[ ! x]}, x = intern_fun_log, y = list_fun, SIMPLIFY = FALSE, MoreArgs = NULL, USE.NAMES = TRUE)
        list_fun_pos <- base::mapply(FUN = function(x, y){y[ ! x]}, x = intern_fun_log, y = list_fun_pos, SIMPLIFY = FALSE, MoreArgs = NULL, USE.NAMES = TRUE)
        # end remove internal functions elements
        ident_str(
            list_fun = list_fun, 
            list_fun_pos = list_fun_pos, 
            error_nb = 2, 
            intern_error_text_start = intern_error_text_start,
            intern_error_text_end = intern_error_text_end
        )
        # remove empty compartment
        tempo_log2 <- base::sapply(X = list_fun, FUN = function(x){base::length(x = x) == 0}, simplify = TRUE, USE.NAMES = TRUE) # test if empty compartment
        list_fun <- list_fun[ ! tempo_log2]
        list_fun_pos <- list_fun_pos[ ! tempo_log2]
        line_nb <- line_nb[ ! tempo_log2]
        # end remove empty compartment
        output.cat <- base::paste0(
            "INSIDE ", arg_user_setting2$x, ", ", base::ifelse(test = base::length(x = list_fun) == 0, yes = "ONLY", no = ""), "INTERNAL FUNCTION", base::ifelse(test = base::length(x = internal_fun_names) == 1, yes = "", no = "S"), " DETECTED:\n", 
            base::paste0(internal_fun_names, collapse = "\n", recycle0 = FALSE),
            collapse = "\n", recycle0 = FALSE
        )
        # reminder: no colon problem with internal functions
    }
    # end remove internal functions in other functions (list_fun and list_fun_pos)
    if(base::length(x = list_fun) != 0){
        # pattern2 <- base::paste(base::paste0("(?<![A-Za-z0-9._])", fun.uni, "\\s*\\("), collapse = "|") # to split string according to function name as splitter. Pattern (?<![A-Za-z0-9._]) means "must not be preceeded by any alphanum or .or _
        # pattern3 <- base::paste(base::paste0("(?<![A-Za-z0-9._])", fun.uni, "\\s*\\($"), collapse = "|") # same as pattern2 but used to know if the seeked function is at the end of the string
        basic_ini <- ini[line_nb]
        if( ! (base::length(x = list_fun) == base::length(x = list_fun_pos) & base::length(x = list_fun) == base::length(x = line_nb) & base::length(x = list_fun) == base::length(x = basic_ini))){
            tempo_cat <- base::paste0("INTERNAL ERROR 3 IN ", intern_error_text_start, "LENGTHS SHOULD BE IDENTICAL\nlist_fun: ", base::length(x = list_fun), "\nlist_fun_pos: ", base::length(x = list_fun_pos), "\nline_nb: ", base::length(x = line_nb), "\nbasic_ini: ", base::length(x = basic_ini), intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        res <- list_fun_pos
        for(i1 in 1:base::length(x = basic_ini)){
            res[[i1]] <- base::mapply(FUN = function(x , y){z <- base::substr(x = x, start = 1, stop = y - 1)}, x = basic_ini[i1], y = list_fun_pos[[i1]], SIMPLIFY = TRUE, USE.NAMES = FALSE, MoreArgs = NULL)
        }
        # res <- base::strsplit(x = basic_ini, split = pattern2, perl = TRUE) # in res, all the strings should finish by ::
        # tempo.log <- ! base::grepl(x = basic_ini, pattern = pattern3, perl = TRUE) # strings of basic_ini that does not finish by the function name
        # in each compartment of res, the last split section is removed because nothing to test at the end (end of code)
        # if(base::sum(tempo.log, na.rm = TRUE) > 0){
        #     res[tempo.log] <- base::lapply(X = res[tempo.log], FUN = function(x){x[-base::length(x = x)]})
        # }
        # end in each compartment of res, the last split section is removed because nothing to test at the end (end of code)
        res2 <- base::lapply(X = res, FUN = function(x){base::substr(x = x, start = base::nchar(x = x, type = "chars", allowNA = FALSE, keepNA = NA)-1, stop = base::nchar(x = x, type = "chars", allowNA = FALSE, keepNA = NA))}) # base::nchar(x)-1 takes only :: if the strings ends by :::
        base::names(x = res2) <- NULL
        if( ! base::all(base::sapply(X = res2, FUN = function(x){base::length(x = x)}, simplify = TRUE, USE.NAMES = TRUE) == base::sapply(X = res, FUN = function(x){base::length(x = x)}, simplify = TRUE, USE.NAMES = TRUE), na.rm = TRUE)){
            tempo_cat <- base::paste0("INTERNAL ERROR 4 IN ", intern_error_text_start, "LENGTHS SHOULD BE IDENTICAL\nres2: ", base::paste0(base::sapply(X = res2, FUN = function(x){base::length(x = x)}, simplify = TRUE, USE.NAMES = TRUE), collapse = " ", recycle0 = FALSE), "\nres: ", base::paste0(base::sapply(X = res, FUN = function(x){base::length(x = x)}, simplify = TRUE, USE.NAMES = TRUE), collapse = " ", recycle0 = FALSE), intern_error_text_end, collapse = NULL, recycle0 = FALSE) : base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        colon_not_here <- base::lapply(X = res2, FUN = function(x){ ! x %in% "::"}) # no need to check for ":::" because base::nchar(x)-1 takes only :: if the strings ends by :::
        if( ! base::all(base::sapply(X = res2, FUN = function(x){base::length(x = x)}, simplify = TRUE, USE.NAMES = TRUE) == base::sapply(X = colon_not_here, FUN = function(x){base::length(x = x)}, simplify = TRUE, USE.NAMES = TRUE), na.rm = TRUE)){
            tempo_cat <- base::paste0("INTERNAL ERROR 5 IN ", intern_error_text_start, "LENGTHS SHOULD BE IDENTICAL\nres2: ", base::paste0(base::sapply(X = res2, FUN = function(x){base::length(x = x)}, simplify = TRUE, USE.NAMES = TRUE), collapse = " ", recycle0 = FALSE), "\ncolon_not_here: ", base::paste0(base::sapply(X = colon_not_here, FUN = function(x){base::length(x = x)}, simplify = TRUE, USE.NAMES = TRUE), collapse = " ", recycle0 = FALSE), intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if(base::any(base::unlist(x = colon_not_here, recursive = TRUE, use.names = TRUE), na.rm = TRUE)){
            col1 <- base::as.vector(x = base::unlist(x = base::mapply(FUN = function(x, y){base::rep(x = y, base::sum(x, na.rm = TRUE))}, x = colon_not_here, y = line_nb, SIMPLIFY = TRUE, MoreArgs = NULL, USE.NAMES = TRUE), recursive = TRUE, use.names = TRUE), mode = "any")
            col2 <- base::as.vector(x = base::unlist(x = base::mapply(FUN = function(x, y){y[x]}, x = colon_not_here, y = list_fun, SIMPLIFY = TRUE, MoreArgs = NULL, USE.NAMES = TRUE), recursive = TRUE, use.names = TRUE), mode = "any")
            col3 <- base::as.vector(x = base::unlist(x = base::mapply(FUN = function(x, y){y[x]}, x = colon_not_here, y = res, SIMPLIFY = TRUE, MoreArgs = NULL, USE.NAMES = TRUE), recursive = TRUE, use.names = TRUE), mode = "any")
            if( ! (base::length(x = col1) == base::length(x = col2) & base::length(x = col1) == base::length(x = col3) & base::length(x = col2) == base::length(x = col3))){
                tempo_cat <- base::paste0("INTERNAL ERROR 6 IN ", intern_error_text_start, "LENGTHS OF col1 (", base::length(x = col1), "), col2 (", base::length(x = col2), "), AND col3 (", base::length(x = col3), "), SHOULD BE EQUAL.", intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            # removal of functions between quotes and after $
            tempo.log <- saferDev:::.noclean_functions(
                col1 = col1, 
                col2 = col2, 
                col3 = col3, 
                ini = ini, 
                lib_path = lib_path, 
                error_text = embed_error_text
            ) # function names are inside quotes or after $ ?
            if(base::sum(tempo.log, na.rm = TRUE) > 0){
                col1 <- col1[ ! tempo.log] # keep clean functions
                col2 <- col2[ ! tempo.log] # keep clean functions
                col3 <- col3[ ! tempo.log] # keep clean functions 
            }
            # end removal of functions between quotes and after $
            if(base::length(x = col1) > 0){
                tempo.pos <- base::paste0(col1, "\t", col2, "\t\t", col3, collapse = NULL, recycle0 = FALSE)
                output.cat <- base::paste0(
                    base::ifelse(test = base::is.null(x = output.cat), yes = "", no = base::paste0(output.cat, "\n\n", collapse = NULL, recycle0 = FALSE)),
                    "INSIDE ", arg_user_setting2$x, ", SOME :: OR ::: ARE MISSING BEFORE ", text, " FUNCTIONS:\n\n", 
                    "LINE\tFUN\t\tSTRING_BEFORE\n",
                    base::paste0(tempo.pos, collapse = "\n", recycle0 = FALSE), 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
            }
        }
    }
    #### end main code

    #### warning output
    #### end warning output

    #### output
    base::return(base::list(output.cat = output.cat, colon_not_here = base::unlist(x = colon_not_here, recursive = TRUE, use.names = TRUE)))
    #### end output
}
