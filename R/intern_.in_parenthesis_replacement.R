#' @title .in_parenthesis_replacement
#' @description
#' Replace any pattern inside () by another replacement pattern
#' @param string Single string.
#' @param pattern Single string indicating the pattern to detect. Warning : must be very simple pattern, like ",".
#' @param no_regex_pattern Single string of the pattern to detect but without escape characters or list, etc.
#' @param replacement Single string for pattern replacement. Is not regex.
#' @param perl Single logical value. Use Perl regex in pattern ?
#' @param open_pos Single positive and non zero integer indicating the position of the opening parenthesis.
#' @param close_pos Single positive and non zero integer indicating the position of the closing parenthesis.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R package are not installed in the default directories because of lack of admin rights.  More precisely, lib_path is passed through the new argument of .libPaths() so that the new library paths are unique(c(new, .Library.site, .Library)). Warning: .libPaths() is restored to the initial paths, after function execution. Ignored if NULL (default) or if the safer_check argument is FALSE: only the pathways specified by the current .libPaths() are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>.". If NULL, converted into "".
#' @returns A list containing:
#' $string: The input string with all pattern replaced by the replacement pattern.
#' $pos: the positions of the 1rst character of the replaced pattern. NULL if no replaced pattern. In that case, $string is identical to the input string
#' @details
#' - Warning : must be very simple pattern, like "\\(".
#' - Warning: requires saferDev::arg_check. In main safer functions, in the section "######## check of the required functions from the required packages" add these functions when checking for the presence of saferDev:::.in_parenthesis_replacement.
#' @author \href{gael.millot@pasteur.fr}{Gael Millot}
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function (not found by devtools::check())
#' saferDev:::.in_parenthesis_replacement(string = "pattern = base::paste0(pattern, \"\\\\(#\"), text = text", pattern = ",", no_regex_pattern = ",", replacement = " ", perl = TRUE, open_pos = 23, close_pos = 39,  lib_path = NULL, error_text = " INSIDE P1::F1")
#' }
#' 
#' 
#' @keywords internal
.in_parenthesis_replacement <- function(
    string, 
    pattern, 
    no_regex_pattern, 
    replacement, 
    perl,
    open_pos,
    close_pos,
    lib_path, # required because of saferDev::arg_check()
    error_text # warning: in internal functions, error_text without default value returns a R classical non traced error message (specific of internal functions since classical functions are error_text = "")
){
    # DEBUGGING
    # string = "pattern = base::paste0(pattern, \"\\\\(#\"), text = text" ; pattern = "," ; no_regex_pattern = "," ; replacement = " " ; perl = TRUE ; open_pos = 23 ; close_pos = 39 ; lib_path = NULL ; error_text = " INSIDE P1::F1"

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
        "string", 
        "pattern", 
        "no_regex_pattern", 
        "replacement", 
        "perl",
        "open_pos",
        "close_pos",
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
    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "string", 
        "pattern", 
        "no_regex_pattern", 
        "replacement", 
        "perl",
        "open_pos",
        "close_pos"
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
        "string", 
        "pattern", 
        "no_regex_pattern", 
        "replacement", 
        "perl",
        "open_pos",
        "close_pos", 
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
    # not used here
    ######## end safer_check argument checking

    ######## check of lib_path
    # check already done in the main safer function
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    # saferDev::arg_check is required here
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
    tempo <- saferDev::arg_check(data = string, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = pattern, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = no_regex_pattern, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = replacement, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = perl, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = open_pos, class = "vector", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = close_pos, class = "vector", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
    # no need to test neg or 0 value for open_pos and close_pos, because tested downstream with the pos of pattern
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    if(base::substr(x = string, start =  open_pos, stop =  open_pos) != "("){
        tempo_cat <- base::paste0(
            error_text_start, 
            "ARGUMENT open_pos DOES NOT REFER TO A POSITION OF OPENING PARENTHESIS.\nopen_pos:\n", 
            base::paste0(open_pos, collapse = "\n", recycle0 = FALSE), 
            "\nstring:\n", 
            base::paste0(string, collapse = "\n", recycle0 = FALSE), 
            "\nsubstr(string, open_pos, open_pos):\n", 
            base::paste0(base::substr(x = string, start =  open_pos, stop =  open_pos), collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if(base::substr(x = string, start =  close_pos, stop =  close_pos) != ")"){
        tempo_cat <- base::paste0(
            error_text_start, 
            "ARGUMENT close_pos DOES NOT REFER TO A POSITION OF CLOSING PARENTHESIS.\nclose_pos:\n", 
            base::paste0(close_pos, collapse = "\n", recycle0 = FALSE), 
            "\nstring:\n", 
            base::paste0(string, collapse = "\n", recycle0 = FALSE), 
            "\nsubstr(string, close_pos, close_pos):\n", 
            base::paste0(base::substr(x = string, start =  close_pos, stop =  close_pos), collapse = "\n", recycle0 = FALSE),  
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    string_out <- string
    # Extract the substring between the given open and close parentheses
    substring_in_parentheses <- base::substr(x = string_out, start =  open_pos, stop =  close_pos)
    # Find the position of comma within that substring
    comma_position_in_substring <- base::gregexpr(pattern = pattern, text =  substring_in_parentheses, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)[[1]]
    # Initialize a vector to store global positions of the replaced commas
    pos <- NULL
    if(comma_position_in_substring[1] != -1){
        for (relative_comma_position in comma_position_in_substring) {
            # Calculate the global position of the comma in the original string
            global_comma_position <- open_pos + relative_comma_position - 1
            # Replace that comma using substring or substr
            base::substring(text = string_out, first =  global_comma_position, last =  global_comma_position) <- replacement
            # Store the global position
            pos <- base::c(pos, global_comma_position)
        }
    }
    if( ! base::is.null(x = pos)){
        tempo <- base::substring(text = string, first =  pos, last =  pos)
        if( ! base::all(base::unique(x = tempo, incomparables = FALSE) == no_regex_pattern, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                "ARGUMENT no_regex_pattern NOT CORRECTLY DETECTED.\nno_regex_pattern: \"", 
                no_regex_pattern, 
                "\"\nREPLACED CHARACTERS IN string ARGUMENT:\n", 
                base::paste0(tempo, collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # Return both the modified string and positions of replaced commas
    #### end main code

    #### warning output
    #### end warning output

    #### output
    base::return(base::list(string = string_out, pos = pos))
    #### end output

}

