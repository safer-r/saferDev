#' @title .fun_args_pos
#' @description
#' Return the positions of 1st letter of the first function name in a string, as well as its own opening and closing parenthesis, as well as positions of the internal parenthesis (inside the own parenthesis).
#' @param text Single string.
#' @param pattern Single string of a perl regex to extract function name and (), using generally paste0(<FUNCTION_NAME>, "[\\s\\r\\n]*\\(").
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R package are not installed in the default directories because of lack of admin rights.  More precisely, lib_path is passed through the new argument of .libPaths() so that the new library paths are unique(c(new, .Library.site, .Library)). Warning: .libPaths() is restored to the initial paths, after function execution. Ignored if NULL (default) or if the safer_check argument is FALSE: only the pathways specified by the current .libPaths() are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>.". If NULL, converted into "".
#' @returns A list containing two positions:
#' $begin_fun: position of 1st letter of the function name.
#' $begin: position of the "(" of the function.
#' $end: position of the closing ")" of the function.
#' $middle_bracket_pos: list of positions of the couple of brackets in the middle of the begin and end positions. In each compartment, the first number is the position of ( and the second the position of ). NULL if no inside brackets.
#' @details
#' - Warning: the string must be cleaned form brackets between quotes. Use .in_quotes_replacement() for that.
#' - Warning: quotes in strings are escaped, so that position of ( in \"a( is 3, not 4.
#' - Warning: requires saferDev::arg_check. In main safer functions, in the section "######## check of the required functions from the required packages" add these functions when checking for the presence of saferDev:::.fun_args_pos
#' @author \href{gael.millot@pasteur.fr}{Gael Millot}
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function (not found by devtools::check())
#' # Warning : examples only with strings that must be cleaned from brackets between quotes
#' saferDev:::.fun_args_pos(text = ' "a" ; paste0("I", paste0(sum(1:3), collapse = " "), min(1) ) ; range(2)', pattern = paste0("paste0", "[\\s\\r\\n]*\\("), lib_path = NULL, error_text = " INSIDE P1::F1")
#' }
#' 
#' 
#' @keywords internal
.fun_args_pos <- function(
    text, 
    pattern, 
    lib_path, # required because of saferDev::arg_check()
    error_text # warning: in internal functions, error_text without default value returns a R classical non traced error message (specific of internal functions since classical functions are error_text = "")
){
    # DEBUGGING
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R")
    # text = ' "a" ; paste0("I", paste0(sum(1:3), collapse = " "), min(1) ) ; range(2)' ; pattern = paste0("paste0", "[\\s\\r\\n]*\\(") ; lib_path = NULL ; error_text = " INSIDE P1::F1"
    # function_name <- ".fun_args_pos" ; arg_user_setting = base::list(text = "a", pattern = paste0("paste0", "[\\s\\r\\n]*\\("), error_text = "INSIDE P1::F1") ; arg_names <- c("text", "pattern", "lib_path", "error_text")
    # text = 'base::gregexpr(pattern = base::paste0(pattern, "\\(#"), text = text)' ; pattern = 'gregexpr[\\s\\r\\n]*\\(' ; lib_path = NULL ; error_text = " INSIDE P1::F1"
    # function_name <- ".fun_args_pos" ; arg_user_setting = base::list(text = 'base::gregexpr(pattern = base::paste0(pattern, "\\(#"), text = text)', pattern = 'gregexpr[\\s\\r\\n]*\\(', error_text = "INSIDE P1::F1") ; arg_names <- c("text", "pattern", "lib_path", "error_text")
    # text = ' "a" ; paste0("I", paste0(sum(1:3), collapse = " "), min(1) ) ; range(2)', pattern = paste0("paste0", "[\\s\\r\\n]*\\("), lib_path = NULL, error_text = " INSIDE P1::F1")

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
        "text", 
        "pattern", 
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
        "text", 
        "pattern"
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
        "text", 
        "pattern", 
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
    tempo <- saferDev::arg_check(data = text, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = pattern, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
        "text", # a priori cannot be ""
        "pattern"
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())  #  need to test is.null() here
    if(base::any(tempo_log, na.rm = TRUE)){
        # This check is here in case the developer has not correctly fill tempo_arg
        tempo_cat <- base::paste0(
            "INTERNAL ERROR IN THE BACKBONE PART OF ", 
            intern_error_text_start, 
            "IN THE SECTION \"management of \"\" in arguments of mode character\"\n", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS ARE", no = "THIS ARGUMENT IS"), 
            " NOT CLASS \"character\":\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }else{
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
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    check_pos <- function(
        intern_error_nb,
        x, 
        text, 
        pattern, 
        fun_pos, 
        intern_error_text_start, 
        intern_error_text_end
        ){
        if(base::length(x = x) != 1 | base::any(base::is.na(x = x), na.rm = TRUE) | base::is.null(x = x) | base::any(x < 0 , na.rm = TRUE)){
            tempo_name <- base::paste0("check_pos_fun_args_pos_internal_error_", intern_error_nb, ".txt")
            base::cat(text, file = tempo_name, sep = "", fill = FALSE, labels = NULL, append = FALSE)
            tempo_cat <- base::paste0(
                "INTERNAL ERROR ", 
                intern_error_nb, 
                " IN ",
                intern_error_text_start, 
                "INTERNAL FUNCTION DID NOT PROPERLY DETECT THE POSITION OF\n", 
                x, 
                "\ntext: ", 
                # base::paste0(text, collapse = "\n", recycle0 = FALSE), 
                "\npattern: ", 
                base::paste0(pattern, collapse = "\n", recycle0 = FALSE), 
                "\nfun_pos: ", 
                base::paste0(fun_pos, collapse = "\n", recycle0 = FALSE), 
                intern_error_text_end, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }

    while_loop <- function(
        intern_error_nb,
        start,
        all_pos, 
        open_pos,
        close_pos,
        text,
        pattern, 
        intern_error_text_start, 
        intern_error_text_end
    ){
        count <- 1 # 1 because ( of the function is already opened. When count == 0, we will have the closing )
        final_pos <- base::which(x = all_pos == start, arr.ind = FALSE, useNames = TRUE) #start by the first ( but will be incremented
        loop.nb <- 1 
        while(count != 0 & loop.nb < base::length(x = all_pos)){
            final_pos <- final_pos + 1
            if(all_pos[final_pos] %in% open_pos){
                count <- count + 1
            }
            if(all_pos[final_pos] %in% close_pos){
                count <- count - 1
            }
            loop.nb <- loop.nb + 1
        }
        if(count != 0){
            tempo_name <- base::paste0("while_loop_fun_args_pos_internal_error_", intern_error_nb, ".txt")
            base::cat(text, file = tempo_name, sep = "", fill = FALSE, labels = NULL, append = FALSE)
            tempo_cat <- base::paste0(
                "INTERNAL ERROR ", 
                intern_error_nb, 
                " IN ",
                intern_error_text_start, 
                "INTERNAL FUNCTION DID NOT PROPERLY DETECT THE POSITION OF THE CLOSING BRACKET.\n", 
                "\nstart: ",
                start, 
                "\nall_pos: ",
                all_pos, 
                "\nopen_pos: ",
                open_pos,
                "\nclose_pos: ",
                close_pos,
                "\ntext:\nSEE THE FILE ",
                tempo_name, 
                "\npattern: ", 
                base::paste0(pattern, collapse = "\n", recycle0 = FALSE), 
                "\ncount: ", 
                base::paste0(count, collapse = "\n", recycle0 = FALSE), 
                intern_error_text_end, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            base::return(final_pos)
        }
    }

    open_paren_pos <- base::as.vector(x = base::gregexpr(pattern = "\\(", text = text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)[[1]], mode = "any")
    close_paren_pos <- base::as.vector(x = base::gregexpr(pattern = "\\)",  text = text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)[[1]], mode = "any")
    # left position
    fun_pos <- base::as.vector(x = base::gregexpr(pattern = pattern,  text = text, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)[[1]][1], mode = "any") # position of the 1st character of fun in text
    check_pos(
        intern_error_nb = 1, 
        x = fun_pos, 
        text = text, 
        pattern = pattern, 
        fun_pos = fun_pos, 
        intern_error_text_start = intern_error_text_start, 
        intern_error_text_end = intern_error_text_end
    )
    # end left position
    fun_open_paren_pos <- open_paren_pos[open_paren_pos > fun_pos][1] # position of ( of the fonction
    check_pos(
        intern_error_nb = 2, 
        x = fun_open_paren_pos, 
        text = text, 
        pattern = pattern, 
        fun_pos = fun_pos, 
        intern_error_text_start = intern_error_text_start, 
        intern_error_text_end = intern_error_text_end
    )
    # detection of the closing ) of the function
    all_pos <- base::sort(x = base::c(open_paren_pos, close_paren_pos), decreasing = FALSE)
    final_pos <- while_loop(
        intern_error_nb = 3, 
        start = fun_open_paren_pos,
        all_pos = all_pos, 
        open_pos = open_paren_pos, 
        close_pos = close_paren_pos, 
        text = text,
        pattern = pattern, 
        intern_error_text_start = intern_error_text_start, 
        intern_error_text_end = intern_error_text_end
    )
    fun_close_paren_pos <- all_pos[final_pos]
    # end detection of the closing ) of the function
    # middle brackets
    tempo_log <- all_pos > fun_open_paren_pos & all_pos < fun_close_paren_pos
    if(base::any(tempo_log, na.rm = TRUE)){
        all_pos_inside <- all_pos[tempo_log] # all positions of the brackets inside fun(    )
        open_paren_pos_inside <- all_pos_inside[all_pos_inside %in% open_paren_pos]
        count_open_paren_pos_inside <- base::length(x = open_paren_pos_inside)
        close_paren_pos_inside <- all_pos_inside[all_pos_inside %in% close_paren_pos]
        count_close_paren_pos_inside <- base::length(x = open_paren_pos_inside)
        if(count_open_paren_pos_inside != count_close_paren_pos_inside | count_open_paren_pos_inside == 0){ #count_open_paren_pos_inside == 0 because tempo_log above has some TRUE
            tempo_cat <- base::paste0(
                "INTERNAL ERROR 4 IN ", 
                intern_error_text_start, 
                "THE ", 
                function_name, 
                " INTERNAL FUNCTION DID NOT PROPERLY DETECT THE POSITION OF ALL THE BRACKETS INSIDE THE FUN(    ) BRACKETS.", 
                "\ntext: ", 
                base::paste0(text, collapse = "\n", recycle0 = FALSE), 
                "\npattern: ", 
                base::paste0(pattern, collapse = "\n", recycle0 = FALSE), 
                "\nCOUNT OF OPENED BRACKETS: ", 
                count_open_paren_pos_inside, 
                "\nCOUNT OF CLOSING BRACKETS: ", 
                count_close_paren_pos_inside, 
                "\nCHECK THAT THE STRING HAS ALL THE BRACKETS BETWEEN QUOTES REMOVED.", 
                intern_error_text_end, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        middle_bracket_pos <- base::vector(mode = "list", length = count_open_paren_pos_inside)
        for(i2 in 1:count_open_paren_pos_inside){
            final_pos2 <- while_loop(
                intern_error_nb = 5, 
                start = open_paren_pos_inside[i2],
                all_pos = all_pos_inside, 
                open_pos = open_paren_pos_inside, 
                close_pos = close_paren_pos_inside, 
                text = text,
                pattern = pattern, 
                intern_error_text_start = intern_error_text_start, 
                intern_error_text_end = intern_error_text_end
            )
            middle_bracket_pos[[i2]] <- base::c(open_paren_pos_inside[i2], all_pos_inside[final_pos2])
        }
    }else{
        middle_bracket_pos <- NULL
    }
    # end middle brackets
    output <- base::list(
        begin_fun = fun_pos, 
        begin = fun_open_paren_pos,
        end = fun_close_paren_pos,
        middle_bracket_pos = middle_bracket_pos
    )

    #### warning output
    #### end warning output

    #### output
    base::return(output)
    #### end output

}




