#' @title .functions_detect
#' @description
#' Detect all the functions names used inside a function.
#' @param x Function name, written without quotes and brackets.
#' @param skipped_base Vector of strings specifying the basic functions skipped from detection.
#' @param arg_user_setting2 Argument user settings list.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R package are not installed in the default directories because of lack of admin rights.  More precisely, lib_path is passed through the new argument of .libPaths() so that the new library paths are unique(c(new, .Library.site, .Library)). Warning: .libPaths() is restored to the initial paths, after function execution. Ignored if NULL (default) or if the safer_check argument is FALSE: only the pathways specified by the current .libPaths() are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>.". If NULL, converted into "".
#' @returns 
#'  A list:
#' $code: vector of strings of the code of the tested function.
#' $all_basic_funs: vector or strings of names of all the basic R functions.
#' $fun_names: list of names of all the functions, not considering base::c("function", "if", "for", "while", "repeat"). Compartment names indicate the code line number of the functions in $code.
#' $fun_names_pos: list of position of the first character of each $fun_names. Compartment names indicate the code line number of the functions in $code.
#' $code_line_nb: vector of integers of the code line numbers of code for each non empty compartment of $fun_names and $fun_names_pos.
#' $internal_fun_names: vector of string of names of internal functions in the code of the tested function.
#' $arg_user_setting: list of arg user settings of the tested function.
#' @details
#' Does not check if the functions inside the code exist.
#' 
#' Use the regex pattern "([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*\\(" to detect a function in the code.
#' 
#' $all_basic_funs are all the functions in base::c("package:stats", "package:graphics",  "package:grDevices", "package:utils", "package:datasets", "package:methods", "Autoloads", "package:base").
#' 
#' Warning: requires saferDev::arg_check, saferDev:::.extract_all_fun_names, saferDev:::.has_odd_number_of_quotes. In main safer functions, in the section "######## check of the required functions from the required packages" add these functions when checking for the presence of saferDev:::.functions_detect.
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function (not found by devtools::check())
#' source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\dev\\other\\test.R") ; saferDev:::.functions_detect(x = test, arg_user_setting2 = base::list(x =  as.name(x = "test")), skipped_base = c("function", "if", "for", "while", "repeat", "else"), lib_path = NULL, error_text = " INSIDE P1::F1")
#' }
#' @author \href{gael.millot@pasteur.fr}{Gael Millot}
#' 
#' 
#' @keywords internal
.functions_detect <- function(
    # in internal functions, all arguments are without value on purpose
    x, 
    skipped_base, 
    arg_user_setting2, 
    lib_path, # required because of saferDev::arg_check()
    error_text # warning: in internal functions, error_text without default value returns a R classical non traced error message (specific of internal functions since classical functions are error_text = "")
){
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
        "x", 
        "skipped_base", 
        "arg_user_setting2", 
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
        "x", 
        "skipped_base", 
        "arg_user_setting2"
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
    # saferDev:::.extract_all_fun_names is required here
    # saferDev:::.has_odd_number_of_quotes is required here
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
    tempo <- saferDev::arg_check(data = x, class = NULL, typeof = NULL, mode = "function", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = skipped_base, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = arg_user_setting2, class = NULL, typeof = NULL, mode = "list", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
    fun <- base::unlist(x = base::sapply(X = s, FUN = function(x){base::ls(name = x, pos = , envir = , all.names = TRUE, pattern = , sorted = TRUE)}, simplify = TRUE, USE.NAMES = TRUE), recursive = TRUE, use.names = TRUE) # all the basic functions of R in all the scope
    # end recovering the basic functions of R
    # recovering the input function string
    code <- utils::capture.output(x, file = NULL, append = FALSE, type = NULL, split = FALSE) # no lines must be removed because it is to catch the lines of the full code
    code_line_nb <- 1:base::length(x = code)
    # code <- base::paste0(code, collapse = " \\n ") # recovering as single string separated by \\n (and not \n to avoid the eval(\n) when printing the error message)
    code <- base::gsub(x = code, pattern = " +", replacement = " ", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # removal of multiple spaces
    code <- base::sub(x = code, pattern = "^ +", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # removal of multiple spaces in the beginning od strings
    # end recovering the input function string

    # removal of empty lines
    empty_line.log <- base::grepl(x = code, pattern = "^\\s*$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    # end removal of empty lines

    # removal of comments
    comment_line.log <- base::grepl(x = code, pattern = "^\\s*#", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # removal of the lines starting by #
    # no test for empty code here because code style contains "function" as fisrt line of code

    comment.log <- base::grepl(x = code, pattern = "#", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    if(base::any(comment.log, na.rm = TRUE)){
        comment.line.to.rm <- base::which(x = comment.log, arr.ind = FALSE, useNames = TRUE) # elements among code that have #
        lines <- code[comment.log]
        for(i2 in 1:base::length(x = lines)){
            lines.split <- base::strsplit(x = lines[i2], split = "#", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]]
            # detection of the first left # that is not between quotes
            count <- 1
            tempo.line <- lines.split[1]
            while.loop <- TRUE
            while(while.loop == TRUE & count < base::length(x = lines.split)){
                # if odds number of quotes, it means that # has broken the string in the middle of a quoted part
                double.quote.test <- saferDev:::.has_odd_number_of_quotes(
                    input_string = tempo.line, 
                    pattern = '"', 
                    lib_path = lib_path, 
                    error_text = embed_error_text
                ) # here FALSE means even number of quotes, thus that # is not between quotes, thus has to be removed. TRUE means that # is between quotes, thus has to be kept
                simple.quote.test <- saferDev:::.has_odd_number_of_quotes(
                    input_string = tempo.line, 
                    pattern = "'", 
                    lib_path = lib_path, 
                    error_text = embed_error_text
                ) # idem
                odds.quotes.log <- double.quote.test |  simple.quote.test # lines to keep among commented lines
                if(odds.quotes.log == TRUE){
                    count <- count + 1
                    tempo.line <- base::paste0(tempo.line, "#", lines.split[count], collapse = NULL, recycle0 = FALSE)
                }else{
                     while.loop <- FALSE
                }
            }
            # end detection of the first left # that is not between quotes
            lines[i2] <- tempo.line
        }
        code[comment.line.to.rm] <- lines
    }
    # no test for empty code here because code style contains "function" as fisrt line of code
    # end removal of comments
    # catch the internal function name created inside the tested function
    internal_fun_names <- base::unlist(x = base::lapply(X = code, FUN = function(x){
        output <- base::sub(pattern = "^\\s*(([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*)\\s*<-[\\s\\r\\n]*function[\\s\\r\\n]*\\(.*", replacement = "\\1", x = x, perl = TRUE, ignore.case = FALSE, fixed = FALSE, useBytes = FALSE)
        # ^\\s* means in perl: 0 or any spaces at the begining of the string
        # ([a-zA-Z]|\\.[a-zA-Z._]) is for the begining of R function name: either any single alphabet character or a dot and any single alphabet character or dot (because .. is ok for function name) or underscore (because ._ is ok for function name). Starting "dot and num" or underscore is not authorized for function name
        # [a-zA-Z0-9._]* is The rest of the function name: any several of these characters or nothing
        #  \\s*<-[\\s\\r\\n]*function[\\s\\r\\n]*\\(.* 0 or any space, assignation symbol, 0 or any (space, carriage return, end of line), an opening parenthesis, and any character
        if( ! output == x){
            base::return(output)
        }
    }), recursive = TRUE, use.names = TRUE) # To achieve the extraction of the function names, you need to wrap the part of the pattern that matches the function name in parentheses () to create a capturing group
    # end catch the internal function name created inside the tested function
    # trick to deal with end of lines between the name of the function and "("
    if(base::length(x = code) > 1){
        for (i2 in 2:base::length(x = code)) {
            # Check if the current string starts with spaces followed by a '('
            if (base::grepl(pattern = "^\\s*\\(", x = code[i2], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) {
                # Check if the previous string ends with the specified pattern
                if (base::grepl(pattern = "([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*$", x = code[i2 - 1], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) {
                # Append a '(' to the previous string
                code[i2 - 1] <- base::paste0(code[i2 - 1], "(", collapse = NULL, recycle0 = FALSE)
                }
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
    for(i1 in 1:base::length(x = code)){
        tempo <- saferDev:::.extract_all_fun_names(
            text = code[i1], 
            pattern = pattern1, 
            lib_path = lib_path, 
            error_text = embed_error_text
        ) # recover all the function names, followed by "(", present in code, using a perl pattern
        fun_name <- base::c(fun_name, base::list(tempo$string))
        fun_name_pos <- base::c(fun_name_pos, base::list(tempo$pos))
    }
    # tempo <- base::lapply(code, FUN = function(x){saferDev:::.extract_all_fun_names(text = x, pattern = pattern1)})
    # removal of special functions
    tempo_log <- base::lapply(X = fun_name, FUN = function(x){ ! x %in% skipped_base})
    fun_name_wo_op <- base::mapply(FUN = function(x, y){x[y]}, x = fun_name, y = tempo_log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    fun_name_pos_wo_op <- base::mapply(FUN = function(x, y){x[y]}, x = fun_name_pos, y = tempo_log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    if(base::length(x = unlist(fun_name_wo_op)) == 0 | base::length(x = unlist(fun_name_pos_wo_op)) == 0){
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE TESTED FUNCTION ", 
            arg_user_setting2$x, 
            " IS EMPTY OF FUNCTION OR ONLY MADE OF COMMENTS OR ONLY MADE OF THE SKIPPED FUNCTIONS:\n", 
            base::paste0(skipped_base, collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # end removal of special functions
    # removal of empty string
    tempo.log <- base::sapply(X = fun_name_wo_op, FUN = function(x){base::length(x = x) == 0}, simplify = TRUE, USE.NAMES = TRUE) # detection of string with empty function names
    fun_name_wo_op <- fun_name_wo_op[ ! tempo.log]
    fun_name_pos_wo_op <- fun_name_pos_wo_op[ ! tempo.log]
    code_line_nb <- code_line_nb[( ! tempo.log) & ( ! comment_line.log) & ( ! empty_line.log)]
    if( ! (base::length(x = fun_name_wo_op) == base::length(x = fun_name_pos_wo_op) & base::length(x = fun_name_wo_op) == base::length(x = code_line_nb))){
        tempo_cat <- base::paste0("INTERNAL ERROR 2 IN ", intern_error_text_start,  "LENGTHS SHOULD BE IDENTICAL\nfun_name_wo_op: ",  base::length(x = fun_name_wo_op),  "\nfun_name_pos_wo_op: ",  base::length(x = fun_name_pos_wo_op),  "\ncode_line_nb: ",  base::length(x = code_line_nb),  intern_error_text_end,  collapse = NULL,  recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }else if(base::any(base::is.na(x = code_line_nb), na.rm = TRUE)){
        tempo_cat <- base::paste0("INTERNAL ERROR 3 IN ", intern_error_text_start, "code_line_nb SHOULD NOT CONTAIN NA.\ncode_line_nb:\n", base::paste0(code_line_nb, collapse = "\n", recycle0 = FALSE), intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }else{
        # with that, now the code line of code is indicated in as compartment names
        base::names(x = fun_name_wo_op) <- base::paste0("c", code_line_nb, collapse = NULL, recycle0 = FALSE)
        base::names(x = fun_name_pos_wo_op) <- base::paste0("c", code_line_nb, collapse = NULL, recycle0 = FALSE)
    }
    # end removal of empty string
    test.log <- base::mapply(FUN = function(x, y){base::length(x = x) != base::length(x = y)}, x = fun_name_wo_op, y = fun_name_pos_wo_op, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
    if(base::any(test.log, na.rm = TRUE)){
        tempo_cat <- base::paste0("INTERNAL ERROR 4 IN ", intern_error_text_start, "LENGTHS SHOULD BE IDENTICAL IN COMPARTMENTS ", base::paste0(base::which(x = test.log, arr.ind = FALSE, useNames = TRUE), collapse = ", ", recycle0 = FALSE), " OF fun_name_wo_op AND fun_name_pos_wo_op.", intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # fun_name_wo_op_uni <- base::unlist(base::unique(fun_name_wo_op)) # in case
    # end all function names in x

    #### warning output
    #### end warning output

    #### output
    output <- base::list(
        code = code, 
        all_basic_funs = fun, 
        fun_names = fun_name_wo_op, 
        fun_names_pos = fun_name_pos_wo_op, 
        code_line_nb = code_line_nb, 
        internal_fun_names = internal_fun_names,
        arg_user_setting = arg_user_setting2
    )
    base::return(output)
    #### end output
}

