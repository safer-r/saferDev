#' @title .functions_detect
#' @description
#' Detect all the functions names used inside a function.
#' @param x Function name, written without quotes and brackets.
#' @param arg_user_setting Argument user settings list.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = "INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>".
#' @param internal_error_report_link Single string of the link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message.
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
#' - Does not check if the functions inside the code exist.
#' - Use the regex pattern "([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*\\(" to detect a function in the code.
#' - $all_basic_funs are all the functions in base::c("package:stats", "package:graphics",  "package:grDevices", "package:utils", "package:datasets", "package:methods", "Autoloads", "package:base")
#' - Warning: requires saferDev::arg_check, saferDev:::.extract_all_fun_names, saferDev:::.has_odd_number_of_quotes. In the safer Backbone section "######## check of the required functions from the required packages" add these functions when checking for the presence of saferDev:::.functions_detect.
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function
#' source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\dev\\other\\test.R") ; .functions_detect(x = test, arg_user_setting = base::list(x =  as.name(x = "test")), error_text = " INSIDE P1::F1", internal_error_report_link = "test")
#' }
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @keywords internal
#' @rdname internal_function
.functions_detect <- function(
    # in internal functions, all arguments are without value on purpose
    x, 
    arg_user_setting, 
    error_text,
    internal_error_report_link
){
    # DEBUGGING
    # x = x ; arg_user_setting = arg_user_setting ; error_text = "" ; internal_error_report_link = "test"
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\dev\\other\\test2.R") ; x = test2 ; arg_user_setting = base::list(x = as.name(x = "test2"), error_text = " INSIDE P1::F1", internal_error_report_link = "test") ; error_text = " INSIDE P1::F1" ; internal_error_report_link = "test"
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\.github\\profile\\backbone.R") ; x = BACKBONE ; arg_user_setting = base::list(x = as.name(x = "BACKBONE"), error_text = " INSIDE P1::F1", internal_error_report_link = "test") ; error_text = " INSIDE P1::F1" ; internal_error_report_link = "test"
    # FUN1 <- function(x, y){middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, quote = FALSE, envir = base::parent.frame())} ; x = FUN1 ; arg_user_setting = base::list(x = as.name(x = "FUN1"), error_text = " INSIDE P1::F1", internal_error_report_link = "test") ; error_text = " INSIDE P1::F1" ; internal_error_report_link = "test"
    # FUN1 <- function(x, y){FUN2 <- function(x){x = 1}} ; x = FUN1 ; arg_user_setting = base::list(x = as.name(x = "FUN1"), error_text = " INSIDE P1::F1", internal_error_report_link = "test") ; error_text = " INSIDE P1::F1" ; internal_error_report_link = "test"

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
    # already done in the main function.
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    # already done in the main function
    ######## end critical operator checking

    #### end environment checking

    #### argument primary checking

    ######## arg with no default values
    mandat_args <- base::c(
        "x", 
        "arg_user_setting", 
        "error_text",
        "internal_error_report_link"
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
        "x", 
        "arg_user_setting", 
        "error_text"
        # "internal_error_report_link" # inactivated because can be NULL
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
    tempo <- saferDev::arg_check(data = x, class = NULL, typeof = NULL, mode = "function", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = NULL, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    tempo <- saferDev::arg_check(data = arg_user_setting, class = NULL, typeof = NULL, mode = "list", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = NULL, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    # error_text already checked above
    if( ! base::is.null(x = internal_error_report_link)){ # for all arguments that can be NULL, write like this:
        tempo <- saferDev::arg_check(data = internal_error_report_link, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, lib_path = NULL, safer_check = FALSE,  error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
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
    # all character arguments can be ""
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
    # modification of arg_user_setting$x for clean messages
    if(base::as.character(x = arg_user_setting$x)[1] == "::" | base::as.character(x = arg_user_setting$x)[1] == ":::"){
        arg_user_setting$x <- base::paste0(base::as.character(x = arg_user_setting$x)[3], "()")
    }
    # end modification of arg_user_setting$x for clean messages
    # recovering the basic functions of R
    s <- base::c("package:stats", "package:graphics",  "package:grDevices", "package:utils", "package:datasets", "package:methods", "Autoloads", "package:base") # basic base::search() scope
    if(base::any( ! s %in% base::search())){
        tempo.cat <- base::paste0(
            "INTERNAL ERROR 1 IN ",
            intern_error_text_start, 
            "THE base::search() SCOPE OF R HAS CHANGED.\nTHE PROBLEM IS:\n",
            base::paste(s[ ! s %in% base::search()], collapse = "\n"), 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    fun <- base::unlist(base::sapply(X = s, FUN = function(x){base::ls(x, all.names = TRUE)})) # all the basic functions of R in all the scope
    # end recovering the basic functions of R
    # recovering the input function string
    code <- utils::capture.output(x) # no lines must be removed because it is to catch the lines of the full code
    code_line_nb <- 1:base::length(code)
    # code <- base::paste0(code, collapse = " \\n ") # recovering as single string separated by \\n (and not \n to avoid the eval(\n) when printing the error message)
    code <- base::gsub(x = code, pattern = " +", replacement = " ") # removal of multiple spaces
    code <- base::sub(x = code, pattern = "^ +", replacement = "") # removal of multiple spaces in the beginning od strings
    # end recovering the input function string

    # removal of empty lines
    empty_line.log <- base::grepl(code, pattern = "^\\s*$")
    # end removal of empty lines

    # removal of comments
    comment_line.log <- base::grepl(code, pattern = "^\\s*#") # removal of the lines starting by #
    if(base::length(code) == 0){
        tempo.cat <- base::paste0(
            error_text_start, 
            "THE TESTED FUNCTION ", 
            arg_user_setting$x, 
            " IS EMPTY OR ONLY MADE OF COMMENTS.", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    comment.log <- base::grepl(x = code, pattern = "#")
    if(base::any(comment.log, na.rm = TRUE)){
        comment.line.to.rm <- base::which(comment.log) # elements among code that have #
        lines <- code[comment.log]
        for(i2 in 1:base::length(lines)){
            lines.split <- base::strsplit(lines[i2], split = "#")[[1]]
            # detection of the first left # that is not between quotes
            count <- 1
            tempo.line <- lines.split[1]
            while.loop <- TRUE
            while(while.loop == TRUE & count < base::length(lines.split)){
                # if odds number of quotes, it means that # has broken the string in the middle of a quoted part
                double.quote.test <- .has_odd_number_of_quotes(
                    input_string = tempo.line, 
                    pattern = '"', 
                    error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
                    internal_error_report_link = internal_error_report_link
                ) # here FALSE means even number of quotes, thus that # is not between quotes, thus has to be removed. TRUE means that # is between quotes, thus has to be kept
                simple.quote.test <- .has_odd_number_of_quotes(
                    input_string = tempo.line, 
                    pattern = "'", 
                    error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
                    internal_error_report_link = internal_error_report_link
                ) # idem
                odds.quotes.log <- double.quote.test |  simple.quote.test # lines to keep among commented lines
                if(odds.quotes.log == TRUE){
                    count <- count + 1
                    tempo.line <- base::paste0(tempo.line, "#", lines.split[count])
                }else{
                     while.loop <- FALSE
                }
            }
            # end detection of the first left # that is not between quotes
            lines[i2] <- tempo.line
        }
        code[comment.line.to.rm] <- lines
    }
    # end removal of comments
    # catch the internal function name created inside the tested function
    internal_fun_names <- base::unlist(base::lapply(X = code, FUN = function(x){
        output <- base::sub(pattern = "^\\s*(([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*)\\s*<-[\\s\\r\\n]*function[\\s\\r\\n]*\\(.*", replacement = "\\1", x = x, perl = TRUE)
        # ^\\s* means in perl: 0 or any spaces at the begining of the string
        # ([a-zA-Z]|\\.[a-zA-Z._]) is for the begining of R function name: either any single alphabet character or a dot and any single alphabet character or dot (because .. is ok for function name) or underscore (because ._ is ok for function name). Starting "dot and num" or underscore is not authorized for function name
        # [a-zA-Z0-9._]* is The rest of the function name: any several of these characters or nothing
        #  \\s*<-[\\s\\r\\n]*function[\\s\\r\\n]*\\(.* 0 or any space, assignation symbol, 0 or any (space, carriage return, end of line), an opening parenthesis, and any character
        if( ! output == x){
            base::return(output)
        }
    })) # To achieve the extraction of the function names, you need to wrap the part of the pattern that matches the function name in parentheses () to create a capturing group
    # end catch the internal function name created inside the tested function
    # trick to deal with end of lines between the name of the function and "("
    if(base::length(code) > 1){
        for (i2 in 2:base::length(code)) {
            # Check if the current string starts with spaces followed by a '('
            if (base::grepl("^\\s*\\(", code[i2])) {
                # Check if the previous string ends with the specified pattern
                if (base::grepl("([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*$", code[i2 - 1])) {
                # Append a '(' to the previous string
                code[i2 - 1] <- base::paste0(code[i2 - 1], "(")
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
    for(i1 in 1:base::length(code)){
        tempo <- .extract_all_fun_names(
            text = code[i1], 
            pattern = pattern1,
            error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE),
            internal_error_report_link = internal_error_report_link
        ) # recover all the function names, followed by "(", present in code, using a perl pattern
        fun_name <- base::c(fun_name, base::list(tempo$string))
        fun_name_pos <- base::c(fun_name_pos, base::list(tempo$pos))
    }
    # tempo <- base::lapply(code, FUN = function(x){saferDev:::.extract_all_fun_names(text = x, pattern = pattern1)})
    # removal of special functions
    tempo_log <- base::lapply(fun_name, FUN = function(x){ ! x %in% base::c("function", "if", "for", "while", "repeat")})
    fun_name_wo_op <- base::mapply(FUN = function(x, y){x[y]}, x = fun_name, y = tempo_log, SIMPLIFY = FALSE)
    fun_name_pos_wo_op <- base::mapply(FUN = function(x, y){x[y]}, x = fun_name_pos, y = tempo_log, SIMPLIFY = FALSE)
    # end removal of special functions
    # removal of empty string
    tempo.log <- base::sapply(fun_name_wo_op, FUN = function(x){base::length(x) == 0}) # detection of string with empty function names
    fun_name_wo_op <- fun_name_wo_op[ ! tempo.log]
    fun_name_pos_wo_op <- fun_name_pos_wo_op[ ! tempo.log]
    code_line_nb <- code_line_nb[( ! tempo.log) & ( ! comment_line.log) & ( ! empty_line.log)]
    if( ! (base::length(fun_name_wo_op) == base::length(fun_name_pos_wo_op) & base::length(fun_name_wo_op) == base::length(code_line_nb))){
        tempo.cat <- base::paste0(
            "INTERNAL ERROR 2 IN ",
            intern_error_text_start, 
            "LENGTHS SHOULD BE IDENTICAL\nfun_name_wo_op: ", 
            base::length(fun_name_wo_op), 
            "\nfun_name_pos_wo_op: ", 
            base::length(fun_name_pos_wo_op), 
            "\ncode_line_nb: ", 
            base::length(code_line_nb), 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }else if(base::any(base::is.na(code_line_nb))){
        tempo.cat <- base::paste0(
            "INTERNAL ERROR 3 IN ", 
            intern_error_text_start, 
            "code_line_nb SHOULD NOT CONTAIN NA.\ncode_line_nb:\n", 
            base::paste0(code_line_nb, collapse = "\n"), 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }else{
        # with that, now the code line of code is indicated in as compartment names
        base::names(fun_name_wo_op) <- base::paste0("c", code_line_nb)
        base::names(fun_name_pos_wo_op) <- base::paste0("c", code_line_nb)
    }
    # end removal of empty string
    test.log <- base::mapply(FUN = function(x, y){base::length(x) != base::length(y)}, x = fun_name_wo_op, y = fun_name_pos_wo_op, SIMPLIFY = TRUE)
    if(base::any(test.log, na.rm = TRUE)){
        tempo.cat <- base::paste0(
            "INTERNAL ERROR 4 IN ",
            intern_error_text_start, 
            "LENGTHS SHOULD BE IDENTICAL IN COMPARTMENTS ", 
            base::paste0(base::which(test.log), collapse = ", "), 
            " OF fun_name_wo_op AND fun_name_pos_wo_op", 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # fun_name_wo_op_uni <- base::unlist(base::unique(fun_name_wo_op)) # in case
    # end all function names in x
    #### output
    output <- base::list(
        code = code, 
        all_basic_funs = fun, 
        fun_names = fun_name_wo_op, 
        fun_names_pos = fun_name_pos_wo_op, 
        code_line_nb = code_line_nb, 
        internal_fun_names = internal_fun_names,
        arg_user_setting = arg_user_setting
    )
    base::return(output)
    #### end output
}

