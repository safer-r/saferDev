#' @title colons_check
#' @description
#' Verify that all the functions used inside a function are all referenced by their package attribution. For instance: base::mean() and not mean(), or saferDev:::.base_op_check() and not .base_op_check().
#' @param x a function name, written without quotes and brackets.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns 
#' A table-like message indicating the missing :: or ::: or a message saying that everything seems fine.
#' Table-like: column 1, the line number in the function code (starting at the "<- function" line, i.e., without counting the #' header lines); column 2,  the function name; column 3, the code preceeding the function name
#' With missing :: or :::, the message also indicates if internal functions are created inside the checked function code, since these functions cannot have :: or :::.
#' @details
#' - More precisely, colons_check() verifies that all the strings before an opening bracket "(" are preceeded by "::" (":::" are also checked since the "::" pattern detects ":::"). Thus, it cannot check function names written without brackets, like in the FUN argument of some functions, e.g., sapply(1:3, FUN = as.character).
#' 
#' - The perl regex used to detect a function name is: "[a-zA-Z.][a-zA-Z0-9._]*\\s*\\(".
#' 
#' - Function names preceeded by $ and any space are not considered (pattern "\\$ *[a-zA-Z.][a-zA-Z0-9._]* *\\(")
#'  
#' - The following R functions using bracket are not considered: "function", "if", "for", "while" and "repeat".
#' 
#' - Most of the time, colons_check() does not check inside comments, but some unexpected writting could dupe colons_check().
#' 
#' - The returned line numbers is indicative, depending on which source is checked. For instance, saferDev::report (compiled) has not the same line numbers as its source file (https://github.com/safer-r/saferDev/blob/main/R/report.R). Notably, compiled functions do not have comments anymore, compared to the same source function sourced into the working environment. In addition, the counting starts at the "<- function" line, i.e., without counting the #' header lines potentially present in source files.
#' 
#' - Of note, during package creation, the devtools::check() command tells which functions where wrongly attributed to package. Example: 
#'     checking dependencies in R code ... WARNING
#'       '::' or ':::' import not declared from: 'sbase'
#'       Missing or unexported objects:
#'         'base::dev.off' 'base::graphics.off' 'base::hcl' 'base::par' 'base::read.table' 'saferGG::report'
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>  
#' @examples
#' colons_check(mean)
#' colons_check(colons_check)
#' source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\colons_check.R") ; colons_check(test)
#' @export
colons_check <- function(
    x, 
    safer_check = TRUE
){

    # DEBUGGING
    # x = .expand_R_libs_env_var ; safer_check = TRUE
    # library(saferGraph) ; x = close2 ; safer_check = TRUE 
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\get_message.R") ; x = get_message ; safer_check = TRUE # Warning: x = saferDev::get_message does not return the same number of code lines
    # library(saferDev) ; x = get_message ; safer_check = TRUE # Warning: does not return the same number of code lines than the previsou example
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\colons_check.R") ; x = colons_check ; safer_check = TRUE # Warning: x = saferDev::get_message does not return the same number of code lines
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\dev\\other\\test.R") ; x = test ; safer_check = TRUE # Warning: x = saferDev::get_message does not return the same number of code lines
    # package name
    package.name <- "saferDev"
    # end package name
    # function name
    function.name <- base::paste0(base::as.list(x = base::match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function.name[1] == "::()" | function.name[1] == ":::()"){
        function.name <- function.name[3]
    }
    arg.names <- base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- base::as.list(x = base::match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    if(base::as.character(x = arg.user.setting$x)[1] == "::" | base::as.character(x = arg.user.setting$x)[1] == ":::"){
        arg.user.setting$x <- base::paste0(base::as.character(x = arg.user.setting$x)[3], "()")
    }
    # end function name
    # critical operator checking
    if(safer_check == TRUE){
        saferDev:::.base_op_check(
            external.function.name = function.name,
            external.package.name = package.name
        )
    }
    # end critical operator checking
    # package checking
    # check of lib.path
    # end check of lib.path
    # check of the required function from the required packages
    # end check of the required function from the required packages
    # end package checking

    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "x"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::missing(", base::paste0(mandat.args, collapse = ") | base::missing("), ")")))
    if(base::any(tempo)){ # normally no NA for base::missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args[tempo], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = x, mode = "function", length = 1,  fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    # lib.path already checked above
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/gmillot/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
    # end argument primary checking

    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = get, env = base::sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & base::lapply(base::lapply(tempo.arg, FUN = get, env = base::sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log, na.rm = TRUE) == TRUE){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-base::c(
        "x",
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with base::is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    # warning initiation
    ini.warning.length <- base::options()$warning.length # required to have the max characters of output messages
    base::options(warning.length = 8170)
    # end warning initiation
    # other checkings
    # end other checkings
    # end second round of checking and data preparation

    # main code
    # recovering the basic functions of R
    s <- base::c("package:stats", "package:graphics",  "package:grDevices", "package:utils", "package:datasets", "package:methods", "Autoloads", "package:base") # basic base::search() scope
    if(base::any( ! s %in% base::search())){
        tempo.cat <- base::paste0("INTERNAL ERROR 5 IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE base::search() SCOPE OF R HAS CHANGED.\nTHE PROBLEM IS:\n",
            base::paste(s[ ! s %in% base::search()], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    fun <- base::unlist(base::sapply(X = s, FUN = function(x){base::ls(x, all.names = TRUE)})) # all the basic functions of R in all the scope
    # end recovering the basic functions of R
    # recovering the input function string
    ini <- utils::capture.output(x) # no lines must be removed because it is to catch the lines of the full code
    code_line_nb <- 1:base::length(ini)
    # ini <- base::paste0(ini, collapse = " \\n ") # recovering as single string separated by \\n (and not \n to avoid the eval(\n) when printing the error message)
    ini <- base::gsub(x = ini, pattern = " +", replacement = " ") # removal of multiple spaces
    ini <- base::sub(x = ini, pattern = "^ +", replacement = "") # removal of multiple spaces in the beginning od strings
    # end recovering the input function string

    # removal of empty lines
    empty_line.log <- base::grepl(ini, pattern = "^\\s*$")
    # end removal of empty lines

    # removal of comments
    comment_line.log <- base::grepl(ini, pattern = "^\\s*#") # removal of the lines starting by #
    if(base::length(ini) == 0){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE TESTED FUNCTION ", arg.user.setting$x, " IS EMPTY OR ONLY MADE OF COMMENTS")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    comment.log <- base::grepl(x = ini, pattern = "#")
    if(base::any(comment.log, na.rm = TRUE)){
        comment.line.to.rm <- base::which(comment.log) # elements among ini that have #
        lines <- ini[comment.log]
        for(i2 in 1:base::length(lines)){
            lines.split <- base::strsplit(lines[i2], split = "#")[[1]]
            # detection of the first left # that is not between quotes
            count <- 1
            tempo.line <- lines.split[1]
            while.loop <- TRUE
            while(while.loop == TRUE & count < base::length(lines.split)){
                # if odds number of quotes, it means that # has broken the string in the middle of a quoted part
                double.quote.test <- .has_odd_number_of_quotes(input_string = tempo.line, pattern = '"') # here FALSE means even number of quotes, thus that # is not between quotes, thus has to be removed. TRUE means that # is between quotes, thus has to be kept
                simple.quote.test <- .has_odd_number_of_quotes(input_string = tempo.line, pattern = "'") # idem
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
        ini[comment.line.to.rm] <- lines
    }
    # end removal of comments

    # catch the internal function name created inside the tested function
    internal_fun_names <- base::unlist(base::lapply(X = ini, FUN = function(x){
        output <- base::sub(pattern = "^\\s*([a-zA-Z.]{1}[a-zA-Z0-9._]*)\\s*<-[\\s\\r\\n]*function[\\s\\r\\n]*\\(.*", replacement = "\\1", x = x, perl = TRUE)
        if( ! output == x){
            base::return(output)
        }
    })) # To achieve the extraction of the function names, you need to wrap the part of the pattern that matches the function name in parentheses () to create a capturing group
    # end catch the internal function name created inside the tested function
    # trick to deal with end of lines between the name of the function and "("
    if(base::length(ini) > 1){
        for (i2 in 2:base::length(ini)) {
            # Check if the current string starts with spaces followed by a '('
            if (base::grepl("^\\s*\\(", ini[i2])) {
                # Check if the previous string ends with the specified pattern
                if (base::grepl("[a-zA-Z.]{1}[a-zA-Z0-9._]*\\s*$", ini[i2 - 1])) {
                # Append a '(' to the previous string
                ini[i2 - 1] <- base::paste0(ini[i2 - 1], "(")
                }
            }
        }
    }
    # end trick to deal with end of lines between the name of the function and "("
    # all function names in x
    pattern1 <- "[a-zA-Z.][a-zA-Z0-9._]*\\s*\\(" # pattern to detect a function name, a$fun( is removed in .extract_all()
    # I could have used [\\s\\r\\n]* meaning any space or end of line or carriage return between the name and "(" but finally, another strategy used
    # - `this does not work well, as it does not take dots: "\\b[a-zA-Z\\.\\_]{1}[a-zA-Z0-9\\.\\_]+\\b", because of `\\b`: These are word boundaries. It ensures that the pattern matches only a complete word and not a part of a word.
    # - `[a-zA-Z.]{1}`: This portion of the pattern matches any uppercase letter (`A-Z`), lowercase letter (`a-z`), or a period (`.`) a single time ({1}).
    # - `[a-zA-Z0-9._]*`: This part of the pattern matches any uppercase letter (`A-Z`), lowercase letter (`a-z`), number (`0-9`), period (`.`), or underscore (`_`), repeated one or more times (`+`). This represents the possible characters inside an R function name.
    # - `\\b`: Again, these are word boundaries, making sure the pattern captures the entire word and not just part of it.
    # -  not used: `(?= *\\()`: This is a lookahead assertion. It checks that the preceding pattern is followed by any spaces and a parenthesis (`\\(`), but doesn't include the spaces and parenthesis in the match. This is because, in R code, a function call is usually followed by a parenthesis, but the parenthesis is not part of the function name.
    fun_name <- base::lapply(ini, FUN = function(x){.extract_all(text = x, pattern = pattern1)}) # recover all the function names, followed by "(", present in ini
    fun_name_wo_op <- base::lapply(fun_name, FUN = function(x){x[ ! x %in% base::c("function", "if", "for", "while", "repeat")]}) # removal of special functions
    tempo.log <- base::sapply(fun_name_wo_op, FUN = function(x){base::length(x) == 0}) # detection of string with empty function names
    fun_name_wo_op <- fun_name_wo_op[ ! tempo.log] # removal of empty string
    code_line_nb_wo_op <- code_line_nb[( ! tempo.log) & ( ! comment_line.log) & ( ! empty_line.log)]
    if(base::length(fun_name_wo_op) != base::length(code_line_nb_wo_op)){
        tempo.cat <- base::paste0("INTERNAL ERROR 6 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL\nfun_name_wo_op: ", base::length(fun_name_wo_op), "\ncode_line_nb: ", base::length(code_line_nb_wo_op))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    fun_name_wo_op_uni <- base::unlist(base::unique(fun_name_wo_op))
    # end all function names in x

    # basic function names in x
    in_basic_fun <- base::lapply(fun_name_wo_op, FUN = function(x){x[x %in% fun]}) #  names of all the basic functions used in x
    tempo.log <- base::sapply(in_basic_fun, FUN = function(x){base::length(x) == 0}) # detection of string with empty function names
    in_basic_fun <- in_basic_fun[ ! tempo.log] # removal of empty string
    in_basic_code_line_nb <- code_line_nb_wo_op[ ! tempo.log]
    if(base::length(in_basic_fun) != base::length(in_basic_code_line_nb)){
        tempo.cat <- base::paste0("INTERNAL ERROR 7 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL\nin_basic_fun: ", base::length(in_basic_fun), "\ncode_line_nb: ", base::length(in_basic_code_line_nb))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    in_basic_fun_uni <- base::unlist(base::unique(in_basic_fun)) #  names of unique basic functions used in x
    # end basic function names in x
    # other function names in x
    in_other_fun <- base::lapply(fun_name_wo_op, FUN = function(x){x[ ! x %in% base::c(fun, arg.user.setting$x)]}) #  names of all the other functions used in x, except the one tested (arg.user.setting$x), because can be in error messages
    tempo.log <- base::sapply(in_other_fun, FUN = function(x){base::length(x) == 0}) # detection of string with empty function names
    in_other_fun <- in_other_fun[ ! tempo.log] # removal of empty string
    in_other_code_line_nb <- code_line_nb_wo_op[ ! tempo.log]
    if(base::length(in_other_fun) != base::length(in_other_code_line_nb)){
        tempo.cat <- base::paste0("INTERNAL ERROR 8 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL\nin_other_fun: ", base::length(in_other_fun), "\ncode_line_nb: ", base::length(in_other_code_line_nb))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    in_other_fun_uni <- base::unlist(base::unique(in_other_fun)) # names of unique basic functions used in x, except the one tested (arg.user.setting$x), because can be in error messages
    # end other function names in x
    # analyse of :: before basic functions in x
    if(base::length(in_basic_fun_uni) > 0){
        tempo <- .create_message(
            list.fun = in_basic_fun, 
            list.fun.uni = in_basic_fun_uni, 
            list.line.nb = in_basic_code_line_nb, 
            ini = ini, 
            arg.user.setting = arg.user.setting, 
            function.name = function.name, 
            package.name = package.name, 
            text = "BASIC",
            internal_fun_names = internal_fun_names
        )
        tempo.log <- tempo$colon_not_here
        output.cat <- tempo$output.cat
    }else{
        tempo.log <- FALSE
        output.cat <- NULL
    }
    # end analyse of :: before basic functions in x
    # analyse of :: before other functions in x
    if(base::length(in_other_fun_uni) > 0){
        tempo <- .create_message(
            list.fun = in_other_fun, 
            list.fun.uni = in_other_fun_uni, 
            list.line.nb = in_other_code_line_nb, 
            ini = ini, 
            arg.user.setting = arg.user.setting, 
            function.name = function.name, 
            package.name = package.name, 
            text = "OTHER",
            internal_fun_names = internal_fun_names
        )
        tempo.log.b <- tempo$colon_not_here
        output.cat.b <- tempo$output.cat
    }else{
        tempo.log.b <- FALSE
        output.cat.b <- NULL
    }
    # end analyse of :: before basic functions in x
    if( ( ! base::any(tempo.log)) & ! base::any(tempo.log.b)){
        base::cat("\n\nEVERYTHING SEEMS CLEAN\n\n")
    }else{
        tempo.cat <- base::paste0(base::ifelse(base::is.null(output.cat), base::paste0("INSIDE ", arg.user.setting$x, "(), EVERYTHING SEEMS CLEAN FOR R BASIC FUNCTIONS\n\n"), base::paste0(output.cat, base::ifelse(base::is.null(output.cat.b), "", "\n\n"))), output.cat.b)
        base::cat(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"))
    }
    # end main code
    # output
    # warning output
    base::options(warning.length = ini.warning.length)
    # end warning output
    # end output
}



