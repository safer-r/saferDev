#' @title all_args_here
#' @description
#' Verify that all the functions used inside a function are written with all their arguments. For instance: base::paste0(letters[1:2], collapse = NULL, recycle0 = FALSE) and not paste0(letters[1:2]).
#' @param x a function name, written without quotes and brackets.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns 
#' A table-like message indicating the missing arguments or a message saying that everything seems fine.
#' Table-like: column 1, the line number in the function code (starting at the "<- function" line, i.e., without counting the #' header lines); column 2,  the function name; column 3, the code preceeding the function name; column 4, the missing arguments with default values
#' @details
#' - More precisely, all_args_here() verifies that all the strings before an opening bracket "(" are written with all their arguments. Thus, it cannot check function names written without brackets, like in the FUN argument of some functions, e.g., sapply(1:3, FUN = as.character).
#' 
#' - The perl regex used to detect a function name is: "[a-zA-Z.][a-zA-Z0-9._]*\\s*\\(".
#' 
#' - Function names preceeded by $ and any space are not considered (pattern "\\$ *[a-zA-Z.][a-zA-Z0-9._]* *\\(")
#'  
#' - The following R functions using bracket are not considered: "function", "if", "for", "while" and "repeat".
#' 
#' - Most of the time, all_args_here() does not check inside comments, but some unexpected writting could dupe all_args_here().
#' 
#' - The returned line numbers is indicative, depending on which source is checked. For instance, saferDev::report (compiled) has not the same line numbers as its source file (https://github.com/safer-r/saferDev/blob/main/R/report.R). Notably, compiled functions do not have comments anymore, compared to the same source function sourced into the working environment. In addition, the counting starts at the "<- function" line, i.e., without counting the #' header lines potentially present in source files.
#' 
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @examples
#' all_args_here(mean)
#' all_args_here(all_args_here)
#' source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") ; all_args_here(test)
#' @export
all_args_here <- function(
    x, 
    safer_check = TRUE
){
    # DEBUGGING
    # x = .expand_R_libs_env_var ; safer_check = TRUE
    # library(saferGraph) ; x = close2 ; safer_check = TRUE 
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\get_message.R") ; x = get_message ; safer_check = TRUE # Warning: x = saferDev::get_message does not return the same number of code lines
    # library(saferDev) ; x = get_message ; safer_check = TRUE # Warning: does not return the same number of code lines than the previsou example
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\all_args_here.R") ; x = all_args_here ; safer_check = TRUE # Warning: x = saferDev::get_message does not return the same number of code lines
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
    # arg.user.setting <<- arg.user.setting
    # end function name
    # main code
    out <- saferDev:::.functions_detect(
        x = x, 
        safer_check = safer_check,
        arg.user.setting = arg.user.setting, 
        function.name = function.name, 
        package.name = package.name
    )
    list.line.nb <- out$code_line_nb_wo_op # vector of line numbers in ini
    fun <-  out$fun_name_wo_op # list of function names for each line of ini
    ini <- out$ini # vector of strings of the tested function code
    if(length(list.line.nb) > 0){
        if(base::length(fun) != base::length(list.line.nb)){
            tempo.cat <- base::paste0("INTERNAL ERROR 2 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL\nfun: ", base::length(fun), "\nlist.line.nb: ", base::length(list.line.nb))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        col1 <- base::as.vector(base::unlist(base::mapply(FUN = function(x, y){base::rep(y, base::length(x[[]]))}, x = fun, y = list.line.nb)))
        col2 <- base::as.vector(base::unlist(fun))
        col3 <- base::as.vector(base::unlist(base::mapply(FUN = function(x, y){y[x]}, x = colon_not_here, y = res)))
    }



    pattern2 <- base::paste(base::paste0("(?<![A-Za-z0-9._])", list.fun.uni, "\\s*\\("), collapse = "|") # to split string according to function name as splitter. Pattern (?<![A-Za-z0-9._]) means "must not be preceeded by any alphanum or .or _
    pattern3 <- base::paste(base::paste0("(?<![A-Za-z0-9._])", list.fun.uni, "\\s*\\($"), collapse = "|") # same as pattern2 but used to know if the seeked function is at the end of the string
    basic_ini <- ini[list.line.nb]
    res <- base::strsplit(x = basic_ini, split = pattern2, perl = TRUE) # in res, all the strings should finish by ::
    tempo.log <- ! base::grepl(x = basic_ini, pattern = pattern3, perl = TRUE) # strings of basic_ini that does not finish by the function name
    # in each compartment of res, the last split section is removed because nothing to test at the end (end of code)
    if(base::sum(tempo.log, na.rm = TRUE) > 0){
        res[tempo.log] <- base::lapply(X = res[tempo.log], FUN = function(x){x[-base::length(x)]})
    }
    # end in each compartment of res, the last split section is removed because nothing to test at the end (end of code)
    res2 <- base::lapply(X = res, FUN = function(x){base::substr(x, base::nchar(x)-1, base::nchar(x))}) # base::nchar(x)-1 takes only :: if the strings ends by :::
    base::names(res2) <- NULL
    if( ! base::all(base::sapply(X = res2, FUN = function(x){base::length(x)}) == base::sapply(X = res, FUN = function(x){base::length(x)}))){
        tempo.cat <- base::paste0("INTERNAL ERROR 2 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL\nres2: ", base::paste(base::sapply(X = res2, FUN = function(x){base::length(x)}), collapse = " "), "\nres: ", base::paste(base::sapply(X = res, FUN = function(x){base::length(x)}), collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }


    a <- base::formals(fun = base::sys.function(base::sys.parent(n = 2)))




    # end main code
    # output
    # warning output
    base::options(warning.length = out$ini.warning.length)
    # end warning output
    # end output
}



