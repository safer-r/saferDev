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
#' - Use the result to modify the code of the function like this: <PACKAGE>::<FUNCTION> (OR <PACKAGE>:::<FUNCTION> for function names starting by a dot)
#' 
#' - More precisely, colons_check() verifies that all the strings before an opening bracket "(" are preceeded by "::" 
#' 
#' - ":::" are not checked per se, because incorrect writting, like saferDev::.colons_check_message() returns an error, and because base:::sum() is as ok as base::sum(). In the same manner, more than three colons are not checked because it returns an error.
#' 
#' - Warning: the function cannot check function names written without brackets, like in the FUN argument of some functions, e.g., sapply(1:3, FUN = as.character).
#' 
#' - The perl regex used to detect a function name is: "[a-zA-Z.]{1}[a-zA-Z0-9._]*\\s*\\(".
#' 
#' - Function names preceeded by $ and any space are not considered (pattern "\\$ *[a-zA-Z.]{1}[a-zA-Z0-9._]* *\\(")
#'  
#' - The following R functions using bracket are not considered: "function", "if", "for", "while" and "repeat".
#' 
#' - Most of the time, colons_check() does not check inside comments, but some unexpected writting could dupe colons_check().
#' 
#' - The returned line numbers are indicative, depending on which source is checked. For instance, saferDev::report (compiled) has not the same line numbers as its source file (https://github.com/safer-r/saferDev/blob/main/R/report.R). Notably, compiled functions do not have comments anymore, compared to the same source function sourced into the working environment. In addition, the counting starts at the "<- function" line, i.e., without counting the #' header lines potentially present in source files.
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
#' source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") ; colons_check(test)
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
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") ; x = test ; safer_check = TRUE # Warning: x = saferDev::get_message does not return the same number of code lines
    # arg.user.setting = base::list(x = as.name(x = "test"), safer_check = TRUE)
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
    # end function name
    # main code
    out <- .functions_detect(
        x = x, 
        arg.user.setting = arg.user.setting, 
        function.name = function.name, 
        package.name = package.name
    )

    # basic function names in x
    # selection of basic functions
    tempo.log <- base::lapply(out$fun_names, FUN = function(x){x %in% out$all_basic_funs}) #  are names basic functions used in x?
    in_basic_fun <- mapply(FUN = function(x, y){x[y]}, x = out$fun_names, y = tempo.log)
    in_basic_fun_names_pos <-  mapply(FUN = function(x, y){x[y]}, x = out$fun_names_pos, y = tempo.log)
    # end selection of basic functions
    # removal of string with empty function names
    tempo.log <- base::sapply(in_basic_fun, FUN = function(x){base::length(x) == 0}) # detection of string with empty function names
    in_basic_fun <- in_basic_fun[ ! tempo.log] # removal of empty string
    in_basic_fun_names_pos <- in_basic_fun_names_pos[ ! tempo.log]
    in_basic_code_line_nb <- out$code_line_nb[ ! tempo.log]
    if( ! (base::length(in_basic_fun) == base::length(in_basic_fun_names_pos) & base::length(in_basic_fun) == base::length(in_basic_code_line_nb))){
        tempo.cat <- base::paste0("INTERNAL ERROR 1 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL\nin_basic_fun: ", base::length(in_basic_fun), "\nin_basic_fun_names_pos: ", base::length(in_basic_fun_names_pos), "\nin_basic_code_line_nb: ", base::length(in_basic_code_line_nb))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end removal of string with empty function names
    in_basic_fun_uni <- base::unlist(base::unique(in_basic_fun)) #  names of unique basic functions used in x
    # end basic function names in x
     # other function names in x
    # selection of other functions
    tempo.log <- base::lapply(out$fun_names, FUN = function(x){ ! x %in% base::c(out$all_basic_funs, out$arg.user.setting$x)}) #  names of all the other functions used in x, except the one tested (arg.user.setting$x), because can be in error messages
    in_other_fun <- mapply(FUN = function(x, y){x[y]}, x = out$fun_names, y = tempo.log)
    in_other_fun_names_pos <-  mapply(FUN = function(x, y){x[y]}, x = out$fun_names_pos, y = tempo.log)
    # end selection of other functions
    # removal of string with empty function names
    tempo.log <- base::sapply(in_other_fun, FUN = function(x){base::length(x) == 0}) # detection of string with empty function names
    in_other_fun <- in_other_fun[ ! tempo.log] # removal of empty string
    in_other_fun_names_pos <- in_other_fun_names_pos[ ! tempo.log]
    in_other_code_line_nb <- out$code_line_nb[ ! tempo.log]
    if( ! (base::length(in_other_fun) == base::length(in_other_fun_names_pos) & base::length(in_other_fun) == base::length(in_other_code_line_nb))){
        tempo.cat <- base::paste0("INTERNAL ERROR 2 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL\nin_other_fun: ", base::length(in_other_fun), "\nin_other_fun_names_pos: ", base::length(in_other_fun_names_pos), "\nin_other_code_line_nb: ", base::length(in_other_code_line_nb))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end removal of string with empty function names
    in_other_fun_uni <- base::unlist(base::unique(in_other_fun)) #  names of unique other functions used in x
    # end other function names in x
    # analyse of :: before basic functions in x
    if(base::length(in_basic_fun_uni) > 0){
        tempo <- .colons_check_message(
            list.fun = in_basic_fun, 
            fun.uni = in_basic_fun_uni, 
            list.fun.pos = in_basic_fun_names_pos, 
            line.nb = in_basic_code_line_nb, 
            ini = out$code, 
            arg.user.setting = out$arg.user.setting, 
            function.name = function.name, 
            package.name = package.name, 
            text = "BASIC",
            internal_fun_names = out$internal_fun_names
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
        tempo <- .colons_check_message(
            list.fun = in_other_fun, 
            fun.uni = in_other_fun_uni, 
            list.fun.pos = in_other_fun_names_pos, 
            line.nb = in_other_code_line_nb, 
            ini = out$code, 
            arg.user.setting = out$arg.user.setting, 
            function.name = function.name, 
            package.name = package.name, 
            text = "OTHER",
            internal_fun_names = out$internal_fun_names
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
        tempo.cat <- base::paste0(base::ifelse(base::is.null(output.cat), base::paste0("INSIDE ", out$arg.user.setting$x, "(), EVERYTHING SEEMS CLEAN FOR R BASIC FUNCTIONS\n\n"), base::paste0(output.cat, base::ifelse(base::is.null(output.cat.b), "", "\n\n"))), output.cat.b)
        base::cat(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"))
    }
    # end main code
    # output
    # warning output
    # end warning output
    # end output
}



