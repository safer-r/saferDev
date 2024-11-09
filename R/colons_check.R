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
    # x = mean ; safer_check = TRUE
    # x = .expand_R_libs_env_var ; safer_check = TRUE
    # library(saferGraph) ; x = close2 ; safer_check = TRUE 
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\get_message.R") ; x = get_message ; safer_check = TRUE # Warning: x = saferDev::get_message does not return the same number of code lines
    # library(saferDev) ; x = get_message ; safer_check = TRUE # Warning: does not return the same number of code lines than the previsou example
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\colons_check.R") ; x = colons_check ; safer_check = TRUE # Warning: x = saferDev::get_message does not return the same number of code lines
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") ; x = test ; safer_check = TRUE # Warning: x = saferDev::get_message does not return the same number of code lines
    # function_name <- "colons_check" ; arg_user_setting = base::list(x = as.name(x = "test"), safer_check = TRUE)
    #### package name
    package_name <- "saferDev" # write NULL if the function developed is not in a package
    #### end package name

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

    #### critical operator checking
    if( ! (base::all(safer_check %in% base::c(TRUE, FALSE), na.rm = FALSE) & base::length(x = safer_check) == 1 & base::all(base::is.logical(x = safer_check), na.rm = TRUE))){
        tempo_cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\nsafer_check ARGUMENT MUST BE EITHER TRUE OR FALSE. HER IT IS:\n", base::paste0(safer_check, collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    if(safer_check == TRUE){
        saferDev:::.base_op_check(
            external_function_name = function_name, 
            external_package_name = package_name
        )
    }
    #### end critical operator checking

    #### package checking

    ######## check of lib_path
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        saferDev:::.pack_and_function_check(
            fun = base::c(
                "saferDev::arg_check" # write each function preceeded by their package name
            ),
            lib_path = NULL, # write NULL if your function does not have any lib_path argument
            external_function_name = function_name,
            external_package_name = package_name
        )
    }
    ######## end check of the required functions from the required packages

    #### end package checking

    #### argument primary checking

    ######## arg with no default values
    mandat_args <- base::c(
        "x"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = FALSE)){
        tempo_cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\nFOLLOWING ARGUMENT", base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat_args[tempo], collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end arg with no default values

    ######## argument checking with arg_check()
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))
    # add as many lines as below, for each of your arguments of your function in development
    tempo <- saferDev::arg_check(data = x, class = "function", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, fun_name = function_name, pack_name = package_name, safer_check = FALSE) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    # safer_check already checked above
    if( ! base::is.null(x = argum_check)){
        if(base::any(argum_check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text_check[argum_check], sep = " ", collapse = "\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
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
            tempo_cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\n", base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(arg_names[tempo_log], collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end management of NA arguments

    ######## management of NULL arguments
    tempo_arg <-base::c(
        "x", 
        "safer_check"
    )
    tempo_log <- base::sapply( X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
    if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA with base::is.null()
        tempo_cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\n", base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),"\nCANNOT BE NULL", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end management of NULL arguments

    ######## management of "" in arguments of mode character
    ######## end management of "" in arguments of mode character

    #### end argument primary checking

    #### second round of checking and data preparation

    ######## reserved words
    ######## end reserved words

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ######## end warning initiation

    ######## other checkings
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    out <- saferDev:::.functions_detect(
        x = x, 
        arg_user_setting = arg_user_setting, 
        function_name = function_name, 
        package_name = package_name
    )

    # basic function names in x
    # selection of basic functions
    tempo.log <- base::lapply(X = out$fun_names, FUN = function(x){x %in% out$all_basic_funs}) #  are names basic functions used in x?
    in_basic_fun <- base::mapply(FUN = function(x, y){x[y]}, x = out$fun_names, y = tempo.log, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
    in_basic_fun_names_pos <-  base::mapply(FUN = function(x, y){x[y]}, x = out$fun_names_pos, y = tempo.log, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
    # end selection of basic functions
    # removal of string with empty function names
    tempo.log <- base::sapply(X = in_basic_fun, FUN = function(x){base::length(x = x) == 0}) # detection of string with empty function names
    in_basic_fun <- in_basic_fun[ ! tempo.log] # removal of empty string
    in_basic_fun_names_pos <- in_basic_fun_names_pos[ ! tempo.log]
    in_basic_code_line_nb <- out$code_line_nb[ ! tempo.log]
    if( ! (base::length(x = in_basic_fun) == base::length(x = in_basic_fun_names_pos) & base::length(x = in_basic_fun) == base::length(x = in_basic_code_line_nb))){
        tempo.cat <- base::paste0("INTERNAL ERROR 1 IN ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL\nin_basic_fun: ", base::length(x = in_basic_fun), "\nin_basic_fun_names_pos: ", base::length(x = in_basic_fun_names_pos), "\nin_basic_code_line_nb: ", base::length(x = in_basic_code_line_nb), collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    # end removal of string with empty function names
    in_basic_fun_uni <- base::unlist(base::unique(in_basic_fun)) #  names of unique basic functions used in x
    # end basic function names in x
     # other function names in x
    # selection of other functions
    tempo.log <- base::lapply(out$fun_names, FUN = function(x){ ! x %in% base::c(out$all_basic_funs, out$arg_user_setting$x)}) #  names of all the other functions used in x, except the one tested (arg_user_setting$x), because can be in error messages
    in_other_fun <- base::mapply(FUN = function(x, y){x[y]}, x = out$fun_names, y = tempo.log)
    in_other_fun_names_pos <-  base::mapply(FUN = function(x, y){x[y]}, x = out$fun_names_pos, y = tempo.log)
    # end selection of other functions
    # removal of string with empty function names
    tempo.log <- base::sapply(in_other_fun, FUN = function(x){base::length(x = x) == 0}) # detection of string with empty function names
    in_other_fun <- in_other_fun[ ! tempo.log] # removal of empty string
    in_other_fun_names_pos <- in_other_fun_names_pos[ ! tempo.log]
    in_other_code_line_nb <- out$code_line_nb[ ! tempo.log]
    if( ! (base::length(x = in_other_fun) == base::length(x = in_other_fun_names_pos) & base::length(x = in_other_fun) == base::length(x = in_other_code_line_nb))){
        tempo.cat <- base::paste0("INTERNAL ERROR 2 IN ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL\nin_other_fun: ", base::length(x = in_other_fun), "\nin_other_fun_names_pos: ", base::length(x = in_other_fun_names_pos), "\nin_other_code_line_nb: ", base::length(x = in_other_code_line_nb))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    # end removal of string with empty function names
    in_other_fun_uni <- base::unlist(base::unique(in_other_fun)) #  names of unique other functions used in x
    # end other function names in x
    # analyse of :: before basic functions in x
    if(base::length(x = in_basic_fun_uni) > 0){
        tempo <- saferDev:::.colons_check_message(
            list.fun = in_basic_fun, 
            fun.uni = in_basic_fun_uni, 
            list.fun.pos = in_basic_fun_names_pos, 
            line.nb = in_basic_code_line_nb, 
            ini = out$code, 
            arg_user_setting = out$arg_user_setting, 
            function_name = function_name, 
            package_name = package_name, 
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
    if(base::length(x = in_other_fun_uni) > 0){
        tempo <- saferDev:::.colons_check_message(
            list.fun = in_other_fun, 
            fun.uni = in_other_fun_uni, 
            list.fun.pos = in_other_fun_names_pos, 
            line.nb = in_other_code_line_nb, 
            ini = out$code, 
            arg_user_setting = out$arg_user_setting, 
            function_name = function_name, 
            package_name = package_name, 
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
    if(( ! base::any(tempo.log, na.rm = TRUE)) & ! base::any(tempo.log.b, na.rm = TRUE)){
        tempo.cat <- base::paste0("AFTER RUNNING ", function_name, " OF THE ", package_name, " PACKAGE:\nINSIDE ", base::as.character(x = out$arg_user_setting$x), "(), EVERYTHING SEEMS CLEAN")
    }else{
        tempo.cat <- base::paste0(base::ifelse(base::is.null(x = output.cat), base::paste0("AFTER RUNNING ", function_name, " OF THE ", package_name, " PACKAGE\n\nINSIDE ", out$arg_user_setting$x, "(), EVERYTHING SEEMS CLEAN FOR R BASIC FUNCTIONS\n\n", collapse = NULL, recycle0 = FALSE), base::paste0(output.cat, base::ifelse(base::is.null(x = output.cat.b), "", "\n\n"), collapse = NULL, recycle0 = FALSE)), output.cat.b, collapse = NULL, recycle0 = FALSE)
    }
    base::cat(base::paste0("\n\n", tempo.cat, "\n\n", collapse = NULL, recycle0 = FALSE), file = NULL, sep = NULL, fill = FALSE, labels = NULL, append = FALSE)
    #### end main code

    #### output
    #### end output

    #### warning output
    #### end warning output
}


