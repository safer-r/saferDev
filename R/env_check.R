#' @title env_check
#' @description
#' Verify that object names in the environment defined by the pos parameter are identical or not to object names in the above environments (following R Scope). This can be used to verify that names used for objects inside a function or in the working environment do not override names of objects already present in the above R environments, following the R scope.
#' @param pos Single non nul positive integer indicating the position of the environment checked (argument n of the parent.frame() function). Value 1 means one step above the env_check() local environment (by default). This means that when env_check(pos = 1) is used inside a function A, it checks if the name of any object in the local environment of this function A is also present in above environments, following R Scope, starting by the just above environment. When env_check(pos = 1) is used in the working (Global) environment (named .GlobalEnv), it checks the object names of this .GlobalEnv environment, in the above environments. See the examples below.
#' @param name Single character string indicating a string that will be added in the output string, for instance the name of a function inside which env_check() is used.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful to overcome R execution using system with non admin rights for R package installation in the default directories. Ignored if NULL (default): only the pathways specified by .libPaths() are used for package calling. Specify the right path if the function returns a package path error.
#' @param safer_check Single logical value. Perform some "safer" checks? If TRUE, checkings are performed before main code running (see https://github.com/safer-r): 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = "INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>".
#' @returns
#' A character string indicating the object names of the tested environment that match object names in the above environments, following the R scope, or NULL if no match.
#' @seealso \code{\link{exists}}.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' # Examples in the working environment
#' # creation of the object mean with value 1 in the .GlobalEnv environment, 
#' # knowing that the mean() function also exists in the environment base, above .GlobalEnv:
#' mean <- 1 
#' # creation of the object t.test with value 1 in the .GlobalEnv environment, 
#' # knowing that the t.test() function also exists in the environment stats, above .GlobalEnv:
#' t.test <- 1 
#' search() # current R scope (order of the successive R environments).
#' utils::find("mean") # where the objects with the name "mean" are present.
#' utils::find("t.test") # where the objects with the name "mean" are present.
#' a <- env_check(pos = 1) # test if any object name of the global environment are above environments 
#' a # output string.
#' cat(a)
#' # test if any object of the stats environment (one step above .GlobalEnv) 
#' # are in upper environments of stats. Returns NULL since no object names of stats are in upper environments:
#' env_check(pos = 2) 
#' rm(mean) ; rm (t.test)
#' 
#' # Examples inside a function
#' # env_check() checks if the object names inside the fun1 function 
#' # exist in the .GlobalEnv environment and above:
#' fun1 <- function(){t.test <- 0 ; mean <- 5 ; env_check(pos = 1)} 
#' fun1()
#' cat(fun1())
#' # env_check() checks if the object names inside the environment one step above fun2(), 
#' # here .GlobalEnv, exist in the upper environments of .GlobalEnv:
#' fun2 <- function(){sum <- 0 ; env_check(pos = 2)} 
#' fun2()
#' # Warning: cat(fun2()) does not return NULL, because the environement tested is not anymore .GlobalEnv but inside cat().
#' # With the name of the function fun3 indicated in the message:
#' fun3 <- function(){t.test <- 0 ; mean <- 5 ; env_check(pos = 1, name = "fun3")}
#' fun3()
#' # Alternative way:
#' fun4 <- function(){t.test <- 0 ; mean <- 5 ; env_check(pos = 1, name = as.character(sys.calls()[[length(sys.calls())]]))}
#' fun4()
#' # sys.calls() gives the name of the imbricated functions, 
#' # sys.calls()[[length(sys.calls())]] the name of the function one step above.
#' fun5 <- function(){fun6 <- function(){print(sys.calls())} ; fun6()}
#' fun5()
#' # A way to have the name of the tested environment according to test.pos value:
#' fun7 <- function(){
#'     min <- "a"
#'     fun8 <- function(){
#'         test.pos <- 1 # value 1 tests the fun8 env, 2 tests the fun7 env.
#'         range <- "a"
#'         env_check(pos = test.pos, name = if(length(sys.calls()) >= test.pos){
#'             as.character(sys.calls()[[length(sys.calls()) + 1 - test.pos]])
#'         }else{
#'             search()[(1:length(search()))[test.pos - length(sys.calls())]]
#'         }) 
#'     }
#'     fun8()
#' }
#' fun7()
#' @export
env_check <- function(
    pos = 1, 
    name = NULL, 
    lib_path = NULL, 
    safer_check = TRUE, 
    error_text = "" 
){
    # DEBUGGING
    # pos = 1 ; name = "mean"; safer_check = TRUE # for function debugging

    #### package name
    package_name <- "saferDev" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message
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
    # basic error text start
    error_text_start <- base::paste0(
        "ERROR IN ", 
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)), 
        function_name, 
        collapse = NULL, 
        recycle0 = FALSE
    )
    # end basic error text start
    # check of the error_text argument
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
    # end check of the error_text argument
    # basic error text start updated
    error_text_start <- base::paste0(
        error_text_start, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    # end basic error text start updated
    # internal error text
    intern_error_text_start <- base::paste0(
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)), 
        function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    intern_error_text_end <- base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, ".", collapse = NULL, recycle0 = FALSE))
    # end internal error text
    #### end error_text initiation

    #### environment checking

    ######## check of lib_path
    # must be before any :: or ::: non basic package calling
    if( ! base::is.null(x = lib_path)){
        if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = FALSE)){ # no na.rm = TRUE with typeof
            tempo_cat <- base::paste0(
                error_text_start, 
                "DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\nHERE IT IS:\n", 
                base::paste0(lib_path, collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }else if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA
            tempo_cat <- base::paste0(
                error_text_start, 
                "DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXISTS:\n", 
                base::paste0(lib_path, collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }else{
            base:::.libPaths(new = base::sub(x = lib_path, pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base:::.libPaths(new = ) add path to default path. BEWARE: base:::.libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
            lib_path <- base:::.libPaths(new = , include.site = TRUE)
        }
    }else{
        lib_path <- base:::.libPaths(new = , include.site = TRUE) # base:::.libPaths(new = lib_path) # or base:::.libPaths(new = base::c(base:::.libPaths(), lib_path))
    }
    ######## end check of lib_path

    ######## safer_check argument checking
    if( ! (base::all(safer_check %in% base::c(TRUE, FALSE), na.rm = FALSE) & base::length(x = safer_check) == 1 & base::all(base::is.logical(x = safer_check), na.rm = TRUE))){
        tempo_cat <- base::paste0(
            error_text_start, 
            "safer_check ARGUMENT MUST BE EITHER TRUE OR FALSE.\nHER IT IS:\n", 
            base::paste0(safer_check, collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end safer_check argument checking

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check(
            fun = base::c(
                "saferDev:::.base_op_check", 
                "saferDev::arg_check"
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
            internal_error_report_link = internal_error_report_link
        )
    }
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
            error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        )
    }
    ######## end critical operator checking

    #### end environment checking

    #### argument primary checking

    ###### arg with no default values
    ###### end arg with no default values

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
    tempo_arg <- base::c(
        "pos",
        # "name", # inactivated because can be NULL
        # "lib_path", # inactivated because can be NULL
        "safer_check",
        "error_text"
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

    ###### argument checking with arg_check()
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))
    # add as many lines as below, for each of your arguments of your function in development
    tempo <- arg_check(data = pos, class = "vector", typeof = "integer", mode = NULL, length = 1,double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = lib_path, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = name)){
        tempo <- arg_check(data = name, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = lib_path, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    # lib_path already checked above
    # safer_check already checked above
    # error_text already checked above
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
    tempo_arg <- base::c(
        "name", 
        "lib_path"
        # "error_text" # inactivated because can be ""
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

    # match.list <- base::vector("list", length = (base::length(base::sys.calls()) - 1 + base::length(base::search()) + base::ifelse(base::length(base::sys.calls()) == 1L, -1, 0))) # match.list is a list of all the environment tested (local of functions and R envs), base::length(base::sys.calls()) - 1 to remove the level of the ebase::nv_check() function, base::sys.calls() giving all the names of the imbricated functions, including env_check, base::ifelse(base::length(sys.calls()) == 1L, -1, 0) to remove Global env if this one is tested
    tempo.name <- base::rev(base::as.character(base::unlist(base::sys.calls()))) # get names of frames (i.e., enclosed env)
    tempo.frame <- base::rev(base::sys.frames())  # get frames (i.e., enclosed env)

    ######## dealing with source()
    # source() used in the Global env creates three frames above the Global env, which should be removed because not very interesting for variable duplications. Add a <<-(sys.frames()) in this code and source anova_contrasts code to see this. With ls(a[[4]]), we can see the content of each env, which are probably elements of source()
    if(base::any(base::sapply(tempo.frame, FUN = base::environmentName) %in% "R_GlobalEnv")){
        global.pos <- base::which(base::sapply(tempo.frame, FUN = base::environmentName) %in% "R_GlobalEnv")
        # remove the global env (because already in base::search(), and all the oabove env
        tempo.name <- tempo.name[-base::c(global.pos:base::length(tempo.frame))]
        tempo.frame <- tempo.frame[-base::c(global.pos:base::length(tempo.frame))]
    }
    ######## end dealing with source()

    # might have a problem if(length(tempo.name) == 0L){
    match.list <- base::vector("list", length = base::length(tempo.name) + base::length(base::search())) # match.list is a list of all the environment tested (local of functions and R envs), base::length(base::sys.calls()) - 1 to remove the level of the env_check() function, base::sys.calls() giving all the names of the imbricated functions, including env_check, base::ifelse(base::length(base::sys.calls()) == 1L, -1, 0) to remove Global env if this one is tested
    ls.names <- base::c(tempo.name, base::search()) # names of the functions + names of the base::search() environments
    ls.input <- base::c(tempo.frame, base::as.list(base::search())) # environements of the functions + names of the base::search() environments
    base::names(match.list) <- ls.names # 
    match.list <- match.list[-base::c(1:(pos + 1))] # because we check only above pos
    ls.tested <- ls.input[[pos + 1]]
    ls.input <- ls.input[-base::c(1:(pos + 1))]
    for(i1 in 1:base::length(match.list)){
        if(base::any(base::ls(name = ls.input[[i1]], all.names = TRUE) %in% base::ls(name = ls.tested, all.names = TRUE))){
            match.list[i1] <- base::list(base::ls(name = ls.input[[i1]], all.names = TRUE)[base::ls(name = ls.input[[i1]], all.names = TRUE) %in% base::ls(name = ls.tested, all.names = TRUE)])
        }
    }

    ######## output
    if( ! base::all(base::sapply(match.list, FUN = is.null), na.rm = TRUE)){
        output <- base::paste0("SOME VARIABLES ", base::ifelse(base::is.null(name), "OF THE CHECKED ENVIRONMENT", base::paste0("OF ", name)), " ARE ALSO PRESENT IN :\n", base::paste0(base::names(match.list[ ! base::sapply(match.list, FUN = base::is.null)]), ": ", base::sapply(match.list[ ! base::sapply(match.list, FUN = base::is.null)], FUN = base::paste0, collapse = " "), collapse = "\n"), "\n")
    }else{
        output <- NULL
    }
    base::return(output)
    ######## end output
    #### end main code
}

