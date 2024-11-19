#' @title is_python_package_here
#' @description
#' Check if the specified python packages are installed locally.
#' @param req_package Character vector of package names to import.
#' @param python_exec_path Single optional character vector specifying the absolute pathways of the executable python file to use (associated to the packages to use). If NULL, the reticulate::import_from_path() function used in is_python_package_here() seeks for an available version of python.exe, and then uses python_config(python_version, required_module, python_versions). But might not be the correct one for the python_lib_path parameter specified. Thus, it is recommanded to do not leave NULL, notably when using computing clusters.
#' @param python_lib_path Optional character vector specifying the absolute pathways of the directories containing some of the listed packages in the req_package argument, if not in the default directories.
#' @param lib_path Absolute path of the reticulate packages, if not in the default folders.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns An error message if at least one of the checked packages is missing in python_lib_path, nothing otherwise.
#' @details 
#' WARNINGS
#' 
#' for python 3.7. Previous versions return an error "Error in sys$stdout$flush() : attempt to apply non-function"
#' @importFrom reticulate py_run_string
#' @importFrom reticulate use_python
#' @importFrom reticulate import_from_path
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' # example of error message
#' 
#' # is_python_package_here(req_package = "nopackage")
#' 
#' 
#' # commented because this example returns an error if the python package is not installed on the computer
#' # (require the installation of the python serpentine package 
#' # from https://github.com/koszullab/serpentine
#' 
#' # is_python_package_here(req_package = "serpentine")
#' 
#' 
#' # another example of error message
#' 
#' # is_python_package_here(req_package = "serpentine", python_lib_path = "blablabla")
#' @export
is_python_package_here <- function(
        req_package, 
        python_exec_path = NULL, 
        python_lib_path = NULL, 
        lib_path = NULL,
        safer_check = TRUE
){
    # DEBUGGING
    # req_package = "serpentine" ; python_exec_path = "C:/ProgramData/Anaconda3/python.exe" ; python_lib_path = "c:/programdata/anaconda3/lib/site-packages/" ; lib_path = NULL ; safer_check = TRUE
    # req_package = "bad" ; python_lib_path = NULL ; lib_path = NULL ; safer_check = TRUE
    
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
    if( ! base::is.null(x = lib_path)){
        if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = FALSE)){ # no na.rm = TRUE with typeof
            tempo_cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\nDIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", base::paste(lib_path, sep = " ", collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }else if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA
            tempo_cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\nDIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib_path, sep = " ", collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }else{
            base:::.libPaths(new = base::sub(x = lib_path, pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base:::.libPaths(new = ) add path to default path. BEWARE: base:::.libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
            lib_path <- base:::.libPaths(new = , include.site = TRUE)
        }
    }else{
        lib_path <- base:::.libPaths(new = , include.site = TRUE) # base:::.libPaths(new = lib_path) # or base:::.libPaths(new = base::c(base:::.libPaths(), lib_path))
    }
    ######## end check of lib_path

    # check of the required function from the required packages
    if(safer_check == TRUE){
        saferDev:::.pack_and_function_check(
        fun = base::c(
            "reticulate::py_run_string", 
            "reticulate::use_python",
            "reticulate::import_from_path",
            "saferDev::arg_check"
        ),
        lib_path = lib_path,# write NULL if your function does not have any lib_path argument
        external_function_name = function_name,
        external_package_name = package_name
        )
    }
    ######## end check of the required functions from the required packages

    #### end package checking

    #### argument primary checking

    ######## arg with no default values
    mandat_args <- base::c(
        "req_package"
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
    tempo <- saferDev::arg_check(data = req_package, class = "character", typeof = NULL, mode = "character", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, fun_name = function_name, pack_name = package_name, safer_check = FALSE) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = python_exec_path)){
        tempo <- saferDev::arg_check(data = python_exec_path, class = "character", typeof = NULL, mode = "character", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, fun_name = function_name, pack_name = package_name, safer_check = FALSE) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        if(tempo$problem == FALSE){
            if( ! base::all(base::file.exists(python_exec_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and python_exec_path == NA
                tempo_cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nFILE PATH INDICATED IN THE python_exec_path ARGUMENT DOES NOT EXISTS:\n", base::paste(python_exec_path, collapse = "\n"))
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }
        }
    }
    if( ! base::is.null(x = python_lib_path)){
        tempo <- saferDev::arg_check(data = python_lib_path, class = "vector", typeof = NULL, mode = "character", length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = FALSE, print = FALSE, data_name = NULL, fun_name = function_name, pack_name = package_name, safer_check = FALSE) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        if(tempo$problem == FALSE){
            if( ! base::all(base::dir.exists(python_lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and python_lib_path == NA
                tempo_cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE python_lib_path ARGUMENT DOES NOT EXISTS:\n", base::paste(python_lib_path, collapse = "\n"))
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }
        }
    }
    # lib_path already checked above
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
        tempo_log <- base::suppressWarnings(expr = base::sapply(X = base::lapply(X = arg_user_setting, FUN = function(x){base::is.na(x = x)}), FUN = function(x){base::any(x = x, na.rm = FALSE)}, simplify = TRUE, USE.NAMES = TRUE), classes = "warning") & base::lapply(X = arg_user_setting, FUN = function(x){base::length(x = x)}) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo_log, na.rm = FALSE)){ # normally no NA because base::is.na() used here
            tempo_cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\n", base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(arg_names[tempo_log], collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end management of NA arguments

    ######## management of NULL arguments
     tempo_arg <-base::c(
        "req_package",
        # "python_exec_path", # inactivated because can be null
        # "python_lib_path", # inactivated because can be null
        # "lib_path", # inactivated because can be null
        "safer_check"
    )
    tempo_log <- base::sapply( X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
    if(base::any(tempo_log, na.rm = FALSE)){ # normally no NA with base::is.null()
        tempo_cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\n", base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),"\nCANNOT BE NULL", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end management of NULL arguments

    # end argument primary checking
    
    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)

    ######## management of "" in arguments of mode character
    tempo_arg <-base::c(
        "req_package",
        "python_exec_path",
        "python_lib_path"
    )
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())  # for character argument that can also be NULL, if NULL -> considered as character
    if(base::any(tempo_log, na.rm = TRUE)){
        tempo_cat <- base::paste0("INTERNAL ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\n", base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS ARE", no = "THIS ARGUMENT IS"), " NOT MODE \"character\":\n", base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = FALSE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\n", base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),"\nCANNOT CONTAIN \"\"", collapse = NULL, recycle0 = FALSE)
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

    ######## other checkings
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    if(base::is.null(python_exec_path)){
        python_exec_path <- reticulate::py_run_string(
            "import sys;
            path_lib = sys.path") # python string
        python_exec_path <- python_exec_path$path_lib
    }
    if(base::is.null(python_lib_path)){
        python_lib_path <- reticulate::py_run_string(
            "import sys ;
            path_lib = sys.path") # python string
        python_lib_path <- python_lib_path$path_lib
    }
    reticulate::use_python(base::Sys.which(python_exec_path), required = TRUE) # required to avoid the use of erratic python exec by reticulate::import_from_path()
    for(i1 in 1:base::length(req_package)){
        tempo.try <- base::vector("list", length = base::length(python_lib_path))
        for(i2 in 1:base::length(python_lib_path)){
            tempo.try[[i2]] <- base::suppressWarnings(base::try(reticulate::import_from_path(req_package[i1], path = python_lib_path[i2]), silent = TRUE))
            tempo.try[[i2]] <- base::suppressWarnings(base::try(reticulate::import_from_path(req_package[i1], path = python_lib_path[i2]), silent = TRUE)) # done twice to avoid the error message  about flushing present the first time but not the second time. see https://stackoverflow.com/questions/57357001/reticulate-1-13-error-in-sysstdoutflush-attempt-to-apply-non-function
        }
        if(base::all(base::sapply(tempo.try, FUN = grepl, pattern = "[Ee]rror"), na.rm = TRUE)){
            base::print(tempo.try)
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nPACKAGE ", req_package[i1], " MUST BE INSTALLED IN THE MENTIONNED DIRECTORY:\n", base::paste(python_lib_path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        } # else{
        # suppressMessages(suppressWarnings(suppressPackageStartupMessages(assign(req_package[i1], reticulate::import(req_package[i1]))))) # not required because try() already evaluates
        # }
    }
    #### end main code

    #### output
    #### end output

    #### warning output
    #### end warning output
}
