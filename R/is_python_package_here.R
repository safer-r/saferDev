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
    
    # package name
    package_name <- "saferDev"
    # end package name
    
    # function name
    function_name <- base::paste0(base::as.list(base::match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function_name[1] == "::()" | function_name[1] == ":::()"){
        function_name <- function_name[3]
    }
    arg.names <- base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) # names of all the arguments
    arg_user_setting <- base::as.list(base::match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # critical operator checking
    if(safer_check == TRUE){
        saferDev:::.base_op_check(
            external_function_name = function_name,
            external_package_name = package_name
        )
    }
    # end critical operator checking
    # package checking
    # check of lib_path
    if( ! base::is.null(lib_path)){
        if( ! base::all(base::typeof(lib_path) == "character")){ # no na.rm = TRUE with typeof
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", base::paste(lib_path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }else if( ! base::all(base::dir.exists(lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib_path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }else{
            base::.libPaths(new = base::sub(x = lib_path, pattern = "/$|\\\\$", replacement = "")) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
            lib_path <- base::.libPaths()
        }
    }else{
        lib_path <- base::.libPaths() # base::.libPaths(new = lib_path) # or base::.libPaths(new = base::c(base::.libPaths(), lib_path))
    }
    # end check of lib_path
    # check of the required function from the required packages
    if(safer_check == TRUE){
        saferDev:::.pack_and_function_check(
        fun = base::c(
            "reticulate::py_run_string", 
            "reticulate::use_python",
            "reticulate::import_from_path"
        ),
        lib_path = lib_path,
        external_function_name = function_name,
        external_package_name = package_name
        )
    }
    # end check of the required function from the required packages
    # end package checking
    
    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "req_package"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat.args, collapse = "),base::missing("), "))")))
    if(base::any(tempo)){ # normally no NA for base::missing() output
        tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args[tempo], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = req_package, class = "character", fun.name = function_name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(python_exec_path)){
        tempo <- saferDev::arg_check(data = python_exec_path, class = "character", length = 1, fun.name = function_name, safer_check = FALSE) ; base::eval(ee)
        if(tempo$problem == FALSE){
            if( ! base::all(base::file.exists(python_exec_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and python_exec_path == NA
                tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nFILE PATH INDICATED IN THE python_exec_path ARGUMENT DOES NOT EXISTS:\n", base::paste(python_exec_path, collapse = "\n"))
                text.check <- base::c(text.check, tempo.cat)
                argum.check <- base::c(argum.check, TRUE)
            }
        }
    }
    if( ! base::is.null(python_lib_path)){
        tempo <- saferDev::arg_check(data = python_lib_path, class = "vector", mode = "character", fun.name = function_name, safer_check = FALSE) ; base::eval(ee)
        if(tempo$problem == FALSE){
            if( ! base::all(base::dir.exists(python_lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and python_lib_path == NA
                tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE python_lib_path ARGUMENT DOES NOT EXISTS:\n", base::paste(python_lib_path, collapse = "\n"))
                text.check <- base::c(text.check, tempo.cat)
                argum.check <- base::c(argum.check, TRUE)
            }
        }
    }
    # lib_path already checked above
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg_user_setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg_user_setting) == 0)){
        tempo.arg <- base::names(arg_user_setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because base::is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
     tempo.arg <-base::c(
        "req_package",
        # "python_exec_path", # inactivated because can be null
        # "python_lib_path", # inactivated because can be null
        # "lib_path", # inactivated because can be null
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with base::is.null()
        tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    # warning initiation
    # end warning initiation
    # other checkings
    # end other checkings
    # end second round of checking and data preparation

    # main code
    if(base::is.null(python_exec_path)){
        python_exec_path <- reticulate::py_run_string("
import sys ;
path_lib = sys.path
") # python string
        python_exec_path <- python_exec_path$path_lib
    }
    if(base::is.null(python_lib_path)){
        python_lib_path <- reticulate::py_run_string("
import sys ;
path_lib = sys.path
") # python string
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
    # output
    # warning output
    # end warning output
    # end output
    # end main code
}
