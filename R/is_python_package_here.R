#' @title is_python_package_here
#' @description
#' Check if the specified python packages are installed locally.
#' @param req.package Character vector of package names to import.
#' @param python.exec.path Single optional character vector specifying the absolute pathways of the executable python file to use (associated to the packages to use). If NULL, the reticulate::import_from_path() function used in is_python_package_here() seeks for an available version of python.exe, and then uses python_config(python_version, required_module, python_versions). But might not be the correct one for the python.lib.path parameter specified. Thus, it is recommanded to do not leave NULL, notably when using computing clusters.
#' @param python.lib.path Optional character vector specifying the absolute pathways of the directories containing some of the listed packages in the req.package argument, if not in the default directories.
#' @param lib.path Absolute path of the reticulate packages, if not in the default folders.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns An error message if at least one of the checked packages is missing in python.lib.path, nothing otherwise.
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
#' # is_python_package_here(req.package = "nopackage")
#' 
#' 
#' # commented because this example returns an error if the python package is not installed on the computer
#' # (require the installation of the python serpentine package 
#' # from https://github.com/koszullab/serpentine
#' 
#' # is_python_package_here(req.package = "serpentine")
#' 
#' 
#' # another example of error message
#' 
#' # is_python_package_here(req.package = "serpentine", python.lib.path = "blablabla")
#' @export
is_python_package_here <- function(
        req.package, 
        python.exec.path = NULL, 
        python.lib.path = NULL, 
        lib.path = NULL,
        safer_check = TRUE
){
    # DEBUGGING
    # req.package = "serpentine" ; python.exec.path = "C:/ProgramData/Anaconda3/python.exe" ; python.lib.path = "c:/programdata/anaconda3/lib/site-packages/" ; lib.path = NULL ; safer_check = TRUE
    # req.package = "bad" ; python.lib.path = NULL ; lib.path = NULL ; safer_check = TRUE
    
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
    # check of lib.path
    if( ! base::is.null(lib.path)){
        if( ! base::all(base::typeof(lib.path) == "character")){ # no na.rm = TRUE with typeof
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", base::paste(lib.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }else if( ! base::all(base::dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }else{
            base::.libPaths(new = base::sub(x = lib.path, pattern = "/$|\\\\$", replacement = "")) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
            lib.path <- base::.libPaths()
        }
    }else{
        lib.path <- base::.libPaths() # base::.libPaths(new = lib.path) # or base::.libPaths(new = base::c(base::.libPaths(), lib.path))
    }
    # end check of lib.path
    # check of the required function from the required packages
    if(safer_check == TRUE){
        saferDev:::.pack_and_function_check(
        fun = base::c(
            "reticulate::py_run_string", 
            "reticulate::use_python",
            "reticulate::import_from_path"
        ),
        lib.path = lib.path,
        external_function_name = function_name,
        external_package_name = package_name
        )
    }
    # end check of the required function from the required packages
    # end package checking
    
    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "req.package"
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
    tempo <- saferDev::arg_check(data = req.package, class = "character", fun.name = function_name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(python.exec.path)){
        tempo <- saferDev::arg_check(data = python.exec.path, class = "character", length = 1, fun.name = function_name, safer_check = FALSE) ; base::eval(ee)
        if(tempo$problem == FALSE){
            if( ! base::all(base::file.exists(python.exec.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and python.exec.path == NA
                tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nFILE PATH INDICATED IN THE python.exec.path ARGUMENT DOES NOT EXISTS:\n", base::paste(python.exec.path, collapse = "\n"))
                text.check <- base::c(text.check, tempo.cat)
                argum.check <- base::c(argum.check, TRUE)
            }
        }
    }
    if( ! base::is.null(python.lib.path)){
        tempo <- saferDev::arg_check(data = python.lib.path, class = "vector", mode = "character", fun.name = function_name, safer_check = FALSE) ; base::eval(ee)
        if(tempo$problem == FALSE){
            if( ! base::all(base::dir.exists(python.lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and python.lib.path == NA
                tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE python.lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(python.lib.path, collapse = "\n"))
                text.check <- base::c(text.check, tempo.cat)
                argum.check <- base::c(argum.check, TRUE)
            }
        }
    }
    # lib.path already checked above
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
        "req.package",
        # "python.exec.path", # inactivated because can be null
        # "python.lib.path", # inactivated because can be null
        # "lib.path", # inactivated because can be null
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
    if(base::is.null(python.exec.path)){
        python.exec.path <- reticulate::py_run_string("
import sys ;
path_lib = sys.path
") # python string
        python.exec.path <- python.exec.path$path_lib
    }
    if(base::is.null(python.lib.path)){
        python.lib.path <- reticulate::py_run_string("
import sys ;
path_lib = sys.path
") # python string
        python.lib.path <- python.lib.path$path_lib
    }
    reticulate::use_python(base::Sys.which(python.exec.path), required = TRUE) # required to avoid the use of erratic python exec by reticulate::import_from_path()
    for(i1 in 1:base::length(req.package)){
        tempo.try <- base::vector("list", length = base::length(python.lib.path))
        for(i2 in 1:base::length(python.lib.path)){
            tempo.try[[i2]] <- base::suppressWarnings(base::try(reticulate::import_from_path(req.package[i1], path = python.lib.path[i2]), silent = TRUE))
            tempo.try[[i2]] <- base::suppressWarnings(base::try(reticulate::import_from_path(req.package[i1], path = python.lib.path[i2]), silent = TRUE)) # done twice to avoid the error message  about flushing present the first time but not the second time. see https://stackoverflow.com/questions/57357001/reticulate-1-13-error-in-sysstdoutflush-attempt-to-apply-non-function
        }
        if(base::all(base::sapply(tempo.try, FUN = grepl, pattern = "[Ee]rror"), na.rm = TRUE)){
            base::print(tempo.try)
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nPACKAGE ", req.package[i1], " MUST BE INSTALLED IN THE MENTIONNED DIRECTORY:\n", base::paste(python.lib.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        } # else{
        # suppressMessages(suppressWarnings(suppressPackageStartupMessages(assign(req.package[i1], reticulate::import(req.package[i1]))))) # not required because try() already evaluates
        # }
    }
    # output
    # warning output
    # end warning output
    # end output
    # end main code
}
