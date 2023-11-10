#' @title python_pkg_check
#' @description
#' Check if the specified python packages are present in the computer (no import).
#' @param req.package Character vector of package names to import.
#' @param python.exec.path Single optional character vector specifying the absolute pathways of the executable python file to use (associated to the packages to use). If NULL, the reticulate::import_from_path() function used in python_pkg_check() seeks for an available version of python.exe, and then uses python_config(python_version, required_module, python_versions). But might not be the correct one for the lib.path parameter specified. Thus, it is recommanded to do not leave NULL, notably when using computing clusters.
#' @param lib.path Optional character vector specifying the absolute pathways of the directories containing some of the listed packages in the req.package argument, if not in the default directories.
#' @param R.lib.path Absolute path of the reticulate packages, if not in the default folders.
#' @returns Nothing.
#' @details 
#' REQUIRED PACKAGES
#' 
#' reticulate
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' check()
#'
#' pack()
#' 
#' 
#' WARNINGS
#' 
#' for python 3.7. Previous versions return an error "Error in sys$stdout$flush() : attempt to apply non-function"
#' @importFrom reticulate py_run_string
#' @importFrom reticulate use_python
#' @importFrom reticulate import_from_path
#' @examples
#' # example of error message
#' 
#' # python_pkg_check(req.package = "nopackage")
#' 
#' 
#' # example without error message 
#' # (require the installation of the python serpentine package 
#' # from https://github.com/koszullab/serpentine
#' 
#' # python_pkg_check(req.package = "serpentine")
#' 
#' 
#' # another example of error message
#' 
#' # python_pkg_check(req.package = "serpentine", lib.path = "blablabla")
#' @export
python_pkg_check <- function(
        req.package, 
        python.exec.path = NULL, 
        lib.path = NULL, 
        R.lib.path = NULL
){
    # DEBUGGING
    # req.package = "serpentine" ; python.exec.path = "C:/ProgramData/Anaconda3/python.exe" ; lib.path = "c:/programdata/anaconda3/lib/site-packages/" ; R.lib.path = NULL
    # req.package = "bad" ; lib.path = NULL ; R.lib.path = NULL
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    
    # package checking
    # check of R.lib.path
    if( ! is.null(R.lib.path)){
        if( ! all(typeof(R.lib.path) == "character")){ # no na.rm = TRUE with typeof
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE R.lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", paste(R.lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! all(dir.exists(R.lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and R.lib.path == NA
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE R.lib.path ARGUMENT DOES NOT EXISTS:\n", paste(R.lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end check of R.lib.path
    # cuteDev required function checking
    req.function <- c(
        "check",
        "pack"
    )
    tempo <- NULL
    for(i1 in req.function){
        if(length(utils::find(i1, mode = "function")) == 0L){
            tempo <- c(tempo, i1)
        }
    }
    if( ! is.null(tempo)){
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFUNCTION", ifelse(length(tempo) > 1, "S ", ""), " FROM THE cuteDev PACKAGE", ifelse(length(tempo) > 1, " ARE", " IS"), " MISSING IN THE R ENVIRONMENT:\n", paste0(tempo, collapse = "()\n"), "()")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end cutedev required function checking
    # check of other required packages
    # end check of other required packages
    # end package checking
    # check of the required function from the required packages
    # end check of the required function from the required packages
    
    # argument primary checking
    # arg with no default values
    mandat.args <- c(
        "req.package"
    )
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, collapse = "),missing("), "))")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- check(data = req.package, class = "character", fun.name = function.name) ; eval(ee)
    if( ! is.null(python.exec.path)){
        tempo <- check(data = python.exec.path, class = "character", length = 1, fun.name = function.name) ; eval(ee)
        if(tempo$problem == FALSE){
            if( ! all(file.exists(python.exec.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and python.exec.path == NA
                tempo.cat <- paste0("ERROR IN ", function.name, ": FILE PATH INDICATED IN THE python.exec.path ARGUMENT DOES NOT EXISTS:\n", paste(python.exec.path, collapse = "\n"))
                text.check <- c(text.check, tempo.cat)
                arg.check <- c(arg.check, TRUE)
            }
        }
    }
    if( ! is.null(lib.path)){
        tempo <- check(data = lib.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
        if(tempo$problem == FALSE){
            if( ! all(dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
                tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
                text.check <- c(text.check, tempo.cat)
                arg.check <- c(arg.check, TRUE)
            }
        }
    }
    if( ! is.null(R.lib.path)){
        tempo <- check(data = R.lib.path, class = "character", fun.name = function.name) ; eval(ee)
        if(tempo$problem == FALSE){
            if( ! all(dir.exists(R.lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and R.lib.path == NA
                tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE R.lib.path ARGUMENT DOES NOT EXISTS:\n", paste(R.lib.path, collapse = "\n"))
                text.check <- c(text.check, tempo.cat)
                arg.check <- c(arg.check, TRUE)
            }
        }
    }
    if( ! is.null(arg.check)){
        if(any(arg.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # check with r_debugging_tools
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using check()
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # management of NA arguments
    if( ! (all(class(arg.user.setting) == "list", na.rm = TRUE) & length(arg.user.setting) == 0)){
        tempo.arg <- names(arg.user.setting) # values provided by the user
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- paste0("ERROR IN ", function.name, "\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    # end management of NULL arguments
    
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    
    # warning initiation
    # end warning initiation
    
    # other checkings
    # end other checkings
    
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # end second round of checking and data preparation
    
    # package checking
    pack(req.package = "reticulate", lib.path = R.lib.path)
    # end package checking
    # main code
    if(is.null(python.exec.path)){
        python.exec.path <- reticulate::py_run_string("
import sys ;
path_lib = sys.path
") # python string
        python.exec.path <- python.exec.path$path_lib
    }
    if(is.null(lib.path)){
        lib.path <- reticulate::py_run_string("
import sys ;
path_lib = sys.path
") # python string
        lib.path <- lib.path$path_lib
    }
    reticulate::use_python(Sys.which(python.exec.path), required = TRUE) # required to avoid the use of erratic python exec by reticulate::import_from_path()
    for(i1 in 1:length(req.package)){
        tempo.try <- vector("list", length = length(lib.path))
        for(i2 in 1:length(lib.path)){
            tempo.try[[i2]] <- suppressWarnings(try(reticulate::import_from_path(req.package[i1], path = lib.path[i2]), silent = TRUE))
            tempo.try[[i2]] <- suppressWarnings(try(reticulate::import_from_path(req.package[i1], path = lib.path[i2]), silent = TRUE)) # done twice to avoid the error message  about flushing present the first time but not the second time. see https://stackoverflow.com/questions/57357001/reticulate-1-13-error-in-sysstdoutflush-attempt-to-apply-non-function
        }
        if(all(sapply(tempo.try, FUN = grepl, pattern = "[Ee]rror"), na.rm = TRUE)){
            print(tempo.try)
            tempo.cat <- paste0("ERROR IN ", function.name, ": PACKAGE ", req.package[i1], " MUST BE INSTALLED IN THE MENTIONNED DIRECTORY:\n", paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
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
