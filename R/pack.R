#' @title pack
#' @description
#' Check if the specified R packages are present in the computer and import them into the working environment.
#' @param req.package Character vector of package names to import.
#' @param load Single logical value. Load the package into the environment (using library())? Interesting if packages are not in default folders or for checking the functions names of packages using search().
#' @param lib.path Optional character vector specifying the absolute pathways of the directories containing some of the listed packages in the req.package argument, if not in the default directories. Ignored if NULL.
#' @returns Nothing.
#' @details
#' REQUIRED PACKAGES
#' 
#' none
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' check()
#' 
#' @examples
#' # pack(req.package = "nopackage") # should return an error
#' pack(req.package = "ggplot2")
#' pack(req.package = "ggplot2", lib.path = "C:/Users/yhan/AppData/Local/R/win-library/4.3") 
#' # should return an error if the lib.path argument is not an existing directory
#' @importFrom utils installed.packages
#' @export
pack <- function(
        req.package, 
        load = FALSE, 
        lib.path = NULL
){
    # DEBUGGING
    # req.package = "ggplot2" ; lib.path = "C:/Program Files/R/R-4.3.1/library"
    # req.package = "serpentine" ; lib.path = "C:/Users/yhan/AppData/Local/Temp/RtmpkfYz9V/RLIBS_2cb051b94b38"
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # package checking
    # check of lib.path
    if( ! is.null(lib.path)){
        if( ! all(typeof(lib.path) == "character")){ # no na.rm = TRUE with typeof
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! all(dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end check of lib.path
    # cuteDev required function checking
    req.function <- c(
        "check"
    )
    tempo <- NULL
    for(i1 in req.function){
        if(length(find(i1, mode = "function")) == 0L){
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
    tempo <- eval(parse(text = paste0("missing(", paste0(mandat.args, collapse = ") | missing("), ")")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", "HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- check(data = req.package, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
    tempo <- check(data = load, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
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
    if( ! is.null(arg.check)){
        if(any(arg.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using check()
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
    tempo.arg <-c(
        "req.package", 
        "load" 
        # "lib.path" # inactivated because can be null
    )
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.null)
    if(any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- paste0("ERROR IN ", function.name, ":\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
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
    
    # main code
    if(is.null(lib.path)){
        lib.path <- .libPaths() # .libPaths(new = lib.path) # or .libPaths(new = c(.libPaths(), lib.path))
    }else{
        .libPaths(new = sub(x = lib.path, pattern = "/$|\\\\$", replacement = "")) # .libPaths(new = ) add path to default path. BEWARE: .libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
        lib.path <- .libPaths()
    }
    tempo <- NULL
    for(i1 in 1:length(req.package)){
        if( ! req.package[i1] %in% rownames(utils::installed.packages(lib.loc = lib.path))){
            tempo <- c(tempo, req.package[i1])
        }
    }
    if( ! is.null(tempo)){
        tempo.cat <- paste0(
            "ERROR IN ", 
            function.name, 
            ": PACKAGE", 
            ifelse(length(tempo) == 1L, paste0("\n\n", tempo, "\n\n"), paste0("S\n", paste(tempo, collapse = "\n"), "\n")), 
            "MUST BE INSTALLED IN", 
            ifelse(length(lib.path) == 1L, "", " ONE OF THESE FOLDERS"), 
            ":\n", 
            paste(lib.path, collapse = "\n")
        )
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else if(load == TRUE){
        for(i2 in 1:length(req.package)){
            suppressMessages(suppressWarnings(suppressPackageStartupMessages(library(req.package[i2], lib.loc = lib.path, quietly = TRUE, character.only = TRUE))))
        }
    }
    # output
    # warning output
    # end warning output
    # end output
    # end main code
}
