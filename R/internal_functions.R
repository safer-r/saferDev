#' @title .pkg_check
#' @description
#' Check if required packages are present in the computer for this package.
#' @param req.package Character vector of package names to check.
#' @param lib.path Character vector specifying the absolute pathways of the directories containing some of the listed packages in the req.package argument, if not in the default directories. Ignored if NULL.
#' @param external.function.name Name of the function using the .pkg_check() function.
#' @returns Nothing.
#' @examples
#' # .pkg_check(req.package = "nopackage") # commented because this example returns an error
#' # commented because this example returns an error if the lib.path argument is not an existing directory
#' @importFrom utils installed.packages
#' @keywords internal
#' @rdname internal_function


.pkg_check <- function(
        req.package, 
        lib.path,
        external.function.name
){
    # DEBUGGING
    # req.package = "ggplot2" ; lib.path = "C:/Program Files/R/R-4.3.1/library"
    # req.package = "serpentine" ; lib.path = "C:/Users/yhan/AppData/Local/Temp/RtmpkfYz9V/RLIBS_2cb051b94b38"
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # argument primary checking
    # arg with no default values
    mandat.args <- c(
        "req.package", 
        "lib.path", 
        "external.function.name"
    )
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, collapse = "),missing("), "))")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, " (INTERNAL FUNCTION OF THE cuteDev PACKAGE)\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args[tempo], collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
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
        # "lib.path",
        "external.function.name"
    )
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.null)
    if(any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- paste0("ERROR IN ", function.name, " (INTERNAL FUNCTION OF THE cuteDev PACKAGE:\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    # check of lib.path
    if( ! is.null(lib.path)){
        if( ! all(typeof(lib.path) == "character")){ # no na.rm = TRUE with typeof
            tempo.cat <- paste0("ERROR IN ", function.name, " (INTERNAL FUNCTION OF THE cuteDev PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! all(dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- paste0("ERROR IN ", function.name, " (INTERNAL FUNCTION OF THE cuteDev PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end check of lib.path
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
            external.function.name, 
            "() OF THE cuteDev PACKAGE. REQUIRED PACKAGE", 
            ifelse(length(tempo) == 1L, paste0(":\n", tempo, "\n\n"), paste0("S:\n", paste(tempo, collapse = "\n"), "\n\n")), 
            "MUST BE INSTALLED IN", 
            ifelse(length(lib.path) == 1L, "", " ONE OF THESE FOLDERS"), 
            ":\n", 
            paste(lib.path, collapse = "\n")
        )
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
}

