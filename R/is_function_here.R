#' @title is_function_here
#' @description
#' Check if required functions are present in installed packages. This controls modifications in of function names package versions.
#' @param fun Character vector of the names of the required functions, preceded by the name of the package they belong to and a double colon. Example: c("ggplot2::geom_point", "grid::gpar"). Warning: do not write "()" at the end of the function
#' @param lib.path Character vector specifying the absolute pathways of the directories containing the listed packages in the fun argument, if not in the default directories. Ignored if NULL.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns  An error message if at least one of the checked packages is missing in lib.path, or if at least one of the checked functions is missing in the required package, nothing otherwise.
#' @seealso \code{\link{exists}}, \code{\link[methods]{findFunction}}
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' # is_function_here(fun = "ggplot2::notgood") # commented because this example returns an error
#' is_function_here(fun = c("ggplot2::geom_point", "grid::gpar"))
#' @export


is_function_here <- function(
        fun, 
        lib.path = NULL,
        safer_check = TRUE
){
    # DEBUGGING
    # fun = "ggplot2::geom_point" ; lib.path = "C:/Program Files/R/R-4.3.1/library" ; external.function.name = "fun1" ; safer_check = TRUE
    # package name
    package.name <- "saferDev"
    # end package name
    # function name
    function.name <- base::paste0(base::as.list(base::match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- base::as.list(base::match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
            external.function.name = function.name,
            external.package.name = package.name)
    }
    # end critical operator checking
    # check of lib.path
    if( ! base::is.null(lib.path)){
        if( ! base::all(base::typeof(lib.path) == "character")){ # no na.rm = TRUE with typeof
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", base::paste(lib.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }else if( ! base::all(base::dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n"))
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
    # end check of the required function from the required packages
    # end package checking

    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "fun"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::missing(", base::paste0(mandat.args, collapse = ") | base::missing("), ")")))
    if(base::any(tempo)){ # normally no NA for base::missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    tempo <- arg_check(data = fun, class = "vector", mode = "character", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    # lib.path already checked above
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
    # end argument primary checking

    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because base::is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-base::c(
        "fun",
        # "lib.path", # inactivated because can be null
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with base::is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
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
    tempo.log <- base::grepl(x = fun, pattern = "^.+::.+$")
    if( ! base::all(tempo.log)){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nTHE STRING IN fun ARGUMENT MUST CONTAIN \"::\":\n", base::paste(fun[ ! tempo.log], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    tempo.log <- base::grepl(x = fun, pattern = "^.+\\(\\)$")
    if(base::any(tempo.log)){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nTHE STRING IN fun ARGUMENT MUST NOT FINISH BY \"()\":\n", base::paste(fun[tempo.log], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    pkg.fun.name.list <- base::strsplit(fun, "::") # package in 1 and function in 2
    pkg.name <- base::sapply(X = pkg.fun.name.list, FUN = function(x){x[1]})
    pkg.log <- pkg.name %in% base::rownames(utils::installed.packages(lib.loc = lib.path))
    if( ! base::all(pkg.log)){
        tempo <- pkg.name[ ! pkg.log]
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            function.name, 
            " OF THE ",
            package.name,
            " PACKAGE: ",
            "\nREQUIRED PACKAGE", 
            base::ifelse(base::length(tempo) == 1L, base::paste0(":\n", tempo), base::paste0("S:\n", base::paste(tempo, collapse = "\n"))), 
            "MUST BE INSTALLED IN", 
            base::ifelse(base::length(lib.path) == 1L, "", " ONE OF THESE FOLDERS"), 
            ":\n", 
            base::paste(lib.path, collapse = "\n")
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    fun.log <- base::sapply(X = pkg.fun.name.list, FUN = function(x){base::exists(x[2], envir = base::asNamespace(x[1]))})
    if( ! base::all(fun.log)){
        tempo <- fun[ ! fun.log]
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            function.name, 
            " OF THE ",
            package.name,
            " PACKAGE: ",
            "\nREQUIRED FUNCTION",
            base::ifelse(base::length(tempo) == 1L, " IS ", "S ARE "), 
            "MISSING IN THE INSTALLED PACKAGE", 
            base::ifelse(base::length(tempo) == 1L, base::paste0(":\n", tempo), base::paste0("S:\n", base::paste(tempo, collapse = "\n")))
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end main code
    # output
    # warning output
    # end warning output
    # end output
}