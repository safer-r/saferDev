#' @title report
#' @description
#' Log file function: print a character string or a data object into a same output file.
#' @param data Object to print in the output file. If NULL, nothing is done, with no warning.
#' @param output Single character string. Name of the output file.
#' @param path Location of the output file.
#' @param overwrite Single logical value. If output file already exists, defines if the printing is appended (default FALSE) or if the output file content is erased before printing (TRUE).
#' @param rownames.kept Single logical value. Defines whether row names have to be removed or not in small tables (less than length.rows rows).
#' @param vector.cat Single logical value. If TRUE print a vector of length > 1 using cat() instead of capture.output(). Otherwise (default FALSE) the opposite.
#' @param noquote Single logical value. If TRUE no quote are present for the characters.
#' @param sep Single integer representing the number of empty lines after printed data.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns Nothing.
#' @examples
#' #report()
#' report(data = 1:3, output = "results.txt", path = ".", overwrite = TRUE, 
#' rownames.kept = FALSE, vector.cat = FALSE, noquote = FALSE, sep = 2)
#' @export
report <- function(
        data, 
        output = "results.txt", 
        path, 
        overwrite = FALSE, 
        rownames.kept = FALSE, 
        vector.cat = FALSE, 
        noquote = TRUE, 
        sep = 2,
        safer_check = TRUE
){
    # DEBUGGING
    # data = 1:3 ; output = "results.txt" ; path = "C:/Users/yhan/Desktop" ; overwrite = TRUE ; rownames.kept = FALSE ; vector.cat = FALSE ; noquote = FALSE ; sep = 2 ; safer_check = TRUE # for function debugging
    
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
        .base_op_check(external.function.name = function.name
        )
    }
    # end critical operator checking
    # package checking
    # check of lib.path
    # end check of lib.path
    # check of the required function from the required packages
    # end check of the required function from the required packages
    # end package checking
    
    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "data",
        "path"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::missing(", base::paste0(mandat.args, collapse = ") | base::missing("), ")")))
    if(base::any(tempo)){ # normally no NA for missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", "HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    tempo <- arg_check(data = output, class = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if(tempo$problem == FALSE & output == ""){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: output ARGUMENT AS \"\" DOES NOT CORRESPOND TO A VALID FILE NAME")
        text.check <- base::c(text.check, tempo.cat)
        argum.check <- base::c(argum.check, TRUE)
    }
    tempo <- arg_check(data = path, class = "vector", mode = "character", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if(tempo$problem == FALSE){
        if( ! base::all(base::dir.exists(path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: path ARGUMENT DOES NOT CORRESPOND TO EXISTING DIRECTORY\n", base::paste(path, collapse = "\n"))
            text.check <- base::c(text.check, tempo.cat)
            argum.check <- base::c(argum.check, TRUE)
        }
    }
    tempo <- arg_check(data = overwrite, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- arg_check(data = rownames.kept, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- arg_check(data = vector.cat, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- arg_check(data = noquote, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- arg_check(data = sep, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
        
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
        # the next lines are inactivated but kept because at a time, I might have a problem with data (solved with data = NULL). These 4 lines are just to know how to detect a missing argument. Important here because if data is not provided, print the code of the data function
        # arg.user.list <- as.list(match.call(expand.dots = FALSE))[-1] # recover all the arguments provided by the function user (excluding the argument with defaults values not provided by the user. Thus, it is really the list indicated by the user)
        # default.arg.list <- formals(fun = sys.function(sys.parent())) # list of all the arguments of the function with their default values (not the values of the user !). It seems that ls() as first line of the function provide the names of the arguments (empty, called, etc., or not)
        # arg.without.default.value <- sapply(default.arg.list, is.symbol) & sapply(sapply(default.arg.list, as.character), identical, "") # logical to detect argument without default values (these are typeof "symbol" and class "name" and empty character
        # if( ! all(names(default.arg.list)[arg.without.default.value] %in% names(arg.user.list))){ # test that the arguments with no null values are provided by the user
        # tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: VALUE REQUIRED FOR THESE ARGUMENTS WITH NO DEFAULTS VALUES: ", paste(names(default.arg.list)[arg.without.default.value][ ! names(default.arg.list)[arg.without.default.value] %in% names(arg.user.list)], collapse = " "))
        # stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        # }
    # end argument primary checking
    
    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) == "list", na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-base::c(
        "data", 
        "output", 
        "path",
        "overwrite", 
        "rownames.kept", 
        "vector.cat", 
        "noquote",
        "sep",
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
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
    if( ! base::is.null(data)){
        if(base::all(base::class(data) == "data.frame") | base::all(base::class(data) == "table") | base::all(base::class(data) %in% base::c("matrix", "array"))){ # before R4.0.0, it was  all(class(data) %in% c("matrix", "data.frame", "table")) # class() never returns NA
            if(rownames.kept == FALSE & base::all(base::class(data) == "data.frame") & base::nrow(data) != 0 & base::nrow(data) <= 4){ # for data frames with nrows <= 4
                rownames.output.tables <- ""
                length.rows <- base::nrow(data)
                for(i in 1:length.rows){ # replace the rownames of the first 4 rows by increasing number of spaces (because identical row names not allowed in data frames). This method cannot be extended to more rows as the printed data frame is shifted on the right because of "big empty rownames"
                    rownames.output.tables <- base::c(rownames.output.tables, base::paste0(rownames.output.tables[i]," ", collapse=""))
                }
                base::row.names(data) <- rownames.output.tables[1:length.rows]
            }else if(rownames.kept == FALSE & (base::all(base::class(data) == "table") | base::all(base::class(data) %in% base::c("matrix", "array")))){ # before R4.0.0, it was  & all(class(data) %in% c("matrix", "table"))
                base::rownames(data) <- base::rep("", base::nrow(data)) # identical row names allowed in matrices and tables
            }
            if(noquote == TRUE){
                utils::capture.output(base::noquote(data), file=base::paste0(path, "/", output), append = ! overwrite)
            }else{
                utils::capture.output(data, file=base::paste0(path, "/", output), append = ! overwrite)
            }
        }else if(base::is.vector(data) & base::all(base::class(data) != "list") & (base::length(data) == 1L | vector.cat == TRUE)){
            if(noquote == TRUE){
                base::cat(base::noquote(data), file= base::paste0(path, "/", output), append = ! overwrite)
            }else{
                base::cat(data, file= base::paste0(path, "/", output), append = ! overwrite)
            }
        }else if(base::all(base::mode(data) == "character")){ # characters (array, list, factor or vector with vector.cat = FALSE)
            if(noquote == TRUE){
                utils::capture.output(base::noquote(data), file=base::paste0(path, "/", output), append = ! overwrite)
            }else{
                utils::capture.output(data, file=base::paste0(path, "/", output), append = ! overwrite)
            }
        }else{ # other object (S4 for instance, which do not like noquote()
            utils::capture.output(data, file=base::paste0(path, "/", output), append = ! overwrite)
        }
        sep.final <- base::paste0(base::rep("\n", sep), collapse = "")
        base::write(sep.final, file= base::paste0(path, "/", output), append = TRUE) # add a sep
    }
    # end main code
    # output
    # warning output
    # end warning output
    # end output
}
