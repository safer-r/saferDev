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
        sep = 2
){
    # DEBUGGING
    # data = 1:3 ; output = "results.txt" ; path = "C:/Users/yhan/Desktop" ; overwrite = TRUE ; rownames.kept = FALSE ; vector.cat = FALSE ; noquote = FALSE ; sep = 2 # for function debugging
    
    # package name
    package.name <- "saferDev"
    # end package name

    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # critical operator checking
    .base_op_check(external.function.name = function.name)
    # end critical operator checking
    # package checking
    # check of lib.path
    # end check of lib.path
    # check of the required function from the required packages
    # end check of the required function from the required packages
    # end package checking
    
    # argument primary checking
    # arg with no default values
    mandat.args <- c(
        "data",
        "path"
    )
    tempo <- eval(parse(text = paste0("missing(", paste0(mandat.args, collapse = ") | missing("), ")")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", "HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(argum.check <- c(argum.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- arg_check(data = output, class = "character", length = 1, fun.name = function.name) ; eval(ee)
    if(tempo$problem == FALSE & output == ""){
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: output ARGUMENT AS \"\" DOES NOT CORRESPOND TO A VALID FILE NAME")
        text.check <- c(text.check, tempo.cat)
        argum.check <- c(argum.check, TRUE)
    }
    tempo <- arg_check(data = path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
    if(tempo$problem == FALSE){
        if( ! all(dir.exists(path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: path ARGUMENT DOES NOT CORRESPOND TO EXISTING DIRECTORY\n", paste(path, collapse = "\n"))
            text.check <- c(text.check, tempo.cat)
            argum.check <- c(argum.check, TRUE)
        }
    }
    tempo <- arg_check(data = overwrite, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- arg_check(data = rownames.kept, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- arg_check(data = vector.cat, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- arg_check(data = noquote, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- arg_check(data = sep, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, fun.name = function.name) ; eval(ee)
    if( ! is.null(argum.check)){
        if(any(argum.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
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
    # management of NA arguments
    if( ! (all(class(arg.user.setting) == "list", na.rm = TRUE) & length(arg.user.setting) == 0)){
        tempo.arg <- names(arg.user.setting) # values provided by the user
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-c(
        "data", 
        "output", 
        "path",
        "overwrite", 
        "rownames.kept", 
        "vector.cat", 
        "noquote",
        "sep" 
    )
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.null)
    if(any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
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
    if( ! is.null(data)){
        if(all(class(data) == "data.frame") | all(class(data) == "table") | all(class(data) %in% c("matrix", "array"))){ # before R4.0.0, it was  all(class(data) %in% c("matrix", "data.frame", "table")) # class() never returns NA
            if(rownames.kept == FALSE & all(class(data) == "data.frame") & nrow(data) != 0 & nrow(data) <= 4){ # for data frames with nrows <= 4
                rownames.output.tables <- ""
                length.rows <- nrow(data)
                for(i in 1:length.rows){ # replace the rownames of the first 4 rows by increasing number of spaces (because identical row names not allowed in data frames). This method cannot be extended to more rows as the printed data frame is shifted on the right because of "big empty rownames"
                    rownames.output.tables <- c(rownames.output.tables, paste0(rownames.output.tables[i]," ", collapse=""))
                }
                row.names(data) <- rownames.output.tables[1:length.rows]
            }else if(rownames.kept == FALSE & (all(class(data) == "table") | all(class(data) %in% c("matrix", "array")))){ # before R4.0.0, it was  & all(class(data) %in% c("matrix", "table"))
                rownames(data) <- rep("", nrow(data)) # identical row names allowed in matrices and tables
            }
            if(noquote == TRUE){
                utils::capture.output(noquote(data), file=paste0(path, "/", output), append = ! overwrite)
            }else{
                utils::capture.output(data, file=paste0(path, "/", output), append = ! overwrite)
            }
        }else if(is.vector(data) & all(class(data) != "list") & (length(data) == 1L | vector.cat == TRUE)){
            if(noquote == TRUE){
                cat(noquote(data), file= paste0(path, "/", output), append = ! overwrite)
            }else{
                cat(data, file= paste0(path, "/", output), append = ! overwrite)
            }
        }else if(all(base::mode(data) == "character")){ # characters (array, list, factor or vector with vector.cat = FALSE)
            if(noquote == TRUE){
                utils::capture.output(noquote(data), file=paste0(path, "/", output), append = ! overwrite)
            }else{
                utils::capture.output(data, file=paste0(path, "/", output), append = ! overwrite)
            }
        }else{ # other object (S4 for instance, which do not like noquote()
            utils::capture.output(data, file=paste0(path, "/", output), append = ! overwrite)
        }
        sep.final <- paste0(rep("\n", sep), collapse = "")
        write(sep.final, file= paste0(path, "/", output), append = TRUE) # add a sep
    }
    # end main code
    # output
    # warning output
    # end warning output
    # end output
}
