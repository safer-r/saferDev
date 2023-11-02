######## fun_secu() #### verif that local variables are not present in other envs

# todo list check OK
# Check r_debugging_tools-v1.4.R
# Check fun_test() 20201107 (see cute_checks.docx)
# example sheet
# check all and any OK
# -> clear to go Apollo
# -> transferred into the cute package

#' @title secu
#' @description
#' Verify that object names in the environment defined by the pos parameter are identical or not to object names in the above environment (following R Scope). This can be used to verify that names used for objects inside a function or in the working environment do not override names of objects already present in the above R environments, following the R scope.
#' @param pos Single non nul positive integer indicating the position of the environment checked (argument n of the parent.frame() function). Value 1 means one step above the fun_secu() local environment (by default). This means that when fun_secu(pos = 1) is used inside a function A, it checks if the name of any object in the local environment of this function A is also present in above environments, following R Scope, starting by the just above environment. When fun_secu(pos = 1) is used in the working (Global) environment (named .GlobalEnv), it checks the object names of this .GlobalEnv environment, in the above environments. See the examples below.
#' @param name Single character string indicating a string that will be added in the output string, for instance the name of a function inside which fun_secu() is used.
#' @details
#' REQUIRED PACKAGES
#' 
#' None
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' fun_check()
#' @examples
#' # Example in the working environment
#' 
#' mean <- 1 # creation of the object mean with value 1 in the .GlobalEnv environment, knowing that the mean() function also exists in the environment base, above .GlobalEnv.
#' t.test <- 1 # creation of the object t.test with value 1 in the .GlobalEnv environment, knowing that the t.test() function also exists in the environment stats, above .GlobalEnv.
#' search() # current R scope (order of the successive R environments).
#' find("mean") # where the objects with the name "mean" are present.
#' find("t.test") # where the objects with the name "mean" are present.
#' a <- fun_secu(pos = 1) # test if any object name of the global environment are above environments (or fun_secu(), as pos = 1 is the default value).
#' a # the output string of fun_sec().
#' cat(a) # the evaluated output.
#' fun_secu(pos = 2) # test if any object of the stats environment (one step above .GlobalEnv) are above environments. Returns NULL since no object names of stats are in above environments
#' 
#' 
#' # Example inside a function
#' 
#' fun1 <- function(){t.test <- 0 ; mean <- 5 ; fun_secu(pos = 1)} # fun_secu() will check if the object names inside the fun1 function exist in the .GlobalEnv environment and above.
#' fun1()
#' fun2 <- function(){t.test <- 0 ; mean <- 5 ; fun_secu(pos = 2)} # fun_secu() will check if the object names inside the fun2 function exist in the stats environment and above.
#' fun2()
#' fun3 <- function(){t.test <- 0 ; mean <- 5 ; fun_secu(pos = 2, name = "fun3")} # idem fun2() but with the name of the function fun2 indicated. Instead of writting name = "fun3", we can also use name = as.character(sys.calls()[[length(sys.calls())]]), as sys.calls() gives the function name at top stack of the imbricated functions, sys.calls()[[length(sys.calls())]] the name of the just above function. This can also been used for the above function: as.character(sys.call(1))
#' fun3()
#' test.pos <- 1
#' fun_secu(pos = test.pos, name = if(length(sys.calls()) >= test.pos){as.character(sys.calls()[[length(sys.calls()) + 1 - test.pos]])}else{search()[ (1:length(search()))[test.pos - length(sys.calls())]]}) # here is a way to have the name of the tested environment according to test.pos value
#' @returns A character string indicating the object names of the tested environment that match object names in the above environments, following the R scope, or NULL if no match.
#' @export
fun_secu <- function(
        pos = 1, 
        name = NULL
){
    # DEBUGGING
    # pos = 1 ; name = "mean" # for function debugging
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
    }
    # end check of lib.path
    # cuteDev required function checking
    req.function <- c(
        "fun_check"
    )
    tempo <- NULL
    for(i1 in req.function){
        if(length(find(i1, mode = "function")) == 0L){
            tempo <- c(tempo, i1)
        }
    }
    if( ! is.null(tempo)){
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFUNCTION", ifelse(length(tempo) > 1, "S ", ""), " FROM THE cuteDev PACKAGE", ifelse(length(tempo) > 1, " ARE", " IS"), " MISSING IN THE R ENVIRONMENT:\n", paste0(tempo, collapse = "()\n"), "()")
)
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
    # end arg with no default values
    # using fun_check()
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debugging: used by r_debugging_tools
    ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- fun_check(data = pos, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(name)){
        tempo <- fun_check(data = name, class = "vector", typeof = "character", fun.name = function.name) ; eval(ee)
    }
    if( ! is.null(arg.check)){
        if(any(arg.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end using fun_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
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
    tempo.arg <- c(
        "pos"
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
    
    # package checking
    # end package checking
    # main code
    # match.list <- vector("list", length = (length(sys.calls()) - 1 + length(search()) + ifelse(length(sys.calls()) == 1L, -1, 0))) # match.list is a list of all the environment tested (local of functions and R envs), length(sys.calls()) - 1 to remove the level of the fun_secu() function, sys.calls() giving all the names of the imbricated functions, including fun_secu, ifelse(length(sys.calls()) == 1L, -1, 0) to remove Global env if this one is tested
    tempo.name <- rev(as.character(unlist(sys.calls()))) # get names of frames (i.e., enclosed env)
    tempo.frame <- rev(sys.frames())  # get frames (i.e., enclosed env)
    # dealing with source()
    # source() used in the Global env creates three frames above the Global env, which should be removed because not very interesting for variable duplications. Add a <<-(sys.frames()) in this code and source anova_contrasts code to see this. With ls(a[[4]]), we can see the content of each env, which are probably elements of source()
    if(any(sapply(tempo.frame, FUN = environmentName) %in% "R_GlobalEnv")){
        global.pos <- which(sapply(tempo.frame, FUN = environmentName) %in% "R_GlobalEnv")
        # remove the global env (because already in search(), and all the oabove env
        tempo.name <- tempo.name[-c(global.pos:length(tempo.frame))]
        tempo.frame <- tempo.frame[-c(global.pos:length(tempo.frame))]
    }
    # end dealing with source()
    # might have a problem if(length(tempo.name) == 0L){
    match.list <- vector("list", length = length(tempo.name) + length(search())) # match.list is a list of all the environment tested (local of functions and R envs), length(sys.calls()) - 1 to remove the level of the fun_secu() function, sys.calls() giving all the names of the imbricated functions, including fun_secu, ifelse(length(sys.calls()) == 1L, -1, 0) to remove Global env if this one is tested
    ls.names <- c(tempo.name, search()) # names of the functions + names of the search() environments
    ls.input <- c(tempo.frame, as.list(search())) # environements of the functions + names of the search() environments
    names(match.list) <- ls.names # 
    match.list <- match.list[-c(1:(pos + 1))] # because we check only above pos
    ls.tested <- ls.input[[pos + 1]]
    ls.input <- ls.input[-c(1:(pos + 1))]
    for(i1 in 1:length(match.list)){
        if(any(ls(name = ls.input[[i1]], all.names = TRUE) %in% ls(name = ls.tested, all.names = TRUE))){
            match.list[i1] <- list(ls(name = ls.input[[i1]], all.names = TRUE)[ls(name = ls.input[[i1]], all.names = TRUE) %in% ls(name = ls.tested, all.names = TRUE)])
        }
    }
    # output
    # warning output
    # end warning output
    if( ! all(sapply(match.list, FUN = is.null), na.rm = TRUE)){
        output <- paste0("SOME VARIABLES ", ifelse(is.null(name), "OF THE CHECKED ENVIRONMENT", paste0("OF ", name)), " ARE ALSO PRESENT IN :\n", paste0(names(match.list[ ! sapply(match.list, FUN = is.null)]), ": ", sapply(match.list[ ! sapply(match.list, FUN = is.null)], FUN = paste0, collapse = " "), collapse = "\n"), "\n")
    }else{
        output <- NULL
    }
    return(output)
    # end output
    # end main code
}

