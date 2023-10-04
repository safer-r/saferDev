######## fun_secu() #### verif that local variables are not present in other envs

#' @title fun_secu
#' @description
#' Verify that variables in the environment defined by the pos parameter are not present in the above environment (following R Scope). 
#' 
#' This can be used to avoid R scope preference of functions like get().
#' @param pos Single integer indicating the position of the environment checked (argument n of parent.frame()). Value 1 means one step above the fun_secu() local environment (by default). This means that when fun_secu(pos = 1) is used inside a function A, it checks if variables in the local environment of this function A are also present in above environments (following R Scope). When fun_secu(pos = 1) is used in the Global environment, it checks the objects of this environment.
#' @param name Single character string indicating the name of the function checked. If NULL, fun_secu() checks all the variables of the environment indicated by pos, as explained in the pos argument description. If non-null, fun_secu() checks all the variables present in the local env of the function will be checked in the above envs (which includes the working environment (Global env).
#' @returns A character string of the local variables that match variables in the different environments of the R scope, or NULL if no match.
#' @details
#' REQUIRED PACKAGES
#' 
#' None
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' fun_check()
#' 
#' @examples
#' fun_secu()
#' fun_secu(pos = 2)
#' mean <- 0 ; 
#' fun1 <- function(){sd <- 1 ; 
#' fun_secu(name = as.character(sys.calls()[[length(sys.calls())]]))} ; 
#' fun2 <- function(){cor <- 2 ; fun1()} ; fun1() ; fun2() ; 
#' rm(mean) 
#' # sys.calls() gives the function name at top stack of the imbricated functions, 
#' # sys.calls()[[length(sys.calls())]] the name of the just above function. 
#' # This can also been used for the above function: as.character(sys.call(1)).
#' test.pos <- 2 ; mean <- 0 ; 
#' fun1 <- function(){sd <- 1 ; 
#' fun_secu(pos = test.pos, 
#' name = if(length(sys.calls()) >= test.pos)
#' {as.character(sys.calls()[[length(sys.calls()) + 1 - test.pos]])}
#' else{search()[ (1:length(search()))[test.pos - length(sys.calls())]]})} ; 
#' fun2 <- function(){cor <- 2 ; fun1()} ; fun1() ; fun2() ; rm(mean) 
#' # for argument name, here is a way to have the name of the tested environment 
#' # according to test.pos value
#' @export
fun_secu <- function(
        pos = 1, 
        name = NULL
){
    # DEBUGGING
    # pos = 1 ; name = NULL # for function debugging
    
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # required function checking
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
        tempo.cat <- paste0("ERROR IN ", function.name, "\nREQUIRED cute FUNCTION", ifelse(length(tempo) > 1, "S ARE", " IS"), " MISSING IN THE R ENVIRONMENT:\n", paste0(tempo, collapse = "()\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end required function checking
    # reserved words to avoid bugs (names of dataframe columns used in this function)
    # end reserved words to avoid bugs (used in this function)
    # argument primary checking
    # arg with no default values
    # end arg with no default values
    
    # argument primary checking
    # using fun_check()
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- fun_check(data = pos, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(name)){
        tempo <- fun_check(data = name, class = "vector", typeof = "character", length = 1, fun.name = function.name) ; eval(ee)
    }
    if( ! is.null(arg.check)){
        if(any(arg.check) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end using fun_check()
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
    # end argument primary checking
    
    # second round of checking and data preparation
    # management of NA arguments
    if( ! (all(class(arg.user.setting) == "list") & length(arg.user.setting) == 0)){
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
        "pos"
        # "name" # inactivated because can be null
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
    # reserved word checking
    # end reserved word checking
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
    if( ! all(sapply(match.list, FUN = is.null))){
        output <- paste0("SOME VARIABLES ", ifelse(is.null(name), "OF THE CHECKED ENVIRONMENT", paste0("OF ", name)), " ARE ALSO PRESENT IN :\n", paste0(names(match.list[ ! sapply(match.list, FUN = is.null)]), ": ", sapply(match.list[ ! sapply(match.list, FUN = is.null)], FUN = paste0, collapse = " "), collapse = "\n"))
    }else{
        output <- NULL
    }
    # output
    return(output)
    # end output
    # end main code
}
