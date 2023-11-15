#' @title arg_test
#' @description
#' Test combinations of argument values of a function.
#' @param fun Single character string indicating the name of the function tested (without brackets).
#' @param arg Vector of character strings of arguments of fun. At least arguments that do not have default values must be present in this vector.
#' @param val List with number of compartments equal to the length of arg, each compartment containing values of the corresponding argument in arg. Each different value must be in a list or in a vector. For instance, argument 3 in arg is a logical argument (values accepted TRUE, FALSE, NA). Thus, compartment 3 of val can be either list(TRUE, FALSE, NA), or c(TRUE, FALSE, NA). NULL value alone must be written list(NULL).
#' @param expect.error List of exactly the same structure as val argument, but containing FALSE or TRUE, depending on whether error is expected (TRUE) or not (FALSE) for each corresponding value of val. A message is returned depending on discrepancies between the expected and observed errors. See the examples below. BEWARE: not always possible to write the expected errors for all the combination of argument values. Ignored if NULL.
#' @param parall Single logical value. Force parallelization ?
#' @param thread.nb Single numeric integer indicating the number of threads to use if ever parallelization is required. If NULL, all the available threads will be used. Ignored if parall is FALSE.
#' @param print.count Single interger value. Print a working progress message every print.count during loops. BEWARE: can increase substentially the time to complete the process if using a small integer value, like 10 for instance. Use Inf if no loop message desired.
#' @param plot.fun Single logical value. Plot the plotting function tested for each test? Ignored if the tested function is not a graphic function.
#' @param export Single logical value. Export the results into a .RData file and into a .tsv file? If FALSE, return a list into the console (see below). BEWARE: will be automatically set to TRUE if parall is TRUE. This means that when using parallelization, the results are systematically exported, not returned into the console.
#' @param res.path Single character string indicating the absolute pathway of the folder where the txt results and pdfs, containing all the plots, will be saved. Several txt and pdf, one per thread, if parallelization. Ignored if export is FALSE. Must be specified if parall is TRUE or if export is TRUE.
#' @param lib.path Vector of characters specifying the absolute pathways of the directories containing the required packages if not in the default directories. Ignored if NULL.
#' @returns
#' One or several pdf if a plotting function is tested and if the plot.fun argument is TRUE. 
#' 
#' And then, if export is FALSE a list containing:
#' - $fun: the tested function.
#' - $ini: the initial input values.
#' - $data: a data frame of all the combination tested, containing the following columns:
#'      # the different values tested, named by arguments
#'      - $kind: a vector of character strings indicating the kind of test result: either "ERROR", or "WARNING", or "OK".
#'      - $problem: a logical vector indicating if error or not.
#'      - $expected.error: optional logical vector indicating the expected error specified in the expect.error argument.
#'      - $message: either NULL if $kind is always "OK", or the messages.
#' - $sys.info: system and packages info.
#' 
#' If export is TRUE:
#' - the same list object into a .RData file.
#' - also the $data data frame into a .tsv file.
#' - if expect.error is non NULL and if any discrepancy, the $data data frame into a .tsv file but containing only the rows with discrepancies between expected and observed errors.
#' @details 
#' REQUIRED PACKAGES
#' 
#' lubridate
#' 
#' parallel if parall argument is TRUE (included in the R installation packages but not automatically loaded)
#' 
#' pdftools if parall argument is TRUE (included in the R installation packages but not automatically loaded)
#' 
#' If the tested function is in a package, this package must be imported first (no parallelization) or must be in the classical R package folder indicated by the lib.path argument (parallelization)
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' arg_check()
#' 
#' get_message()
#' 
#' pkg_check()
#' 
#' 
#' WARNINGS
#' 
#' Limited to 43 arguments with at least 2 values each. The total number of arguments tested can be more if the additional arguments have a single value. The limit is due to nested "for" loops (https://stat.ethz.ch/pipermail/r-help/2008-March/157341.html), but this limitation is away from the number of tests performed that would be 2^43.
#' @importFrom lubridate seconds_to_period
#' @importFrom pdftools pdf_combine
#' @examples
#' arg_test(fun = "unique", arg = c("x", "incomparables"), 
#' val = list(x = list(1:10, c(1,1,2,8), NA), incomparable = c(TRUE, FALSE, NA)))
#' 
#' arg_test(fun = "unique", arg = c("x", "incomparables"), 
#' val = list(x = list(1:10, c(1,1,2,8), NA), 
#' incomparable = c(TRUE, FALSE, NA)), expect.error = list(x = list(FALSE, FALSE, TRUE), 
#' incomparable = c(FALSE, FALSE, TRUE)))
#' 
#' arg_test(fun = "unique", arg = c("x", "incomparables"), 
#' val = list(x = list(1:10, c(1,1,2,8), NA), incomparable = c(TRUE, FALSE, NA)), 
#' expect.error = list(x = list(FALSE, FALSE, TRUE), incomparable = c(FALSE, FALSE, TRUE)), 
#' export = TRUE, res.path = getwd())
#' 
#' # Return an error if you run this example
#' # arg_test(fun = "round", arg = c("data", "dec.nb", "after.lead.zero"), val = list(L1 = list(c(1, 1.0002256, 1.23568), "a", NA), L2 = list(2, c(1,3), NA), L3 = c(TRUE, FALSE, NA)))
#' 
#' arg_test(fun = "plot", arg = c("x", "y"), val = list(x = list(1:10, 12:13, NA, (1:10)^2), 
#' y = list(1:10, NA, NA)),  expect.error = list(x = list(FALSE, TRUE, TRUE, FALSE), 
#' y = list(FALSE, TRUE, TRUE)), parall = FALSE, thread.nb = NULL, plot.fun = TRUE, 
#' res.path = ".", lib.path = NULL)
#' 
#' arg_test(fun = "plot", arg = c("x", "y"), val = list(x = list(1:10, 12:13, NA, 
#' (1:10)^2), y = list(1:10, NA, NA)), parall = FALSE, thread.nb = 4, 
#' plot.fun = TRUE, res.path = ".", 
#' lib.path = NULL)
#' 
#' # set.seed(1) ; 
#' # obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10), 
#' # stringsAsFactors = TRUE) ; arg_test(fun = "fun_gg_boxplot", arg = c("data1", "y", "categ"), 
#' # val = list(L1 = list(L1 = obs1), L2 = list(L1 = "Time"), L3 = list(L1 = "Group1")))
#' 
#' # set.seed(1) ; 
#' # obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10), 
#' # stringsAsFactors = TRUE) ; 
#' # arg_test(fun = "fun_gg_boxplot", arg = c("data1", "y", "categ"), val = list(L1 = list(obs1), 
#' # L2 = "Time", L3 = "Group1"), parall = FALSE, thread.nb = NULL, plot.fun = TRUE, 
#' # res.path = "C:\\Users\\yhan\\Desktop\\", lib.path = "C:\\Program Files\\R\\R-4.3.1\\library\\")
#' 
#' # library(ggplot2) ; arg_test(fun = "geom_histogram", arg = c("data", "mapping"), 
#' # val = list(x = list(data.frame(X = "a", stringsAsFactors = TRUE)), 
#' # y = list(ggplot2::aes(x = X))), parall = FALSE, thread.nb = NULL, 
#' # plot.fun = TRUE, res.path = "C:\\Users\\yhan\\Desktop\\", 
#' # lib.path = "C:\\Program Files\\R\\R-4.3.1\\library\\") 
#' # BEWARE: ggplot2::geom_histogram does not work
#' @export
arg_test <- function(
        fun, 
        arg, 
        val, 
        expect.error = NULL, 
        parall = FALSE, 
        thread.nb = NULL, 
        print.count = 10, 
        plot.fun = FALSE, 
        export = FALSE, 
        res.path = NULL, 
        lib.path = NULL
){
    # DEBUGGING
    # fun = "get_message"; arg = c("data", "kind"); val = list(data = "ls", kind = t26_20201124); list(TRUE, c(unlist(expected.error.t26))) ; expect.error = NULL ; parall = FALSE ; thread.nb = NULL ; print.count = 1 ; plot.fun = FALSE ; export = FALSE ; res.path = "C:\\Users\\yhan\\Desktop\\" ; lib.path = NULL 
    # fun = "unique" ; arg = "x" ; val = list(x = list(1:10, c(1,1,2,8), NA)) ; expect.error = list(x = list(FALSE, FALSE, TRUE)) ; parall = FALSE ; thread.nb = NULL ; plot.fun = FALSE ; export = FALSE ; res.path = "C:\\Users\\gmillot\\Desktop\\" ; lib.path = NULL ; print.count = 1 # for function debugging
    # fun = "unique" ; arg = c("x", "incomparables") ; val = list(x = list(1:10, c(1,1,2,8), NA), incomparable = c(TRUE, FALSE, NA)) ; expect.error = NULL ; parall = FALSE ; thread.nb = 2 ; plot.fun = FALSE ; export = TRUE ; res.path = "C:\\Users\\gmillot\\Desktop\\" ; lib.path = NULL ; print.count = 10 # for function debugging
    # fun = "plot" ; arg = c("x", "y") ; val = list(x = list(1:10, 12:13, NA), y = list(1:10, NA, NA)) ; expect.error = list(x = list(FALSE, FALSE, TRUE, FALSE), y = list(FALSE, TRUE, TRUE)) ; print.count = 10 ; parall = FALSE ; thread.nb = NULL ; plot.fun = TRUE ; export = TRUE ; res.path = "C:\\Users\\gmillot\\Desktop\\" ; lib.path = NULL # for function debugging
    # set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10), stringsAsFactors = TRUE) ; fun = "gg_boxplot" ; arg = c("data1", "y", "categ") ; val = list(L1 = list(L1 = obs1), L2 = list(L1 = "Time"), L3 = list(L1 = "Group1")) ; expect.error = NULL ; print.count = 10 ; parall = FALSE ; thread.nb = NULL ; plot.fun = TRUE ; export = TRUE ; res.path = "C:\\Users\\gmillot\\Desktop\\" ; lib.path = NULL # for function debugging
    # fun = "unique" ; arg = "x" ; val = list(x = list(1:3, mean)) ; expect.error = list(x = list(TRUE, TRUE)) ; parall = FALSE ; thread.nb = NULL ; plot.fun = FALSE ; export = FALSE ; res.path = "C:\\Users\\gmillot\\Desktop\\" ; lib.path = NULL ; print.count = 1 # for function debugging
    # function name
    ini <- match.call(expand.dots = FALSE) # initial parameters
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
        "arg_check", 
        "get_message", 
        "pkg_check"
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
        "fun", 
        "arg", 
        "val"
    )
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, collapse = "),missing("), "))")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # using arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(argum.check <- c(argum.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- arg_check(data = fun, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- arg_check(data = arg, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
    tempo <- arg_check(data = val, class = "list", na.contain = TRUE, fun.name = function.name) ; eval(ee)
    if( ! is.null(expect.error)){
        tempo <- arg_check(data = expect.error, class = "list", fun.name = function.name) ; eval(ee)
    }
    tempo <- arg_check(data = parall, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
    if(parall == TRUE){
        if( ! is.null(thread.nb)){
            tempo <- arg_check(data = thread.nb, typeof = "integer", double.as.integer.allowed = TRUE, neg.values = FALSE, length = 1, fun.name = function.name) ; eval(ee)
            if(tempo$problem == FALSE & thread.nb < 1){
                tempo.cat <- paste0("ERROR IN ", function.name, ": thread.nb PARAMETER MUST EQUAL OR GREATER THAN 1: ", thread.nb)
                text.check <- c(text.check, tempo.cat)
                argum.check <- c(argum.check, TRUE)
            }
        }
    }
    tempo <- arg_check(data = print.count, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    tempo <- arg_check(data = plot.fun, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- arg_check(data = export, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(res.path)){
        tempo <- arg_check(data = res.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
    }
    if( ! is.null(lib.path)){
        tempo <- arg_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
    }
    # end using arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
    # end argument primary checking
    # second round of checking and data preparation
    # new environment
    env.name <- paste0("env", as.numeric(Sys.time()))
    if(exists(env.name, where = -1)){ # verify if still ok when info() is inside a function
        tempo.cat <- paste0("ERROR IN ", function.name, ": ENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else{
        assign(env.name, new.env())
        assign("data", data, envir = get(env.name, envir = sys.nframe(), inherits = FALSE)) # data assigned in a new envir for test
    }
    # end new environment
    # management of NA arguments
    if( ! (all(class(arg.user.setting) == "list", na.rm = TRUE) & length(arg.user.setting) == 0)){
        tempo.arg <- names(arg.user.setting) # values provided by the user
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(any(tempo.log, na.rm = TRUE) == TRUE){
            tempo.cat <- paste0("ERROR IN ", function.name, "\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-c(
        "fun", 
        "arg", 
        "val", 
        # "expect.erro", # because can be NULL
        "parall", 
        # "thread.nb", # because can be NULL
        "print.count", 
        "plot.fun", 
        "export"
        # "res.path", # because can be NULL
        # "lib.path" # because can be NULL
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
    ini.warning.length <- options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    # end warning initiation
    # other checkings
    if(grepl(x = fun, pattern = "()$")){ # remove ()
        fun <- sub(x = fun, pattern = "()$", replacement = "")
    }
    if( ! exists(fun)){
        tempo.cat <- paste0("ERROR IN ", function.name, ": CHARACTER STRING IN fun ARGUMENT DOES NOT EXIST IN THE R WORKING ENVIRONMENT: ", paste(fun, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }else if( ! all(base::class(get(fun)) == "function")){ # here no env = sys.nframe(), inherit = FALSE for get() because fun is a function in the classical scope
        tempo.cat <- paste0("ERROR IN ", function.name, ": fun ARGUMENT IS NOT CLASS \"function\" BUT: ", paste(base::class(get(fun)), collapse = "\n"), "\nCHECK IF ANY CREATED OBJECT WOULD HAVE THE NAME OF THE TESTED FUNCTION")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    if(tempo$problem == FALSE & base::length(arg) == 0L){
        tempo.cat <- paste0("ERROR IN ", function.name, ": arg ARGUMENT CANNOT BE LENGTH 0")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    for(i2 in 1:base::length(val)){ # length(val) must be aequal to nb of arguments
        tempo1 <- arg_check(data = val[[i2]], class = "vector", na.contain = TRUE, fun.name = function.name)
        tempo2 <- arg_check(data = val[[i2]], class = "list", na.contain = TRUE, fun.name = function.name)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo.cat <- paste0("ERROR IN ", function.name, ": COMPARTMENT ", i2, " OF val ARGUMENT MUST BE A VECTOR OR A LIST")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }else if(tempo1$problem == FALSE){ # vector split into list compartments
            val[[i2]] <- split(x = val[[i2]], f = 1:base::length(val[[i2]])) # convert a vector into list, with each value of the vector in a compartment
        }
    }
    if(base::length(arg) != base::length(val)){
        tempo.cat <- paste0("ERROR IN ", function.name, ": LENGTH OF arg ARGUMENT MUST BE IDENTICAL TO LENGTH OF val ARGUMENT:\nHERE IT IS: ", base::length(arg), " VERSUS ", base::length(val))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    args <- names(formals(get(fun))) # here no env = sys.nframe(), inherit = FALSE for get() because fun is a function in the classical scope
    if( ! all(arg %in% args)){
        tempo.cat <- paste0("ERROR IN ", function.name, ": SOME OF THE STRINGS IN arg ARE NOT ARGUMENTS OF fun\nfun ARGUMENTS: ", paste(args, collapse = " "),"\nPROBLEMATIC STRINGS IN arg: ", paste(arg[ ! arg %in% args], collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    if(sum(sapply(val, FUN = length) > 1) > 43){
        tempo.cat <- paste0("ERROR IN ", function.name, ": CANNOT TEST MORE THAN 43 ARGUMENTS IF THEY ALL HAVE AT LEAST 2 VALUES EACH\nHERE THE NUMBER IS: ", sum(sapply(val, FUN = length) > 1))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    if( ! is.null(expect.error)){
        if(base::length(val) != base::length(expect.error)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": LENGTH OF val ARGUMENT MUST BE IDENTICAL TO LENGTH OF expect.error ARGUMENT:\nHERE IT IS: ", base::length(val), " VERSUS ", base::length(expect.error))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        for(i3 in 1:base::length(expect.error)){
            tempo1 <- arg_check(data = expect.error[[i3]], class = "vector",  mode = "logical", fun.name = function.name)
            tempo2 <- arg_check(data =  expect.error[[i3]], class = "list", fun.name = function.name)
            if(tempo1$problem == TRUE & tempo2$problem == TRUE){
                tempo.cat <- paste0("ERROR IN ", function.name, ": COMPARTMENT ", i3, " OF expect.error ARGUMENT MUST BE TRUE OR FALSE")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }else if(tempo1$problem == FALSE){ # vector split into list compartments
                expect.error[[i3]] <- split(x = expect.error[[i3]], f = 1:base::length(expect.error[[i3]])) # convert a vector into list, with each value of the vector in a compartment
            }
        }
        for(i2 in 1:length(expect.error)){
            if(all(class(expect.error[[i2]]) == "list")){
                if( ! all(class(val[[i2]]) == "list")){
                    tempo.cat <- paste0("ERROR IN ", function.name, ": expect.error ARGUMENT MUST BE A LIST OF EXACTLY THE SAME STRUCTURE AS val ARGUMENT.\nHERE COMPARTMENT ", i2, " OF expect.error IS CLASS ", paste(class(expect.error[[i2]]), collapse = " "), "\nAND COMPARTMENT ", i2, " OF val IS CLASS ", paste(class(val[[i2]]), collapse = " "))
                    stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                }else if(base::length(val[[i2]]) != base::length(expect.error[[i2]])){
                    tempo.cat <- paste0("ERROR IN ", function.name, ": LENGTH OF COMPARTMENT ", i2, " OF val ARGUMENT MUST BE IDENTICAL TO LENGTH OF COMPARTMENT ", i2, " OF expect.error ARGUMENT:\nHERE IT IS: ", base::length(val[[i2]]), " VERSUS ", base::length(expect.error[[i2]]))
                    stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                }
            }
        }
    }
    if( ! is.null(res.path)){
        if( ! all(dir.exists(res.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and res.path == NA
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE res.path ARGUMENT DOES NOT EXISTS:\n", paste(res.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
    }
    if(parall == TRUE & is.null(res.path)){
        tempo.cat <- paste0("ERROR IN ", function.name, ": res.path ARGUMENT MUST BE SPECIFIED IF parall ARGUMENT IS TRUE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    if(is.null(res.path) & export == TRUE){
        tempo.cat <- paste0("ERROR IN ", function.name, ": res.path ARGUMENT MUST BE SPECIFIED IF export ARGUMENT TRUE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    if(parall == TRUE & export == FALSE){
        export <- TRUE
        tempo.cat <- paste0("WARNING FROM ", function.name, ": export ARGUMENT CONVERTED TO TRUE BECAUSE thread.nb ARGUMENT IS NOT NULL")
        warning(paste0("\n", tempo.cat, "\n"), call. = FALSE)
    }
    if( ! is.null(lib.path)){
        if( ! all(dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
    }
    # end other checkings
    # reserved word checking
    # end reserved word checking
    # end second round of checking and data preparation
    
    # declaration of special plot functions
    sp.plot.fun <- c("gg_scatter", "gg_bar", "gg_boxplot")
    # end declaration of special plot functions
    # main code
    ini.warning.length <- base::options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    cat("\ntest JOB IGNITION\n")
    ini.date <- Sys.time()
    ini.time <- as.numeric(ini.date) # time of process begin, converted into seconds
    if(is.null(res.path)){
        res.path <- "."
    }
    if(export == TRUE){
        res.path <- paste0(res.path, "/arg_test_res_", trunc(ini.time))
        if(dir.exists(res.path)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": FOLDER ALREADY EXISTS\n", res.path, "\nPLEASE RERUN ONCE")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }else{
            dir.create(res.path)
        }
    }
    total.comp.nb <- prod(sapply(val, FUN = "length"))
    cat(paste0("\nTHE TOTAL NUMBER OF TESTS IS: ", total.comp.nb, "\n"))
    # creation of the txt instruction that includes several loops
    loop.string <- NULL
    end.loop.string <- NULL
    fun.args <- NULL
    fun.args2 <- NULL
    error.values <- NULL
    arg.values <- "list("
    for(i1 in 1:base::length(arg)){
        if(parall == FALSE){
            if(base::length(val[[i1]]) > 1){ # loop only if more than one value in base::length(val[[i1]])
                loop.string <- paste0(loop.string, "for(i", i1, " in 1:", base::length(val[[i1]]), "){")
                end.loop.string <- paste0(end.loop.string, "}")
            }
        }else{
            loop.string <- "for(i in x){"
            end.loop.string <- "}"
        }
        fun.args <- paste0(
            fun.args, 
            ifelse(i1 == 1L, "", ", "), 
            arg[i1], 
            " = val[[", 
            i1, 
            "]][[", 
            if(parall == FALSE){
                if(base::length(val[[i1]]) > 1){
                    paste0("i", i1)
                }else{
                    "1" # a unique element in val[[i1]]
                }
            }else{
                paste0("i.list[[", i1, "]][i]")
            }, 
            "]]"
        )
        fun.args2 <- paste0(
            fun.args2, 
            ifelse(i1 == 1L, "", ", "), 
            arg[i1], 
            " = val[[", 
            i1, 
            "]][[', ", 
            if(parall == FALSE){
                if(base::length(val[[i1]]) > 1){
                    paste0("i", i1)
                }else{
                    "1" # a unique element in val[[i1]]
                }
            }else{
                paste0("i.list[[", i1, "]][i]")
            }, 
            ", ']]"
        )
        arg.values <- paste0(
            arg.values, 
            "val[[", i1, "]][[", 
            if(parall == FALSE){
                if(base::length(val[[i1]]) > 1){
                    paste0("i", i1)
                }else{
                    "1" # a unique element in val[[i1]]
                }
            }else{
                paste0("i.list[[", i1, "]][i]")
            }, 
            "]]", 
            ifelse(i1 == base::length(arg), "", ", ")
        )
        error.values <- paste0(
            error.values, 
            ifelse(i1 == 1L, "", " | "), 
            "expect.error[[", i1, "]][[", 
            if(parall == FALSE){
                if(base::length(expect.error[[i1]]) > 1){
                    paste0("i", i1)
                }else{
                    "1" # a unique element in expect.error[[i1]]
                }
            }else{
                paste0("i.list[[", i1, "]][i]")
            }, 
            "]]"
        )
    }
    arg.values <- paste0(arg.values, ")")
    fun.test <- paste0(fun, "(", fun.args, ")")
    fun.test2 <- paste0("paste0('", fun, "(", fun.args2, ")')")
    # plot title for special plot functions
    if(plot.fun == TRUE){
        plot.kind <- "classic"
        if(fun %in% sp.plot.fun){
            plot.kind <- "special"
            if(any(arg %in% "title")){ # this is for the special functions
                tempo.match <- regmatches(x = fun.test, m = regexpr(text = fun.test, pattern = "title = .+[,)]"))
                tempo.match <- substring(tempo.match , 1, nchar(tempo.match) - 1)
                fun.test <- sub(x = fun.test, pattern = tempo.match, replacement = paste0(tempo.match, "\ntempo.title"))
            }else{
                fun.test <- sub(x = fun.test, pattern = ")$", replacement = ", title = tempo.title)")
            }
        }
    }
    # end plot title for special plot functions
    kind <- character()
    problem <- logical()
    expected.error <- logical()
    res <- character()
    count <- 0
    print.count.loop <- 0
    plot.count <- 0
    if(base::length(arg) == 1L){
        data <- data.frame()
    }else{ # base::length(arg) == 0L already tested above
        data <- data.frame(t(vector("character", base::length(arg))), stringsAsFactors = FALSE)[-1, ] # -1 to remove the single row created and to have an empty data frame with base::length(arg) columns
    }
    code <- paste(
        loop.string, '
count <- count + 1
print.count.loop <- print.count.loop + 1
arg.values.print <- eval(parse(text = arg.values)) # recover the list of the i1 compartment
for(j3 in 1:base::length(arg.values.print)){ # WARNING: do not use i1, i2 etc., here because already in loop.string
tempo.capt <- capture.output(tempo.error <- get_message(data =  paste0("paste(arg.values.print[[", j3, "]])"), kind = "error", header = FALSE, print.no = FALSE, env = get(env.name, envir = sys.nframe(), inherits = FALSE))) # collapsing arg.values sometimes does not work (with function for instance)
if( ! is.null(tempo.error)){
arg.values.print[[j3]] <- paste0("SPECIAL VALUE OF CLASS ", base::class(arg.values.print[[j3]]), " AND TYPE ", base::typeof(arg.values.print[[j3]]))
}
}
data <- rbind(data, as.character(sapply(arg.values.print, FUN = "paste", collapse = " ")), stringsAsFactors = FALSE) # each colum is a test
tempo.capt <- capture.output(tempo.try.error <- get_message(data = eval(parse(text = fun.test2)), kind = "error", header = FALSE, print.no = FALSE, env = get(env.name, envir = sys.nframe(), inherits = FALSE))) # data argument needs a character string but eval(parse(text = fun.test2)) provides it (eval parse replace the i1, i2, etc., by the correct values, meaning that only val is required in the env.name environment)
tempo.capt <- capture.output(tempo.try.warning <- get_message(data = eval(parse(text = fun.test2)), kind = "warning", header = FALSE, env = get(env.name, envir = sys.nframe(), inherits = FALSE), print.no = FALSE)) # data argument needs a character string but eval(parse(text = fun.test2)) provides it (eval parse replace the i1, i2, etc., by the correct values, meaning that only val is required in the env.name environment)
if( ! is.null(expect.error)){
expected.error <- c(expected.error, eval(parse(text = error.values)))
}
if( ! is.null(tempo.try.error)){
kind <- c(kind, "ERROR")
problem <- c(problem, TRUE)
res <- c(res, tempo.try.error)
}else{
if( ! is.null(tempo.try.warning)){
kind <- c(kind, "WARNING")
problem <- c(problem, FALSE)
res <- c(res, tempo.try.warning)
}else{
kind <- c(kind, "OK")
problem <- c(problem, FALSE)
res <- c(res, "")
}
if(plot.fun == TRUE){
invisible(grDevices::dev.set(window.nb))
plot.count <- plot.count + 1
tempo.title <- paste0("test_", sprintf(paste0("%0", nchar(total.comp.nb), "d"), ifelse(parall == FALSE, count, x[count])))
if(plot.kind == "classic"){ # not ggplot. So title has to be added in a classical way
# par(ann=FALSE, xaxt="n", yaxt="n", mar = rep(1, 4), bty = "n", xpd = NA) # old
par(bty = "n", xpd = NA) # new
eval(parse(text = fun.test))
# plot(1, 1, type = "n") # no display with type = "n"
x.left.dev.region <- (par("usr")[1] - ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * par("plt")[1] - ((par("usr")[2] - par("usr")[1]) / ((par("omd")[2] - par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * par("omd")[1])
y.top.dev.region <- (par("usr")[4] + ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - par("usr")[3]) / ((par("omd")[4] - par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * (1 - par("omd")[4]))
text(x = x.left.dev.region, y = y.top.dev.region, labels = tempo.title, adj=c(0, 1), cex = 1.5)
}else if(plot.kind == "special"){ # ggplot. title has been added above
eval(parse(text = fun.test))
}else{
tempo.cat <- paste0("INTERNAL CODE ERROR 1 IN ", function.name, ": CODE HAS TO BE MODIFIED")
stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
}
}
}
if(print.count.loop == print.count){
print.count.loop <- 0
tempo.time <- as.numeric(Sys.time())
tempo.lapse <- round(lubridate::seconds_to_period(tempo.time - ini.time))
final.loop <- (tempo.time - ini.time) / count * ifelse(parall == FALSE, total.comp.nb, base::length(x)) # expected duration in seconds # intra nb.compar loop lapse: time lapse / cycles done * cycles remaining
final.exp <- as.POSIXct(final.loop, origin = ini.date)
cat(paste0(ifelse(parall == FALSE, "\n", paste0("\nIN PROCESS ", process.id, " | ")), "LOOP ", format(count, big.mark=","), " / ", format(ifelse(parall == FALSE, total.comp.nb, base::length(x)), big.mark=","), " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp))
}
if(count == ifelse(parall == FALSE, total.comp.nb, base::length(x))){
tempo.time <- as.numeric(Sys.time())
tempo.lapse <- round(lubridate::seconds_to_period(tempo.time - ini.time))
cat(paste0(ifelse(parall == FALSE, "\nLOOP PROCESS ENDED | ", paste0("\nPROCESS ", process.id, " ENDED | ")), "LOOP ", format(count, big.mark=","), " / ", format(ifelse(parall == FALSE, total.comp.nb, base::length(x)), big.mark=","), " | TIME SPENT: ", tempo.lapse, "\n\n"))
}
', 
end.loop.string
    )
    # end creation of the txt instruction that includes several loops
    if(parall == TRUE){
        # list of i numbers that will be split
        i.list <- vector("list", base::length(val)) # positions to split in parallel jobs
        for(i2 in 1:base::length(arg)){
            if(i2 == 1L){
                tempo.divisor <- total.comp.nb / base::length(val[[i2]])
                i.list[[i2]] <- rep(1:base::length(val[[i2]]), each = as.integer(tempo.divisor))
                tempo.multi <- base::length(val[[i2]])
            }else{
                tempo.divisor <- tempo.divisor / base::length(val[[i2]])
                i.list[[i2]] <- rep(rep(1:base::length(val[[i2]]), each = as.integer(tempo.divisor)), time = as.integer(tempo.multi))
                tempo.multi <- tempo.multi * base::length(val[[i2]])
            }
        }
        # end list of i numbers that will be split
        tempo.cat <- paste0("PARALLELIZATION INITIATED AT: ", ini.date)
        cat(paste0("\n", tempo.cat, "\n"))
        tempo.thread.nb = parallel::detectCores(all.tests = FALSE, logical = TRUE) # detect the number of threads
        if(tempo.thread.nb < thread.nb){
            thread.nb <- tempo.thread.nb
        }
        tempo.cat <- paste0("NUMBER OF THREADS USED: ", thread.nb)
        cat(paste0("\n    ", tempo.cat, "\n"))
        Clust <- parallel::makeCluster(thread.nb, outfile = paste0(res.path, "/test_parall_log.txt")) # outfile to print or cat during parallelization (only possible in a file, outfile = "" do not work on windows)
        tempo.cat <- paste0("SPLIT OF TEST NUMBERS IN PARALLELISATION:")
        cat(paste0("\n    ", tempo.cat, "\n"))
        cluster.list <- parallel::clusterSplit(Clust, 1:total.comp.nb) # split according to the number of cluster
        utils::str(cluster.list) # using print(str()) add a NULL below the result
        cat("\n")
        paral.output.list <- parallel::clusterApply( # paral.output.list is a list made of thread.nb compartments, each made of n / thread.nb (mat theo column number) compartment. Each compartment receive the corresponding results of permut(), i.e., data (permuted mat1.perm), warning message, cor (final correlation) and count (number of permutations)
            cl = Clust,
            x = cluster.list,
            function.name = function.name, 
            ini = ini, 
            thread.nb = thread.nb, 
            print.count = print.count, 
            total.comp.nb = total.comp.nb, 
            sp.plot.fun = sp.plot.fun,
            i.list = i.list, 
            fun.tested = fun,
            arg.values = arg.values,
            fun.test = fun.test,
            fun.test2 = fun.test2,
            kind = kind,
            problem = problem,
            res = res,
            count = count,
            plot.count = plot.count,
            data = data,
            code = code,
            plot.fun = plot.fun, 
            res.path = res.path, 
            lib.path = lib.path, 
            fun = function(
        x, 
        function.name, 
        ini, 
        thread.nb, 
        print.count, 
        total.comp.nb, 
        sp.plot.fun, 
        i.list, 
        fun.tested, 
        arg.values, 
        fun.test, 
        fun.test2, 
        kind, 
        problem, 
        res, 
        count, 
        plot.count, 
        data, 
        code, 
        plot.fun, 
        res.path, 
        lib.path
            ){
                # check again: very important because another R
                process.id <- Sys.getpid()
                cat(paste0("\nPROCESS ID ", process.id, " -> TESTS ", x[1], " TO ", x[base::length(x)], "\n"))
                pkg_check(req.package = "lubridate", lib.path = lib.path, load = TRUE) # load = TRUE to be sure that functions are present in the environment
                # end check again: very important because another R
                # plot management
                if(plot.fun == TRUE){
                    grDevices::pdf(file = paste0(res.path, "/plots_from_test_", x[1], ifelse(base::length(x) == 1L, ".pdf", paste0("-", x[base::length(x)], ".pdf"))))
                }else{
                    grDevices::pdf(file = NULL) # send plots into a NULL file, no pdf file created
                }
                window.nb <- grDevices::dev.cur()
                invisible(grDevices::dev.set(window.nb))
                # end plot management
                # new environment
                ini.date <- Sys.time()
                ini.time <- as.numeric(ini.date) # time of process begin, converted into 
                env.name <- paste0("env", ini.time)
                if(exists(env.name, where = -1)){ # verify if still ok when arg_test() is inside a function
                    tempo.cat <- paste0("ERROR IN ", function.name, ": ENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
                    stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                }else{
                    assign(env.name, new.env())
                    assign("val", val, envir = get(env.name, envir = sys.nframe(), inherits = FALSE)) # var replaced by val
                }
                # end new environment
                print.count.loop <- 0
                suppressMessages(suppressWarnings(eval(parse(text = code))))
                colnames(data) <- arg
                if( ! is.null(expect.error)){
                    data <- data.frame(data, kind = kind, problem = problem, expected.error = expected.error, message = res, stringsAsFactors = FALSE)
                }else{
                    data <- data.frame(data, kind = kind, problem = problem, message = res, stringsAsFactors = FALSE)
                }
                row.names(data) <- paste0("arg_test_", sprintf(paste0("%0", nchar(total.comp.nb), "d"), x))
                sys.info <- utils::sessionInfo()
                sys.info$loadedOnly <- sys.info$loadedOnly[order(names(sys.info$loadedOnly))] # sort the packages
                invisible(grDevices::dev.off(window.nb))
                rm(env.name) # optional, because should disappear at the end of the function execution
                # output
                output <- list(fun = fun, ini = ini, data = data, sys.info = sys.info)
                save(output, file = paste0(res.path, "/arg_test_", x[1], ifelse(base::length(x) == 1L, ".RData", paste0("-", x[base::length(x)], ".RData"))))
                if(plot.fun == TRUE & plot.count == 0L){
                    warn.count <- warn.count + 1
                    tempo.warn <- paste0("(", warn.count,") IN PROCESS ", process.id, ": NO PDF PLOT BECAUSE ONLY ERRORS REPORTED")
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    file.remove(paste0(res.path, "/plots_from_arg_test_", x[1], ifelse(base::length(x) == 1L, ".pdf", paste0("-", x[base::length(x)], ".pdf"))))
                }
                table.out <- as.matrix(data)
                # table.out[table.out == ""] <- " " # does not work # because otherwise utils::read.table() converts "" into NA
                table.out <- gsub(table.out, pattern = "\n", replacement = " ")
                utils::write.table(table.out, file = paste0(res.path, "/table_from_arg_test_", x[1], ifelse(base::length(x) == 1L, ".tsv", paste0("-", x[base::length(x)], ".tsv"))), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "")
            }
        )
        parallel::stopCluster(Clust)
        # files assembly
        if(base::length(cluster.list) > 1){
            for(i2 in 1:base::length(cluster.list)){
                tempo.file <- paste0(res.path, "/table_from_arg_test_", min(cluster.list[[i2]], na.rm = TRUE), ifelse(base::length(cluster.list[[i2]]) == 1L, ".tsv", paste0("-", max(cluster.list[[i2]], na.rm = TRUE), ".tsv"))) # txt file
                tempo <- utils::read.table(file = tempo.file, header = TRUE, stringsAsFactors = FALSE, sep = "\t", row.names = 1, comment.char = "", colClasses = "character") #  row.names = 1 (1st column) because now utils::read.table() adds a NA in the header if the header starts by a tabulation, comment.char = "" because colors with #, colClasses = "character" otherwise convert "" (from NULL) into NA
                if(file.exists(paste0(res.path, "/plots_from_arg_test_", min(cluster.list[[i2]], na.rm = TRUE), ifelse(base::length(cluster.list[[i2]]) == 1L, ".pdf", paste0("-", max(cluster.list[[i2]], na.rm = TRUE), ".pdf"))))){
                    tempo.pdf <- paste0(res.path, "/plots_from_arg_test_", min(cluster.list[[i2]], na.rm = TRUE), ifelse(base::length(cluster.list[[i2]]) == 1L, ".pdf", paste0("-", max(cluster.list[[i2]], na.rm = TRUE), ".pdf"))) # pdf file
                }else{
                    tempo.pdf <- NULL
                }
                tempo.rdata <- paste0(res.path, "/arg_test_", min(cluster.list[[i2]], na.rm = TRUE), ifelse(base::length(cluster.list[[i2]]) == 1L, ".RData", paste0("-", max(cluster.list[[i2]], na.rm = TRUE), ".RData"))) # RData file
                if(i2 == 1L){
                    final.file <- tempo
                    final.pdf <- tempo.pdf
                    # new env for RData combining
                    env.name <- paste0("env", ini.time)
                    if(exists(env.name, where = -1)){ # verify if still ok when arg_arg_test() is inside a function
                        tempo.cat <- paste0("ERROR IN ", function.name, ": ENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
                        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                        # end new env for RData combining
                    }else{
                        assign(env.name, new.env())
                        load(tempo.rdata, envir = get(env.name))
                        tempo.rdata1 <- tempo.rdata
                        assign("final.output", get("output", envir = get(env.name)), envir = get(env.name))
                    }
                }else{
                    final.file <- rbind(final.file, tempo, stringsAsFactors = TRUE)
                    final.pdf <- c(final.pdf, tempo.pdf)
                    load(tempo.rdata, envir = get(env.name))
                    if( ! identical(get("final.output", envir = get(env.name))[c("R.version", "locale", "platform")], get("output", envir = get(env.name))[c("R.version", "locale", "platform")])){
                        tempo.cat <- paste0("ERROR IN ", function.name, ": DIFFERENCE BETWEEN OUTPUTS WHILE THEY SHOULD BE IDENTICAL\nPLEASE CHECK\n", tempo.rdata1, "\n", tempo.rdata)
                        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                    }else{
                        # add the differences in RData $sysinfo into final.output
                        tempo.base1 <- sort(get("final.output", envir = get(env.name))$sys.info$basePkgs)
                        tempo.base2 <- sort(get("output", envir = get(env.name))$sys.info$basePkgs)
                        tempo.other1 <- names(get("final.output", envir = get(env.name))$sys.info$otherPkgs)
                        tempo.other2 <- names(get("output", envir = get(env.name))$sys.info$otherPkgs)
                        tempo.loaded1 <- names(get("final.output", envir = get(env.name))$sys.info$loadedOnly)
                        tempo.loaded2 <- names(get("output", envir = get(env.name))$sys.info$loadedOnly)
                        assign("final.output", {
                            x <- get("final.output", envir = get(env.name))
                            y <- get("output", envir = get(env.name))
                            x$sys.info$basePkgs <- sort(unique(tempo.base1, tempo.base2))
                            if( ! all(tempo.other2 %in% tempo.other1)){
                                x$sys.info$otherPkgs <- c(x$sys.info$otherPkgs, y$sys.info$otherPkgs[ ! (tempo.other2 %in% tempo.other1)])
                                x$sys.info$otherPkgs <- x$sys.info$otherPkgs[order(names(x$sys.info$otherPkgs))]
                            }
                            if( ! all(tempo.loaded2 %in% tempo.loaded1)){
                                x$sys.info$loadedOnly <- c(x$sys.info$loadedOnly, y$sys.info$loadedOnly[ ! (tempo.loaded2 %in% tempo.loaded1)])
                                x$sys.info$loadedOnly <- x$sys.info$loadedOnly[order(names(x$sys.info$loadedOnly))]
                            }
                            x
                        }, envir = get(env.name))
                        # add the differences in RData $sysinfo into final.output
                    }
                }
                file.remove(c(tempo.file, tempo.rdata))
            }
            # combine pdf and save
            if( ! is.null(final.pdf)){
                pdftools::pdf_combine(
                    input = final.pdf,
                    output = paste0(res.path, "/plots_from_arg_test_1-", total.comp.nb, ".pdf")
                )
                file.remove(final.pdf)
            }
            # end combine pdf and save
            # save RData
            assign("output", c(get("final.output", envir = get(env.name)), data = list(final.file)), envir = get(env.name))
            save(output, file = paste0(res.path, "/arg_test_1-", total.comp.nb, ".RData"), envir = get(env.name))
            rm(env.name) # optional, because should disappear at the end of the function execution
            # end save RData
            # save txt
            utils::write.table(final.file, file = paste0(res.path, "/table_from_arg_test_1-", total.comp.nb, ".tsv"), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "")
            # end save txt
            if( ! is.null(expect.error)){
                final.file <- final.file[ ! final.file$problem == final.file$expected.error, ]
                if(nrow(final.file) == 0L){
                    cat(paste0("NO DISCREPANCY BETWEEN EXPECTED AND OBSERVED ERRORS\n\n"))
                }else{
                    cat(paste0("DISCREPANCIES BETWEEN EXPECTED AND OBSERVED ERRORS (SEE THE discrepancy_table_from_arg_test_1-", total.comp.nb, ".tsv FILE)\n\n"))
                    utils::write.table(final.file, file = paste0(res.path, "/discrepancy_table_from_arg_test_1-", total.comp.nb, ".tsv"), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "")
                }
            }
        }
        # end files assembly
    }else{
        # plot management
        if(plot.fun == TRUE){
            grDevices::pdf(file = paste0(res.path, "/plots_from_arg_test_1", ifelse(total.comp.nb == 1L, ".pdf", paste0("-", total.comp.nb, ".pdf"))))
        }else{
            grDevices::pdf(file = NULL) # send plots into a NULL file, no pdf file created
        }
        window.nb <- grDevices::dev.cur()
        invisible(grDevices::dev.set(window.nb))
        # end plot management
        # new environment
        env.name <- paste0("env", ini.time)
        if(exists(env.name, where = -1)){
            tempo.cat <- paste0("ERROR IN ", function.name, ": ENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }else{
            assign(env.name, new.env())
            assign("val", val, envir = get(env.name, envir = sys.nframe(), inherits = FALSE)) # var replaced by val
        }
        # end new environment
        suppressMessages(suppressWarnings(eval(parse(text = code))))
        colnames(data) <- arg
        expect.data <- data.frame()
        if( ! is.null(expect.error)){
            data <- data.frame(data, kind = kind, problem = problem, expected.error = expected.error, message = res, stringsAsFactors = FALSE)
        }else{
            data <- data.frame(data, kind = kind, problem = problem, message = res, stringsAsFactors = FALSE)
        }
        row.names(data) <- paste0("arg_test_", sprintf(paste0("%0", nchar(total.comp.nb), "d"), 1:total.comp.nb))
        sys.info <- utils::sessionInfo()
        sys.info$loadedOnly <- sys.info$loadedOnly[order(names(sys.info$loadedOnly))] # sort the packages
        invisible(grDevices::dev.off(window.nb))
        rm(env.name) # optional, because should disappear at the end of the function execution
        # output
        # warning output
        if( ! is.null(warn)){
            on.exit(warning(paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
        }
        on.exit(expr = options(warning.length = ini.warning.length), add = TRUE)
        # end warning output
        output <- list(fun = fun, ini = ini, data = data, sys.info = sys.info)
        if(plot.fun == TRUE & plot.count == 0L){
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") NO PDF PLOT BECAUSE ONLY ERRORS REPORTED")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            file.remove(paste0(res.path, "/plots_from_arg_test_1", ifelse(total.comp.nb == 1L, ".pdf", paste0("-", total.comp.nb, ".pdf"))))
        }
        if( ! is.null(expect.error)){
            expect.data <- output$data[ ! output$data$problem == output$data$expected.error, ]
            if(nrow(expect.data) == 0L){
                cat(paste0("NO DISCREPANCY BETWEEN EXPECTED AND OBSERVED ERRORS\n\n"))
            }else{
                cat(paste0("DISCREPANCIES BETWEEN EXPECTED AND OBSERVED ERRORS (SEE THE ", if(export == TRUE){paste0("discrepancy_table_from_arg_test_1", ifelse(total.comp.nb == 1L, "", paste0("-", total.comp.nb)), ".tsv FILE")}else{"$data RESULT"}, ")\n\n"))
                if(export == TRUE){
                    expect.data <- as.matrix(expect.data)
                    expect.data <- gsub(expect.data, pattern = "\n", replacement = "  ")
                    utils::write.table(expect.data, file = paste0(res.path, "/discrepancy_table_from_arg_test_1", ifelse(total.comp.nb == 1L, ".tsv", paste0("-", total.comp.nb, ".tsv"))), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "")
                }
            }
        }
        if( ! is.null(warn)){
            base::options(warning.length = 8170)
            on.exit(warning(paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
        }
        on.exit(expr = base::options(warning.length = ini.warning.length), add = TRUE)
        if(export == TRUE){
            save(output, file = paste0(res.path, "/arg_test_1", ifelse(total.comp.nb == 1L, ".RData", paste0("-", total.comp.nb, ".RData"))))
            table.out <- as.matrix(output$data)
            table.out <- gsub(table.out, pattern = "\n", replacement = "  ")
            utils::write.table(table.out, file = paste0(res.path, "/table_from_arg_test_1", ifelse(total.comp.nb == 1L, ".tsv", paste0("-", total.comp.nb, ".tsv"))), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "")
        }else{
            return(output)
        }
        # end output
    }
    # after return() ?
    end.date <- Sys.time()
    end.time <- as.numeric(end.date)
    total.lapse <- round(lubridate::seconds_to_period(end.time - ini.time))
    cat(paste0("test JOB END\n\nTIME: ", end.date, "\n\nTOTAL TIME LAPSE: ", total.lapse, "\n\n\n"))
    # end main code
}
