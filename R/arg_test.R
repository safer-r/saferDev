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
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns
#' One or several pdf if a plotting function is tested and if the plot.fun argument is TRUE. 
#' 
#' And then, if export is FALSE a list containing:
#' 
#' - $fun: the tested function.
#' 
#' - $ini: the initial input values.
#' 
#' - $data: a data frame of all the combination tested, containing the following columns:
#' 
#'      # the different values tested, named by arguments
#'      - $kind: a vector of character strings indicating the kind of test result: either "ERROR", or "WARNING", or "OK".
#'      - $problem: a logical vector indicating if error or not.
#'      
#'      - $expected.error: optional logical vector indicating the expected error specified in the expect.error argument.
#'      - $message: either NULL if $kind is always "OK", or the messages.
#'      
#' - $sys.info: system and packages info.
#' 
#' If export is TRUE: 
#' 
#' - the same list object into a .RData file.
#' 
#' - also the $data data frame into a .tsv file.
#' 
#' - if expect.error is non NULL and if any discrepancy, the $data data frame into a .tsv file but containing only the rows with discrepancies between expected and observed errors.
#' @details 
#' Limited to 43 arguments with at least 2 values each. The total number of arguments tested can be more if the additional arguments have a single value. The limit is due to nested "for" loops (https://stat.ethz.ch/pipermail/r-help/2008-March/157341.html), but this limitation is away from the number of tests performed that would be 2^43.
#' @importFrom lubridate seconds_to_period
#' @importFrom pdftools pdf_combine
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterSplit
#' @importFrom parallel clusterApply
#' @importFrom parallel stopCluster
#' @seealso \code{\link{arg_check}}.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
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
#' # stringsAsFactors = TRUE) ; arg_test(fun = "ggbox", arg = c("data1", "y", "categ"), 
#' # val = list(L1 = list(L1 = obs1), L2 = list(L1 = "Time"), L3 = list(L1 = "Group1")))
#' 
#' # set.seed(1) ; 
#' # obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10), 
#' # stringsAsFactors = TRUE) ; 
#' # arg_test(fun = "ggbox", arg = c("data1", "y", "categ"), val = list(L1 = list(obs1), 
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
        lib.path = NULL,
        safer_check = TRUE
){
    # DEBUGGING
    # fun = "unique" ; arg = "x" ; val = base::list(x = base::list(1:3, mean)) ; expect.error = base::list(x = base::list(TRUE, TRUE)) ; parall = FALSE ; thread.nb = NULL ; plot.fun = FALSE ; export = FALSE ; res.path = "C:\\Users\\gmillot\\Desktop\\" ; lib.path = NULL ; print.count = 1; safer_check = TRUE # for function debugging
    # package name
    package_name <- "saferDev"
    # end package name
    # function name
    ini <- base::match.call(expand.dots = FALSE) # initial parameters (specific of arg_test())
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
            "lubridate::seconds_to_period", 
            "pdftools::pdf_combine",
            "parallel::detectCores",
            "parallel::makeCluster",
            "parallel::clusterSplit",
            "parallel::clusterApply",
            "parallel::stopCluster"
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
        "fun", 
        "arg", 
        "val"
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
    tempo <- saferDev::arg_check(data = fun, class = "vector", mode = "character", length = 1, fun.name = function_name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = arg, class = "vector", mode = "character", fun.name = function_name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = val, class = "list", na.contain = TRUE, fun.name = function_name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(expect.error)){
        tempo <- saferDev::arg_check(data = expect.error, class = "list", fun.name = function_name, safer_check = FALSE) ; base::eval(ee)
    }
    tempo <- saferDev::arg_check(data = parall, class = "vector", mode = "logical", length = 1, fun.name = function_name, safer_check = FALSE) ; base::eval(ee)
    if(parall == TRUE){
        if( ! base::is.null(thread.nb)){
            tempo <- saferDev::arg_check(data = thread.nb, typeof = "integer", double.as.integer.allowed = TRUE, neg.values = FALSE, length = 1, fun.name = function_name, safer_check = FALSE) ; base::eval(ee)
            if(tempo$problem == FALSE & thread.nb < 1){
                tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nthread.nb PARAMETER MUST EQUAL OR GREATER THAN 1: ", thread.nb)
                text.check <- base::c(text.check, tempo.cat)
                argum.check <- base::c(argum.check, TRUE)
            }
        }
    }
    tempo <- saferDev::arg_check(data = print.count, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function_name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = plot.fun, class = "vector", mode = "logical", length = 1, fun.name = function_name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = export, class = "vector", mode = "logical", length = 1, fun.name = function_name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(res.path)){
        tempo <- saferDev::arg_check(data = res.path, class = "vector", mode = "character", fun.name = function_name, safer_check = FALSE) ; base::eval(ee)
    }
    # lib.path already checked above
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    # end argument primary checking

    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg_user_setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg_user_setting) == 0)){
        tempo.arg <- base::names(arg_user_setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = get, env = base::sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & base::lapply(base::lapply(tempo.arg, FUN = get, env = base::sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log, na.rm = TRUE) == TRUE){
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-base::c(
        "fun", 
        "arg", 
        "val", 
        # "expect.erro", # because can be NULL
        "parall", 
        # "thread.nb", # because can be NULL
        "print.count", 
        "plot.fun", 
        "export",
        # "res.path", # because can be NULL
        # "lib.path", # because can be NULL
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
    ini.warning.length <- base::options()$warning.length
    base::options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    # end warning initiation
    # other checkings
    if(base::grepl(x = fun, pattern = "()$")){ # remove ()
        fun <- base::sub(x = fun, pattern = "()$", replacement = "")
    }
    if( ! base::exists(fun)){
        tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nCHARACTER STRING IN fun ARGUMENT DOES NOT EXIST IN THE R WORKING ENVIRONMENT: ", base::paste(fun, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }else if( ! base::all(base::class(base::get(fun)) == "function")){ # here no env = base::sys.nframe(), inherit = FALSE for base::get() because fun is a function in the classical scope
        tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nfun ARGUMENT IS NOT CLASS \"function\" BUT: ", base::paste(base::class(base::get(fun)), collapse = "\n"), "\nCHECK IF ANY CREATED OBJECT WOULD HAVE THE NAME OF THE TESTED FUNCTION")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    if(tempo$problem == FALSE & base::length(arg) == 0L){
        tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\narg ARGUMENT CANNOT BE LENGTH 0")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    for(i2 in 1:base::length(val)){ # base::length(val) must be aequal to nb of arguments
        tempo1 <- saferDev::arg_check(data = val[[i2]], class = "vector", na.contain = TRUE, fun.name = function_name, safer_check = FALSE)
        tempo2 <- saferDev::arg_check(data = val[[i2]], class = "list", na.contain = TRUE, fun.name = function_name, safer_check = FALSE)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nCOMPARTMENT ", i2, " OF val ARGUMENT MUST BE A VECTOR OR A LIST")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }else if(tempo1$problem == FALSE){ # vector split into list compartments
            val[[i2]] <- base::split(x = val[[i2]], f = 1:base::length(val[[i2]])) # convert a vector into list, with each value of the vector in a compartment
        }
    }
    if(base::length(arg) != base::length(val)){
        tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTH OF arg ARGUMENT MUST BE IDENTICAL TO LENGTH OF val ARGUMENT:\nHERE IT IS: ", base::length(arg), " VERSUS ", base::length(val))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    args <- base::names(base::formals(base::get(fun))) # here no env = base::sys.nframe(), inherit = FALSE for base::get() because fun is a function in the classical scope
    if( ! base::all(arg %in% args)){
        tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nSOME OF THE STRINGS IN arg ARE NOT ARGUMENTS OF fun\nfun ARGUMENTS: ", base::paste(args, collapse = " "),"\nPROBLEMATIC STRINGS IN arg: ", base::paste(arg[ ! arg %in% args], collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    if(base::sum(base::sapply(val, FUN = base::length) > 1) > 43){
        tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nCANNOT TEST MORE THAN 43 ARGUMENTS IF THEY ALL HAVE AT LEAST 2 VALUES EACH\nHERE THE NUMBER IS: ", base::sum(base::sapply(val, FUN = base::length) > 1))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "",base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    if( ! base::is.null(expect.error)){
        if(base::length(val) != base::length(expect.error)){
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTH OF val ARGUMENT MUST BE IDENTICAL TO LENGTH OF expect.error ARGUMENT:\nHERE IT IS: ", base::length(val), " VERSUS ", base::length(expect.error))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        for(i3 in 1:base::length(expect.error)){
            tempo1 <- saferDev::arg_check(data = expect.error[[i3]], class = "vector",  mode = "logical", fun.name = function_name, safer_check = FALSE)
            tempo2 <- saferDev::arg_check(data = expect.error[[i3]], class = "list", fun.name = function_name, safer_check = FALSE)
            if(tempo1$problem == TRUE & tempo2$problem == TRUE){
                tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nCOMPARTMENT ", i3, " OF expect.error ARGUMENT MUST BE TRUE OR FALSE")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }else if(tempo1$problem == FALSE){ # vector split into list compartments
                expect.error[[i3]] <- base::split(x = expect.error[[i3]], f = 1:base::length(expect.error[[i3]])) # convert a vector into list, with each value of the vector in a compartment
            }
        }
        for(i2 in 1:base::length(expect.error)){
            if(base::all(base::class(expect.error[[i2]]) == "list")){
                if( ! base::all(base::class(val[[i2]]) == "list")){
                    tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nexpect.error ARGUMENT MUST BE A LIST OF EXACTLY THE SAME STRUCTURE AS val ARGUMENT.\nHERE COMPARTMENT ", i2, " OF expect.error IS CLASS ", base::paste(base::class(expect.error[[i2]]), collapse = " "), "\nAND COMPARTMENT ", i2, " OF val IS CLASS ", base::paste(base::class(val[[i2]]), collapse = " "))
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                }else if(base::length(val[[i2]]) != base::length(expect.error[[i2]])){
                    tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTH OF COMPARTMENT ", i2, " OF val ARGUMENT MUST BE IDENTICAL TO LENGTH OF COMPARTMENT ", i2, " OF expect.error ARGUMENT:\nHERE IT IS: ", base::length(val[[i2]]), " VERSUS ", base::length(expect.error[[i2]]))
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                }
            }
        }
    }
    if( ! base::is.null(res.path)){
        if( ! base::all(base::dir.exists(res.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and res.path == NA
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE res.path ARGUMENT DOES NOT EXISTS:\n", base::paste(res.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
    }
    if(parall == TRUE & base::is.null(res.path)){
        tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nres.path ARGUMENT MUST BE SPECIFIED IF parall ARGUMENT IS TRUE")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    if(base::is.null(res.path) & export == TRUE){
        tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nres.path ARGUMENT MUST BE SPECIFIED IF export ARGUMENT TRUE")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    if(parall == TRUE & export == FALSE){
        export <- TRUE
        tempo.cat <- base::paste0("WARNING FROM ", function_name, " OF THE ", package_name, " PACKAGE\nexport ARGUMENT CONVERTED TO TRUE BECAUSE thread.nb ARGUMENT IS NOT NULL")
        base::warning(base::paste0("\n", tempo.cat, "\n"), call. = FALSE)
    }
    if( ! base::is.null(lib.path)){
        if( ! base::all(base::dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
    }
    # end other checkings
    # end second round of checking and data preparation

    # main code
    # declaration of special plot functions
    sp.plot.fun <- base::c("gg_scatter", "gg_bar", "gg_boxplot")
    # end declaration of special plot functions
    # new environment
    env.name <- base::paste0("env", base::as.numeric(base::Sys.time()))
    if(base::exists(env.name, where = -1)){ # verify if still ok when this function is inside a function
        tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }else{
        base::assign(env.name, base::new.env())
        base::assign("data", data, envir = base::get(env.name, envir = base::sys.nframe(), inherits = FALSE)) # data assigned in a new envir for test
    }
    # end new environment

    base::cat("\ntest JOB IGNITION\n")
    ini.date <- base::Sys.time()
    ini.time <- base::as.numeric(ini.date) # time of process begin, converted into seconds
    if(base::is.null(res.path)){
        res.path <- "."
    }
    if(export == TRUE){
        res.path <- base::paste0(res.path, "/arg_test_res_", base::trunc(ini.time))
        if(base::dir.exists(res.path)){
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nFOLDER ALREADY EXISTS\n", res.path, "\nPLEASE RERUN ONCE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }else{
            base::dir.create(res.path)
        }
    }
    total.comp.nb <- base::prod(base::sapply(val, FUN = "length"))
    base::cat(base::paste0("\nTOTAL NUMBER OF TESTS: ", total.comp.nb, "\n"))
    # creation of the txt instruction that includes several loops
    loop.string <- NULL
    end.loop.string <- NULL
    fun.args <- NULL
    fun.args2 <- NULL
    error.values <- NULL
    arg.values <- "base::list("
    for(i1 in 1:base::length(arg)){
        if(parall == FALSE){
            if(base::length(val[[i1]]) > 1){ # loop only if more than one value in base::length(val[[i1]])
                loop.string <- base::paste0(loop.string, "for(i", i1, " in 1:", base::length(val[[i1]]), "){")
                end.loop.string <- base::paste0(end.loop.string, "}")
            }
        }else{
            loop.string <- "for(i in x){"
            end.loop.string <- "}"
        }
        fun.args <- base::paste0(
            fun.args, 
            base::ifelse(i1 == 1L, "", ", "), 
            arg[i1], 
            " = val[[", 
            i1, 
            "]][[", 
            if(parall == FALSE){
                if(base::length(val[[i1]]) > 1){
                    base::paste0("i", i1)
                }else{
                    "1" # a unique element in val[[i1]]
                }
            }else{
                base::paste0("i.list[[", i1, "]][i]")
            }, 
            "]]"
        )
        fun.args2 <- base::paste0(
            fun.args2, 
            base::ifelse(i1 == 1L, "", ", "), 
            arg[i1], 
            " = val[[", 
            i1, 
            "]][[', ", 
            if(parall == FALSE){
                if(base::length(val[[i1]]) > 1){
                    base::paste0("i", i1)
                }else{
                    "1" # a unique element in val[[i1]]
                }
            }else{
                base::paste0("i.list[[", i1, "]][i]")
            }, 
            ", ']]"
        )
        arg.values <- base::paste0(
            arg.values, 
            "val[[", i1, "]][[", 
            if(parall == FALSE){
                if(base::length(val[[i1]]) > 1){
                    base::paste0("i", i1)
                }else{
                    "1" # a unique element in val[[i1]]
                }
            }else{
                base::paste0("i.list[[", i1, "]][i]")
            }, 
            "]]", 
            base::ifelse(i1 == base::length(arg), "", ", ")
        )
        error.values <- base::paste0(
            error.values, 
            base::ifelse(i1 == 1L, "", " | "), 
            "expect.error[[", i1, "]][[", 
            if(parall == FALSE){
                if(base::length(expect.error[[i1]]) > 1){
                    base::paste0("i", i1)
                }else{
                    "1" # a unique element in expect.error[[i1]]
                }
            }else{
                base::paste0("i.list[[", i1, "]][i]")
            }, 
            "]]"
        )
    }
    arg.values <- base::paste0(arg.values, ")")
    fun.test <- base::paste0(fun, "(", fun.args, ")")
    fun.test2 <- base::paste0("base::paste0('", fun, "(", fun.args2, ")')")
    # plot title for special plot functions
    if(plot.fun == TRUE){
        plot.kind <- "classic"
        if(fun %in% sp.plot.fun){
            plot.kind <- "special"
            if(base::any(arg %in% "title")){ # this is for the special functions
                tempo.match <- base::regmatches(x = fun.test, m = base::regexpr(text = fun.test, pattern = "title = .+[,)]"))
                tempo.match <- base::substring(tempo.match , 1, base::nchar(tempo.match) - 1)
                fun.test <- base::sub(x = fun.test, pattern = tempo.match, replacement = base::paste0(tempo.match, "\ntempo.title"))
            }else{
                fun.test <- base::sub(x = fun.test, pattern = ")$", replacement = ", title = tempo.title)")
            }
        }
    }
    # end plot title for special plot functions
    kind <- base::character()
    problem <- base::logical()
    expected.error <- base::logical()
    res <- base::character()
    count <- 0
    print.count.loop <- 0
    plot.count <- 0
    if(base::length(arg) == 1L){
        data <- base::data.frame()
    }else{ # base::length(arg) == 0L already tested above
        data <- base::data.frame(base::t(base::vector("character", base::length(arg))), stringsAsFactors = FALSE)[-1, ] # -1 to remove the single row created and to have an empty data frame with base::length(arg) columns
    }
    code <- base::paste(
        loop.string, '
            count <- count + 1
            print.count.loop <- print.count.loop + 1
            arg.values.print <- base::eval(base::parse(text = arg.values)) # recover the list of the i1 compartment
            for(j3 in 1:base::length(arg.values.print)){ # WARNING: do not use i1, i2 etc., here because already in loop.string
                tempo.capt <- utils::capture.output(tempo.error <- saferDev::get_message(data =  base::paste0("base::paste(arg.values.print[[", j3, "]])"), kind = "error", header = FALSE, print.no = FALSE, env = base::get(env.name, envir = base::sys.nframe(), inherits = FALSE), safer_check = FALSE)) # collapsing arg.values sometimes does not work (with function for instance)
                if( ! base::is.null(tempo.error)){
                    arg.values.print[[j3]] <- base::paste0("SPECIAL VALUE OF CLASS ", base::class(arg.values.print[[j3]]), " AND TYPE ", base::typeof(arg.values.print[[j3]]))
                }
            }
            data <- base::rbind(data, base::as.character(base::sapply(arg.values.print, FUN = "paste", collapse = " ")), stringsAsFactors = FALSE) # each colum is a test
            tempo.capt <- utils::capture.output(tempo.try.error <- saferDev::get_message(data = base::eval(base::parse(text = fun.test2)), kind = "error", header = FALSE, print.no = FALSE, env = base::get(env.name, envir = base::sys.nframe(), inherits = FALSE),safer_check = FALSE)) # data argument needs a character string but base::eval(base::parse(text = fun.test2)) provides it (base::eval base::parse replace the i1, i2, etc., by the correct values, meaning that only val is required in the env.name environment)
            tempo.capt <- utils::capture.output(tempo.try.warning <- saferDev::get_message(data = base::eval(base::parse(text = fun.test2)), kind = "warning", header = FALSE, env = base::get(env.name, envir = base::sys.nframe(), inherits = FALSE), print.no = FALSE, safer_check = FALSE)) # data argument needs a character string but base::eval(base::parse(text = fun.test2)) provides it (base::eval base::parse replace the i1, i2, etc., by the correct values, meaning that only val is required in the env.name environment)
            if( ! base::is.null(expect.error)){
                expected.error <- base::c(expected.error, base::eval(base::parse(text = error.values)))
            }
            if( ! base::is.null(tempo.try.error)){
                kind <- base::c(kind, "ERROR")
                problem <- base::c(problem, TRUE)
                res <- base::c(res, tempo.try.error)
            }else{
                if( ! base::is.null(tempo.try.warning)){
                    kind <- base::c(kind, "WARNING")
                    problem <- base::c(problem, FALSE)
                    res <- base::c(res, tempo.try.warning)
                }else{
                    kind <- base::c(kind, "OK")
                    problem <- base::c(problem, FALSE)
                    res <- base::c(res, "")
                }
                if(plot.fun == TRUE){
                    base::invisible(grDevices::dev.set(window.nb))
                    plot.count <- plot.count + 1
                    tempo.title <- base::paste0("test_", base::sprintf(base::paste0("%0", base::nchar(total.comp.nb), "d"), base::ifelse(parall == FALSE, count, x[count])))
                    if(plot.kind == "classic"){ # not ggplot. So title has to be added in a classical way
                        # graphics::par(ann=FALSE, xaxt="n", yaxt="n", mar = base::rep(1, 4), bty = "n", xpd = NA) # old
                        graphics::par(bty = "n", xpd = NA) # new
                        base::eval(base::parse(text = fun.test))
                        # base::plot(1, 1, type = "n") # no display with type = "n"
                        x.left.dev.region <- (graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / ((graphics::par("omd")[2] - graphics::par("omd")[1]) * (graphics::par("plt")[2] - graphics::par("plt")[1]))) * graphics::par("omd")[1])
                        y.top.dev.region <- (graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par("omd")[4] - graphics::par("omd")[3]) * (graphics::par("plt")[4] - graphics::par("plt")[3]))) * (1 - graphics::par("omd")[4]))
                        graphics::text(x = x.left.dev.region, y = y.top.dev.region, labels = tempo.title, adj=base::c(0, 1), cex = 1.5)
                    }else if(plot.kind == "special"){ # ggplot. title has been added above
                        base::eval(base::parse(text = fun.test))
                    }else{
                        tempo.cat <- base::paste0("INTERNAL CODE ERROR 1 IN ", function_name, " OF THE ", package_name, " PACKAGE\nCODE HAS TO BE MODIFIED")
                        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
                    }
                }
            }
            if(print.count.loop == print.count){
                print.count.loop <- 0
                tempo.time <- base::as.numeric(base::Sys.time())
                tempo.lapse <- base::round(lubridate::seconds_to_period(tempo.time - ini.time))
                final.loop <- (tempo.time - ini.time) / count * base::ifelse(parall == FALSE, total.comp.nb, base::length(x)) # expected duration in seconds # intra nb.compar loop lapse: time lapse / cycles done * cycles remaining
                final.exp <- base::as.POSIXct(final.loop, origin = ini.date)
                base::cat(base::paste0(base::ifelse(parall == FALSE, "\n", base::paste0("\nIN PROCESS ", process.id, " | ")), "LOOP ", base::format(count, big.mark=","), " / ", base::format(base::ifelse(parall == FALSE, total.comp.nb, base::length(x)), big.mark=","), " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp))
            }
            if(count == base::ifelse(parall == FALSE, total.comp.nb, base::length(x))){
                tempo.time <- base::as.numeric(base::Sys.time())
                tempo.lapse <- base::round(lubridate::seconds_to_period(tempo.time - ini.time))
                base::cat(base::paste0(base::ifelse(parall == FALSE, "\nLOOP PROCESS ENDED | ", base::paste0("\nPROCESS ", process.id, " ENDED | ")), "LOOP ", base::format(count, big.mark=","), " / ", base::format(base::ifelse(parall == FALSE, total.comp.nb, base::length(x)), big.mark=","), " | TIME SPENT: ", tempo.lapse, "\n\n"))
            }
        ', 
        end.loop.string
    )
    # end creation of the txt instruction that includes several loops
    if(parall == TRUE){
        # list of i numbers that will be split
        i.list <- base::vector("list", base::length(val)) # positions to split in parallel jobs
        for(i2 in 1:base::length(arg)){
            if(i2 == 1L){
                tempo.divisor <- total.comp.nb / base::length(val[[i2]])
                i.list[[i2]] <- base::rep(1:base::length(val[[i2]]), each = base::as.integer(tempo.divisor))
                tempo.multi <- base::length(val[[i2]])
            }else{
                tempo.divisor <- tempo.divisor / base::length(val[[i2]])
                i.list[[i2]] <- base::rep(base::rep(1:base::length(val[[i2]]), each = base::as.integer(tempo.divisor)), time = base::as.integer(tempo.multi))
                tempo.multi <- tempo.multi * base::length(val[[i2]])
            }
        }
        # end list of i numbers that will be split
        tempo.cat <- base::paste0("PARALLELIZATION INITIATED AT: ", ini.date)
        base::cat(base::paste0("\n", tempo.cat, "\n"))
        tempo.thread.nb = parallel::detectCores(all.tests = FALSE, logical = TRUE) # detect the number of threads
        if(base::is.null(thread.nb)){
            thread.nb <- tempo.thread.nb
        }else if(tempo.thread.nb < thread.nb){
            thread.nb <- tempo.thread.nb
        }
        tempo.cat <- base::paste0("NUMBER OF THREADS USED: ", thread.nb)
        base::cat(base::paste0("\n    ", tempo.cat, "\n"))
        Clust <- parallel::makeCluster(thread.nb, outfile = base::paste0(res.path, "/test_parall_log.txt")) # outfile to print or cat during parallelization (only possible in a file, outfile = "" do not work on windows)
        tempo.cat <- base::paste0("SPLIT OF TEST NUMBERS IN PARALLELISATION:")
        base::cat(base::paste0("\n    ", tempo.cat, "\n"))
        cluster.list <- parallel::clusterSplit(Clust, 1:total.comp.nb) # split according to the number of cluster
        utils::str(cluster.list) # using base::print(utils::str()) add a NULL below the result
        base::cat("\n")
        paral.output.list <- parallel::clusterApply( # paral.output.list is a list made of thread.nb compartments, each made of n / thread.nb (mat theo column number) compartment. Each compartment receive the corresponding results of this function
            cl = Clust,
            x = cluster.list,
            function_name = function_name, 
            package_name = package_name, 
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
        function_name, 
        package_name, 
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
                process.id <- base::Sys.getpid()
                base::cat(base::paste0("\nPROCESS ID ", process.id, " -> TESTS ", x[1], " TO ", x[base::length(x)], "\n"))
                saferDev:::.pack_and_function_check(
                    fun = base::c(
                        "lubridate::seconds_to_period"
                    ),
                    lib.path = lib.path,
                    external_function_name = function_name, 
                    external_package_name = package_name
                )
                # end check again: very important because another R
                # plot management
                if(plot.fun == TRUE){
                    grDevices::pdf(file = base::paste0(res.path, "/plots_from_test_", x[1], base::ifelse(base::length(x) == 1L, ".pdf", base::paste0("-", x[base::length(x)], ".pdf"))))
                }else{
                    grDevices::pdf(file = NULL) # send plots into a NULL file, no pdf file created
                }
                window.nb <- grDevices::dev.cur()
                base::invisible(grDevices::dev.set(window.nb))
                # end plot management
                # new environment
                ini.date <- base::Sys.time()
                ini.time <- base::as.numeric(ini.date) # time of process begin, converted into 
                env.name <- base::paste0("env", ini.time)
                if(base::exists(env.name, where = -1)){ # verify if still ok when arg_test() is inside a function
                    tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                }else{
                    base::assign(env.name, base::new.env())
                    base::assign("val", val, envir = base::get(env.name, envir = base::sys.nframe(), inherits = FALSE)) # var replaced by val
                }
                # end new environment
                print.count.loop <- 0
                base::suppressMessages(base::suppressWarnings(base::eval(base::parse(text = code))))
                base::colnames(data) <- arg
                if( ! base::is.null(expect.error)){
                    data <- base::data.frame(data, kind = kind, problem = problem, expected.error = expected.error, message = res, stringsAsFactors = FALSE)
                }else{
                    data <- base::data.frame(data, kind = kind, problem = problem, message = res, stringsAsFactors = FALSE)
                }
                base::row.names(data) <- base::paste0("arg_test_", base::sprintf(base::paste0("%0", base::nchar(total.comp.nb), "d"), x))
                sys.info <- utils::sessionInfo()
                sys.info$loadedOnly <- sys.info$loadedOnly[base::order(base::names(sys.info$loadedOnly))] # sort the packages
                base::invisible(grDevices::dev.off(window.nb))
                base::rm(env.name) # optional, because should disappear at the end of the function execution
                # output
                output <- base::list(fun = fun, ini = ini, data = data, sys.info = sys.info)
                base::save(output, file = base::paste0(res.path, "/arg_test_", x[1], base::ifelse(base::length(x) == 1L, ".RData", base::paste0("-", x[base::length(x)], ".RData"))))
                if(plot.fun == TRUE & plot.count == 0L){
                    warn.count <- warn.count + 1
                    tempo.warn <- base::paste0("(", warn.count,") IN PROCESS ", process.id, ": NO PDF PLOT BECAUSE ONLY ERRORS REPORTED")
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
                    base::file.remove(base::paste0(res.path, "/plots_from_arg_test_", x[1], base::ifelse(base::length(x) == 1L, ".pdf", base::paste0("-", x[base::length(x)], ".pdf"))))
                }
                table.out <- base::as.matrix(data)
                # table.out[table.out == ""] <- " " # does not work # because otherwise utils::read.table() converts "" into NA
                table.out <- base::gsub(table.out, pattern = "\n", replacement = " ")
                utils::write.table(table.out, file = base::paste0(res.path, "/table_from_arg_test_", x[1], base::ifelse(base::length(x) == 1L, ".tsv", base::paste0("-", x[base::length(x)], ".tsv"))), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "")
            }
        )
        parallel::stopCluster(Clust)
        # files assembly
        if(base::length(cluster.list) > 1){
            for(i2 in 1:base::length(cluster.list)){
                tempo.file <- base::paste0(res.path, "/table_from_arg_test_", base::min(cluster.list[[i2]], na.rm = TRUE), base::ifelse(base::length(cluster.list[[i2]]) == 1L, ".tsv", base::paste0("-", base::max(cluster.list[[i2]], na.rm = TRUE), ".tsv"))) # txt file
                tempo <- utils::read.table(file = tempo.file, header = TRUE, stringsAsFactors = FALSE, sep = "\t", row.names = 1, comment.char = "", colClasses = "character") #  row.names = 1 (1st column) because now utils::read.table() adds a NA in the header if the header starts by a tabulation, comment.char = "" because colors with #, colClasses = "character" otherwise convert "" (from NULL) into NA
                if(base::file.exists(base::paste0(res.path, "/plots_from_arg_test_", base::min(cluster.list[[i2]], na.rm = TRUE), base::ifelse(base::length(cluster.list[[i2]]) == 1L, ".pdf", base::paste0("-", base::max(cluster.list[[i2]], na.rm = TRUE), ".pdf"))))){
                    tempo.pdf <- base::paste0(res.path, "/plots_from_arg_test_", base::min(cluster.list[[i2]], na.rm = TRUE), base::ifelse(base::length(cluster.list[[i2]]) == 1L, ".pdf", base::paste0("-", base::max(cluster.list[[i2]], na.rm = TRUE), ".pdf"))) # pdf file
                }else{
                    tempo.pdf <- NULL
                }
                tempo.rdata <- base::paste0(res.path, "/arg_test_", base::min(cluster.list[[i2]], na.rm = TRUE), base::ifelse(base::length(cluster.list[[i2]]) == 1L, ".RData", base::paste0("-", base::max(cluster.list[[i2]], na.rm = TRUE), ".RData"))) # RData file
                if(i2 == 1L){
                    final.file <- tempo
                    final.pdf <- tempo.pdf
                    # new env for RData combining
                    env.name <- base::paste0("env", ini.time)
                    if(base::exists(env.name, where = -1)){ # verify if still ok when this function is inside a function
                        tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
                        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                        # end new env for RData combining
                    }else{
                        base::assign(env.name, base::new.env())
                        base::load(tempo.rdata, envir = base::get(env.name))
                        tempo.rdata1 <- tempo.rdata
                        base::assign("final.output", base::get("output", envir = base::get(env.name)), envir = base::get(env.name))
                    }
                }else{
                    final.file <- base::rbind(final.file, tempo, stringsAsFactors = TRUE)
                    final.pdf <- base::c(final.pdf, tempo.pdf)
                    base::load(tempo.rdata, envir = base::get(env.name))
                    if( ! base::identical(base::get("final.output", envir = base::get(env.name))[base::c("R.version", "locale", "platform")], base::get("output", envir = base::get(env.name))[base::c("R.version", "locale", "platform")])){
                        tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nDIFFERENCE BETWEEN OUTPUTS WHILE THEY SHOULD BE IDENTICAL\nPLEASE CHECK\n", tempo.rdata1, "\n", tempo.rdata)
                        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                    }else{
                        # add the differences in RData $sysinfo into final.output
                        tempo.base1 <- base::sort(base::get("final.output", envir = base::get(env.name))$sys.info$basePkgs)
                        tempo.base2 <- base::sort(base::get("output", envir = base::get(env.name))$sys.info$basePkgs)
                        tempo.other1 <- base::names(base::get("final.output", envir = base::get(env.name))$sys.info$otherPkgs)
                        tempo.other2 <- base::names(base::get("output", envir = base::get(env.name))$sys.info$otherPkgs)
                        tempo.loaded1 <- base::names(base::get("final.output", envir = base::get(env.name))$sys.info$loadedOnly)
                        tempo.loaded2 <- base::names(base::get("output", envir = base::get(env.name))$sys.info$loadedOnly)
                        base::assign("final.output", {
                            x <- base::get("final.output", envir = base::get(env.name))
                            y <- base::get("output", envir = base::get(env.name))
                            x$sys.info$basePkgs <- base::sort(base::unique(tempo.base1, tempo.base2))
                            if( ! base::all(tempo.other2 %in% tempo.other1)){
                                x$sys.info$otherPkgs <- base::c(x$sys.info$otherPkgs, y$sys.info$otherPkgs[ ! (tempo.other2 %in% tempo.other1)])
                                x$sys.info$otherPkgs <- x$sys.info$otherPkgs[base::order(base::names(x$sys.info$otherPkgs))]
                            }
                            if( ! base::all(tempo.loaded2 %in% tempo.loaded1)){
                                x$sys.info$loadedOnly <- base::c(x$sys.info$loadedOnly, y$sys.info$loadedOnly[ ! (tempo.loaded2 %in% tempo.loaded1)])
                                x$sys.info$loadedOnly <- x$sys.info$loadedOnly[base::order(base::names(x$sys.info$loadedOnly))]
                            }
                            x
                        }, envir = base::get(env.name))
                        # add the differences in RData $sysinfo into final.output
                    }
                }
                base::file.remove(base::c(tempo.file, tempo.rdata))
            }
            # combine pdf and save
            if( ! base::is.null(final.pdf)){
                pdftools::pdf_combine(
                    input = final.pdf,
                    output = base::paste0(res.path, "/plots_from_arg_test_1-", total.comp.nb, ".pdf")
                )
                base::file.remove(final.pdf)
            }
            # end combine pdf and save
            # save RData
            base::assign("output", base::c(base::get("final.output", envir = base::get(env.name)), data = base::list(final.file)), envir = base::get(env.name))
            base::save(output, file = base::paste0(res.path, "/arg_test_1-", total.comp.nb, ".RData"), envir = base::get(env.name))
            base::rm(env.name) # optional, because should disappear at the end of the function execution
            # end save RData
            # save txt
            utils::write.table(final.file, file = base::paste0(res.path, "/table_from_arg_test_1-", total.comp.nb, ".tsv"), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "")
            # end save txt
            if( ! base::is.null(expect.error)){
                final.file <- final.file[ ! final.file$problem == final.file$expected.error, ]
                if(base::nrow(final.file) == 0L){
                    base::cat(base::paste0("NO DISCREPANCY BETWEEN EXPECTED AND OBSERVED ERRORS\n\n"))
                }else{
                    base::cat(base::paste0("DISCREPANCIES BETWEEN EXPECTED AND OBSERVED ERRORS (SEE THE discrepancy_table_from_arg_test_1-", total.comp.nb, ".tsv FILE)\n\n"))
                    utils::write.table(final.file, file = base::paste0(res.path, "/discrepancy_table_from_arg_test_1-", total.comp.nb, ".tsv"), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "")
                }
            }
        }
        # end files assembly
    }else{
        # plot management
        if(plot.fun == TRUE){
            grDevices::pdf(file = base::paste0(res.path, "/plots_from_arg_test_1", base::ifelse(total.comp.nb == 1L, ".pdf", base::paste0("-", total.comp.nb, ".pdf"))))
        }else{
            grDevices::pdf(file = NULL) # send plots into a NULL file, no pdf file created
        }
        window.nb <- grDevices::dev.cur()
        base::invisible(grDevices::dev.set(window.nb))
        # end plot management
        # new environment
        env.name <- base::paste0("env", ini.time)
        if(base::exists(env.name, where = -1)){
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }else{
            base::assign(env.name, base::new.env())
            base::assign("val", val, envir = base::get(env.name, envir = base::sys.nframe(), inherits = FALSE)) # var replaced by val
        }
        # end new environment
        base::suppressMessages(base::suppressWarnings(base::eval(base::parse(text = code))))
        base::colnames(data) <- arg
        expect.data <- base::data.frame()
        if( ! base::is.null(expect.error)){
            data <- base::data.frame(data, kind = kind, problem = problem, expected.error = expected.error, message = res, stringsAsFactors = FALSE)
        }else{
            data <- base::data.frame(data, kind = kind, problem = problem, message = res, stringsAsFactors = FALSE)
        }
        base::row.names(data) <- base::paste0("arg_test_", base::sprintf(base::paste0("%0", base::nchar(total.comp.nb), "d"), 1:total.comp.nb))
        sys.info <- utils::sessionInfo()
        sys.info$loadedOnly <- sys.info$loadedOnly[base::order(base::names(sys.info$loadedOnly))] # sort the packages
        base::invisible(grDevices::dev.off(window.nb))
        base::rm(env.name) # optional, because should disappear at the end of the function execution
        if(plot.fun == TRUE & plot.count == 0L){
            warn.count <- warn.count + 1
            tempo.warn <- base::paste0("(", warn.count,") NO PDF PLOT BECAUSE ONLY ERRORS REPORTED")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
            base::file.remove(base::paste0(res.path, "/plots_from_arg_test_1", base::ifelse(total.comp.nb == 1L, ".pdf", base::paste0("-", total.comp.nb, ".pdf"))))
        }
        # output
        output <- base::list(fun = fun, ini = ini, data = data, sys.info = sys.info)
        if( ! base::is.null(expect.error)){
            expect.data <- output$data[ ! output$data$problem == output$data$expected.error, ]
            if(base::nrow(expect.data) == 0L){
                base::cat(base::paste0("NO DISCREPANCY BETWEEN EXPECTED AND OBSERVED ERRORS\n\n"))
            }else{
                base::cat(base::paste0("DISCREPANCIES BETWEEN EXPECTED AND OBSERVED ERRORS (SEE THE ", if(export == TRUE){base::paste0("discrepancy_table_from_arg_test_1", base::ifelse(total.comp.nb == 1L, "", base::paste0("-", total.comp.nb)), ".tsv FILE")}else{"$data RESULT"}, ")\n\n"))
                if(export == TRUE){
                    expect.data <- base::as.matrix(expect.data)
                    expect.data <- base::gsub(expect.data, pattern = "\n", replacement = "  ")
                    utils::write.table(expect.data, file = base::paste0(res.path, "/discrepancy_table_from_arg_test_1", base::ifelse(total.comp.nb == 1L, ".tsv", base::paste0("-", total.comp.nb, ".tsv"))), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "")
                }
            }
        }
        if(export == TRUE){
            base::save(output, file = base::paste0(res.path, "/arg_test_1", base::ifelse(total.comp.nb == 1L, ".RData", base::paste0("-", total.comp.nb, ".RData"))))
            table.out <- base::as.matrix(output$data)
            table.out <- base::gsub(table.out, pattern = "\n", replacement = "  ")
            utils::write.table(table.out, file = base::paste0(res.path, "/table_from_arg_test_1", base::ifelse(total.comp.nb == 1L, ".tsv", base::paste0("-", total.comp.nb, ".tsv"))), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "")
        }else{
            base::return(output)
        }
        # end output
    }
    # warning output
    if( ! base::is.null(warn)){
        base::on.exit(base::warning(base::paste0("FROM ", function_name, ":\n\n", warn), call. = FALSE))
    }
    base::on.exit(expr = base::options(warning.length = ini.warning.length), add = TRUE)
    # end warning output
    end.date <- base::Sys.time()
    end.time <- base::as.numeric(end.date)
    total.lapse <- base::round(lubridate::seconds_to_period(end.time - ini.time))
    base::cat(base::paste0("test JOB END\n\nTIME: ", end.date, "\n\nTOTAL TIME LAPSE: ", total.lapse, "\n\n\n"))
    # end main code
}
