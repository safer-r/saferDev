#' @title arg_check
#' @description
#' Check expected values of an argument of functions: class, type, mode, length, restricted values panel, kind of numeric values in addition to the distinction between 'integer' and 'double' (proportion only? Inf values authorized? negative values authorized? Integers of type 'double'?)
#' @param data Object to test.
#' @param class Single character string. Either one of the class() result or "vector" or "ggplot2" (i.e., objects of class c("gg", "ggplot")) or NULL. See the warning section below.
#' @param typeof Single character string. Either one of the typeof() result or NULL.
#' @param mode Single character string. Either one of the mode() result (for non-vector object) or NULL.
#' @param length Single numeric value indicating the length of the object. Not considered if NULL.
#' @param prop Single logical value. Are the numeric values between 0 and 1 (proportion)? If TRUE, can be used alone, without considering class, etc.
#' @param double.as.integer.allowed Single logical value. If TRUE, no error is reported in the cheking message if argument is set to typeof == "integer" or class == "integer", while the reality is typeof == "double" or class == "numeric" but the numbers strictly have zero as modulo (remainder of a division). This means that i <- 1, which is typeof(i) -> "double" is considered as integer with double.as.integer.allowed = TRUE. WARNING: data mod 1 == 0L but not isTRUE(all.equal(data mod 1, 0)) is used here because the argument checks for integers stored as double (does not check for decimal numbers that are approximate integers).
#' @param options Vector of character strings or integers indicating all the possible option values for the data argument, or NULL. Numbers of type "double" are accepted if they have a 0 modulo.
#' @param all.options.in.data Single logical value. If TRUE, all of the options must be present at least once in the data argument, and nothing else. If FALSE, some or all of the options must be present in the data argument, and nothing else. Ignored if options is NULL.
#' @param na.contain Single logical value. Can the data argument contain NA?
#' @param neg.values Single logical value. Are negative numeric values authorized? Warning: the default setting is TRUE, meaning that, in that case, no check is performed for the presence of negative values. The neg.values argument is activated only when set to FALSE. In addition, (1) neg.values = FALSE can only be used when class, typeof or mode arguments are not NULL, otherwise return an error message, (2) the presence of negative values is not checked with neg.values = FALSE if the tested object is a factor and the following checking message is returned "OBJECT MUST BE MADE OF NON NEGATIVE VALUES BUT IS A FACTOR".
#' @param inf.values Single logical value. Are infinite numeric values authorized (Inf or -Inf)? Identical remarks as for the neg.values argument.
#' @param print Single logical value. Print the message if $problem is TRUE? Warning: set by default to FALSE, which facilitates the control of the checking message output when using arg_check() inside functions. See the example section.
#' @param data.name Single character string indicating the name of the object to test. If NULL, use what is assigned to the data argument for the returned message.
#' @param fun.name Single character string indicating the name of the function checked (i.e., when arg_check() is used to check the arguments of this function). If non-null, the value of fun.name will be added into the message returned by arg_check().
#' @returns 
#' A list containing:
#' 
#'- §problem: logical. Is there any problem detected?
#' 
#'- §text: message indicating the details of the problem, or the absence of problem.
#' 
#' - §object.name: value of the data.name argument (i.e., name of the checked object if provided, NULL otherwise).
#' @details
#' - If options == NULL, then at least class or type or mode or length argument must be non-null.
#'  
#' - If options is non-null, then class, type and mode must be NULL, and length can be NULL or specified.
#'  
#' - The function tests what is written in its arguments, even if what is written is incoherent. For instance, arg_check(data = factor(1), class = "factor", mode = "character") will return a problem, whatever the object tested in the data argument, because no object can be class "factor" and mode "character" (factors are class "factor" and mode "numeric"). Of note, length of object of class "environment" is always 0.
#'  
#' - If the tested object is NULL, then the function will always return a checking problem.
#'  
#' - Argument "class" with value "vector" means that the object is tested for class(data) returning only "numeric", "integer", "character", "logical", "complex" or "expression". Please, use another value of class (e.g., class = "call" or class = "list") for other types and class of objects
#'  
#' - Since R >= 4.0.0, class(matrix()) returns "matrix" "array", and not "matrix" alone as before. However, use argument class = "matrix" to check for matrix object (of class "matrix" "array" in R >= 4.0.0) and use argument class = "array" to check for array object (of class "array" in R >= 4.0.0).
#' @examples
#' test <- matrix(1:3)
#' # arg_check(data = test, print = TRUE, class = "vector", mode = "numeric")  # commented because this example returns an error
#' @export
arg_check <- function(
        data, 
        class = NULL, 
        typeof = NULL, 
        mode = NULL, 
        length = NULL, 
        prop = FALSE, 
        double.as.integer.allowed = FALSE, 
        options = NULL, 
        all.options.in.data = FALSE, 
        na.contain = FALSE, 
        neg.values = TRUE, 
        inf.values = TRUE, 
        print = FALSE, 
        data.name = NULL, 
        fun.name = NULL
){
    # DEBUGGING
    # data = mean ; class = NULL ; typeof = NULL ; mode = NULL ; length = NULL ; prop = FALSE ; double.as.integer.allowed = FALSE ; options = "a" ; all.options.in.data = FALSE ; na.contain = FALSE ; neg.values = TRUE ; inf.values = TRUE ; print = TRUE ; data.name = NULL ; fun.name = NULL
    # package name
    package.name <- "saferDev"
    # end package name
    # function name
    # no used in this function for the error message, to avoid env colliding
    # end function name
    # critical operator checking
    .base_op_check(external.function.name = "arg_check()")
    # end critical operator checking
    # check of lib.path
    # end check of lib.path
    # saferdev required function checking
    # end saferdev required function checking
    # check of the required function from the required packages
    # end check of the required function from the required packages

    # argument primary checking
    # fun.name checked first because required next
    if( ! base::is.null(fun.name)){ # I have to use this way to deal with every kind of class for fun.name
        if(base::all(base::class(fun.name) == "character")){ # base::all() without na.rm -> ok because class(NA) is "logical"
            if(base::length(fun.name) != 1){
                tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE: THE fun.name ARGUMENT MUST BE A CHARACTER VECTOR OF LENGTH 1: ", base::paste(fun.name, collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else if(base::any(base::is.na(fun.name))){ # normally no NA with is.na()
                tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE: NO ARGUMENT EXCEPT data AND options CAN HAVE NA VALUES\nPROBLEMATIC ARGUMENT IS fun.name")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
        }else{
            tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE: THE fun.name ARGUMENT MUST BE A CHARACTER VECTOR OF LENGTH 1") # paste(fun.name, collapse = " ") removed here because does not work with objects like function
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end fun.name checked first because required next
    # arg with no default values
    mandat.args <- base::c(
        "data"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat.args, collapse = "), base::missing("), "))")))
    if(base::any(tempo)){ # normally no NA for base::missing() output
        tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": FOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args[tempo], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
    # end argument primary checking

    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of special classes
    basic.class <- base::c(
        "NULL", # because class(NULL) is "NULL". The NULL aspect will be dealt later
        "logical", 
        "integer", 
        "numeric", 
        # "complex", 
        "character"
        # "matrix", 
        # "array", 
        # "data.frame", 
        # "list", 
        # "factor", 
        # "table", 
        # "expression", 
        # "name", 
        # "symbol", 
        # "function", 
        # "uneval", 
        # "environment", 
        # "ggplot2", 
        # "ggplot_built", 
        # "call"
    )
    tempo.arg.base <-base::c( # no names(formals(fun = sys.function(sys.parent(n = 2)))) used with arg_check() to be sure to deal with the correct environment
        "class", 
        "typeof", 
        "mode", 
        "length", 
        "prop", 
        "double.as.integer.allowed", 
        "options", 
        "all.options.in.data", 
        "na.contain", 
        "neg.values", 
        "inf.values", 
        "print", 
        "data.name", 
        "fun.name"
    )
    tempo.class <-base::list( # no get() used to be sure to deal with the correct environment
        base::class(class), 
        base::class(typeof), 
        base::class(mode), 
        base::class(length), 
        base::class(prop), 
        base::class(double.as.integer.allowed), 
        base::class(options), 
        base::class(all.options.in.data), 
        base::class(na.contain), 
        base::class(neg.values), 
        base::class(inf.values), 
        base::class(print), 
        base::class(data.name), 
        base::class(fun.name)
    )
    tempo <- ! base::sapply(base::lapply(tempo.class, FUN = "%in%", basic.class), FUN = base::all)
    if(base::any(tempo)){
        tempo.cat1 <- tempo.arg.base[tempo]
        tempo.cat2 <- base::sapply(tempo.class[tempo], FUN = base::paste0, collapse = " ")
        tempo.sep <- base::sapply(base::mapply(" ", base::max(base::nchar(tempo.cat1)) - base::nchar(tempo.cat1) + 3, FUN = base::rep, SIMPLIFY = FALSE), FUN = base::paste0, collapse = "")
        tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": ANY ARGUMENT EXCEPT data MUST HAVE A BASIC CLASS\nPROBLEMATIC ARGUMENT", base::ifelse(base::length(tempo.cat1) > 1, "S", ""), " AND ASSOCIATED CLASS", base::ifelse(base::length(tempo.cat1) > 1, "ES ARE", " IS"), ":\n", base::paste0(tempo.cat1, tempo.sep, tempo.cat2, collapse = "\n")) # normally no NA with is.na()
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end management of special classes
    # management of NA arguments
    if(base::any(base::is.na(data.name)) | base::any(base::is.na(class)) | base::any(base::is.na(typeof)) | base::any(base::is.na(mode)) | base::any(base::is.na(length)) | base::any(base::is.na(prop)) | base::any(base::is.na(double.as.integer.allowed)) | base::any(base::is.na(all.options.in.data)) | base::any(base::is.na(na.contain)) | base::any(base::is.na(neg.values)) | base::any(base::is.na(inf.values)) | base::any(base::is.na(print)) | base::any(base::is.na(fun.name))){ # normally no NA with is.na()
        tempo <- base::c("data.name", "class", "typeof", "mode", "length", "prop", "double.as.integer.allowed", "all.options.in.data", "na.contain", "neg.values", "inf.values", "print", "fun.name")[base::c(base::any(base::is.na(data.name)), base::any(base::is.na(class)), base::any(base::is.na(typeof)), base::any(base::is.na(mode)), base::any(base::is.na(length)), base::any(base::is.na(prop)), base::any(base::is.na(double.as.integer.allowed)), base::any(base::is.na(all.options.in.data)), base::any(base::is.na(na.contain)), base::any(base::is.na(neg.values)), base::any(base::is.na(inf.values)), base::any(base::is.na(print)), base::any(base::is.na(fun.name)))]
        tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE",base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": NO ARGUMENT EXCEPT data AND options CAN HAVE NA VALUES\nPROBLEMATIC ARGUMENT", base::ifelse(base::length(tempo) > 1, "S ARE", " IS"), ":\n", base::paste(tempo, collapse = "\n")) # normally no NA with is.na()
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <- base::c(
        "prop", 
        "double.as.integer.allowed", 
        "all.options.in.data", 
        "na.contain",
        "neg.values",
        "inf.values",
        "print"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){ # normally no NA with base::is.null()
        tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE: \n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT BE NULL:\n", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    # dealing with logical
    # tested below
    # end dealing with logical
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    # warning initiation
    # end warning initiation
    # other checkings
    if( ! base::is.null(data.name)){
        if( ! (base::length(data.name) == 1L & base::all(base::class(data.name) == "character"))){ # base::all() without na.rm -> ok because class(NA) is "logical"
            tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": data.name ARGUMENT MUST BE A SINGLE CHARACTER ELEMENT AND NOT ", base::paste(data.name, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    if(base::is.null(options) & base::is.null(class) & base::is.null(typeof) & base::is.null(mode) &  prop == FALSE & base::is.null(length)){
        tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": AT LEAST ONE OF THE options, class, typeof, mode, prop, OR length ARGUMENT MUST BE SPECIFIED (I.E, TRUE FOR prop)")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! base::is.null(options) & ( ! base::is.null(class) | ! base::is.null(typeof) | ! base::is.null(mode) | prop == TRUE)){
        tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": THE class, typeof, mode ARGUMENTS MUST BE NULL, AND prop FALSE, IF THE options ARGUMENT IS SPECIFIED\nTHE options ARGUMENT MUST BE NULL IF THE class AND/OR typeof AND/OR mode AND/OR prop ARGUMENT IS SPECIFIED")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! (base::all(base::class(neg.values) == "logical") & base::length(neg.values) == 1L)){ # base::all() without na.rm -> ok because class(NA) is "logical" 
        tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": THE neg.values ARGUMENT MUST BE TRUE OR FALSE ONLY")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if(neg.values == FALSE & base::is.null(class) & base::is.null(typeof) & base::is.null(mode)){
        tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": THE neg.values ARGUMENT CANNOT BE SWITCHED TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! (base::all(base::class(inf.values) == "logical") & base::length(inf.values) == 1L)){ # base::all() without na.rm -> ok because class(NA) is "logical" 
        tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": THE inf.values ARGUMENT MUST BE TRUE OR FALSE ONLY")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if(inf.values == FALSE & base::is.null(class) & base::is.null(typeof) & base::is.null(mode)){
        tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": THE inf.values ARGUMENT CANNOT BE SWITCHED TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! base::is.null(class)){ # may add "formula" and "Date" as in https://renenyffenegger.ch/notes/development/languages/R/functions/class
        if( ! base::all(class %in% base::c("vector", "logical", "integer", "numeric", "complex", "character", "matrix", "array", "data.frame", "list", "factor", "table", "expression", "name", "symbol", "function", "uneval", "environment", "ggplot2", "ggplot_built", "call") & base::length(class) == 1L)){ # length == 1L here because of class(matrix()) since R4.0.0  # base::all() without na.rm -> ok because class cannot be NA (tested above)
            tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": class ARGUMENT MUST BE ONE OF THESE VALUE:\n\"vector\", \"logical\", \"integer\", \"numeric\", \"complex\", \"character\", \"matrix\", \"array\", \"data.frame\", \"list\", \"factor\", \"table\", \"expression\", \"name\", \"symbol\", \"function\", \"environment\", \"ggplot2\", \"ggplot_built\", \"call\"")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(neg.values == FALSE & ! base::any(class %in% base::c("vector", "numeric", "integer", "matrix", "array", "data.frame", "table"))){ # no need of na.rm = TRUE for any() because %in% does not output NA
            tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": class ARGUMENT CANNOT BE OTHER THAN \"vector\", \"numeric\", \"integer\", \"matrix\", \"array\", \"data.frame\", \"table\" IF neg.values ARGUMENT IS SWITCHED TO FALSE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(inf.values == FALSE & ! base::any(class %in% base::c("vector", "numeric", "matrix", "array", "data.frame", "table"))){ # no need of na.rm = TRUE for any() because %in% does not output NA
            tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": class ARGUMENT CANNOT BE OTHER THAN \"vector\", \"numeric\", \"matrix\", \"array\", \"data.frame\", \"table\" IF inf.values ARGUMENT IS SWITCHED TO FALSE. \"integer IS NOT ALLOWED BECAUSE IFINITE VALUES ARE NOT INTEGERS\"")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    if( ! base::is.null(typeof)){ # all the types are here: https://renenyffenegger.ch/notes/development/languages/R/functions/typeof
        if( ! (base::all(typeof %in% base::c("logical", "integer", "double", "complex", "character", "list", "expression", "symbol", "closure", "special", "builtin", "environment", "S4", "language")) & base::length(typeof) == 1L)){ # "language" is the type of object of class "call" # base::all() without na.rm -> ok because typeof cannot be NA (tested above)
            tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": typeof ARGUMENT MUST BE ONE OF THESE VALUE:\n\"logical\", \"integer\", \"double\", \"complex\", \"character\", \"list\", \"expression\", \"name\", \"symbol\", \"closure\", \"special\", \"builtin\", \"environment\", \"S4\", \"language\"")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(neg.values == FALSE & ! typeof %in% base::c("double", "integer")){
            tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": typeof ARGUMENT CANNOT BE OTHER THAN \"double\" OR \"integer\" IF neg.values ARGUMENT IS SWITCHED TO FALSE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(inf.values == FALSE & typeof != "double"){
            tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": typeof ARGUMENT CANNOT BE OTHER THAN \"double\" IF inf.values ARGUMENT IS SWITCHED TO FALSE. \"integer IS NOT ALLOWED BECAUSE IFINITE VALUES ARE NOT INTEGERS\"")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    if( ! base::is.null(mode)){ # all the types are here: https://renenyffenegger.ch/notes/development/languages/R/functions/typeof
        if( ! (base::all(mode %in% base::c("logical", "numeric", "complex", "character", "list", "expression", "name", "symbol", "function", "environment", "S4", "call")) & base::length(mode) == 1L)){ # base::all() without na.rm -> ok because mode cannot be NA (tested above)
            tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": mode ARGUMENT MUST BE ONE OF THESE VALUE:\n\"logical\", \"numeric\", \"complex\", \"character\", \"list\", \"expression\", \"name\", \"symbol\", \"function\", \"environment\", \"S4\", \"call\"")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(neg.values == FALSE & mode != "numeric"){
            tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": mode ARGUMENT CANNOT BE OTHER THAN \"numeric\" IF neg.values ARGUMENT IS SWITCHED TO FALSE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(inf.values == FALSE & mode != "numeric"){
            tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": mode ARGUMENT CANNOT BE OTHER THAN \"numeric\" IF inf.values ARGUMENT IS SWITCHED TO FALSE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    if( ! base::is.null(length)){
        if( ! (base::is.numeric(length) & base::length(length) == 1L & base::all( ! base::grepl(length, pattern = "\\.")))){ # is.na() already arg_checked for length
            tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": length ARGUMENT MUST BE A SINGLE INTEGER VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    if( ! (base::is.logical(prop) & base::length(prop) == 1L)){ # is.na() already checked for prop
        tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": prop ARGUMENT MUST BE TRUE OR FALSE ONLY")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }else if(prop == TRUE){
        if( ! base::is.null(class)){
            if( ! base::any(class %in% base::c("vector", "numeric", "matrix", "array", "data.frame", "table"))){ # no need of na.rm = TRUE for any() because %in% does not output NA
                tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": class ARGUMENT CANNOT BE OTHER THAN NULL, \"vector\", \"numeric\", \"matrix\", \"array\", \"data.frame\", \"table\" IF prop ARGUMENT IS TRUE") # not integer because prop
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
            }
        }
        if( ! base::is.null(mode)){
            if(mode != "numeric"){
                tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": mode ARGUMENT CANNOT BE OTHER THAN NULL OR \"numeric\" IF prop ARGUMENT IS TRUE")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
            }
        }
        if( ! base::is.null(typeof)){
            if(typeof != "double"){
                tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": typeof ARGUMENT CANNOT BE OTHER THAN NULL OR \"double\" IF prop ARGUMENT IS TRUE")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
            }
        }
    }
    if( ! (base::all(base::class(double.as.integer.allowed) == "logical") & base::length(double.as.integer.allowed) == 1L)){ # base::all() without na.rm -> ok because class() never returns NA
        tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": THE double.as.integer.allowed ARGUMENT MUST BE TRUE OR FALSE ONLY: ", base::paste(double.as.integer.allowed, collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! (base::is.logical(all.options.in.data) & base::length(all.options.in.data) == 1L)){
        tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": all.options.in.data ARGUMENT MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY): ", base::paste(all.options.in.data, collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! (base::all(base::class(na.contain) == "logical") & base::length(na.contain) == 1L)){ # base::all() without na.rm -> ok because class() never returns NA
        tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": THE na.contain ARGUMENT MUST BE TRUE OR FALSE ONLY: ", base::paste(na.contain, collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! (base::all(base::class(print) == "logical") & base::length(print) == 1L)){ # base::all() without na.rm -> ok because class() never returns NA
        tempo.cat <- base::paste0("ERROR IN arg_check() OF THE ", package.name, " PACKAGE", base::ifelse(base::is.null(fun.name), "", base::paste0(" INSIDE ", fun.name)), ": THE print ARGUMENT MUST BE TRUE OR FALSE ONLY: ", base::paste(print, collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # data.name and fun.name tested at the beginning
    # end other checkings
    # end second round of checking and data preparation

    # main code
    if(base::is.null(data.name)){
        data.name <- base::deparse(base::substitute(data))
    }
    problem <- FALSE
    text <- base::paste0(base::ifelse(base::is.null(fun.name), "", base::paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT")
    if(( ! base::is.null(options)) & (base::all(base::typeof(data) == "character") | base::all(base::typeof(data) == "integer") | base::all(base::typeof(data) == "double"))){ # base::all() without na.rm -> ok because typeof() never returns NA
        test.log <- TRUE
        if(base::all(base::typeof(data) == "double")){
            if( ! base::all(data %% 1 == 0L, na.rm = TRUE)){
                problem <- TRUE
                text <- base::paste0(base::ifelse(base::is.null(fun.name), "ERROR", base::paste0("ERROR IN ", fun.name)), ": THE ", data.name, " OBJECT MUST BE SOME OF THESE OPTIONS: ", base::paste(options, collapse = " "), "\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER")
                test.log <- FALSE
            }
        }
        if(test.log == TRUE){
            text <- ""
            if( ! base::all(data %in% options)){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
                problem <- TRUE
                text <- base::paste0(base::ifelse(base::is.null(fun.name), "ERROR", base::paste0("ERROR IN ", fun.name)), ": THE ", data.name, " OBJECT MUST BE SOME OF THESE OPTIONS: ", base::paste(options, collapse = " "), "\nTHE PROBLEMATIC ELEMENTS OF ", data.name, " ARE: ", base::paste(base::unique(data[ ! (data %in% options)]), collapse = " "))
            }
            if(all.options.in.data == TRUE){
                if( ! base::all(options %in% data)){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
                    problem <- TRUE
                    text <- base::paste0(base::ifelse(text == "", "", base::paste0(text, "\n")), base::ifelse(base::is.null(fun.name), "ERROR", base::paste0("ERROR IN ", fun.name)), ": THE ", data.name, " OBJECT MUST BE MADE OF ALL THESE OPTIONS: ", base::paste(options, collapse = " "), "\nTHE MISSING ELEMENTS OF THE options ARGUMENT ARE: ",  base::paste(base::unique(options[ ! (options %in% data)]), collapse = " "))
                }
            }
            if( ! base::is.null(length)){
                if(base::length(data) != length){
                    problem <- TRUE
                    text <- base::paste0(base::ifelse(text == "", "", base::paste0(text, "\n")), base::ifelse(base::is.null(fun.name), "ERROR", base::paste0("ERROR IN ", fun.name)), ": THE LENGTH OF ", data.name, " MUST BE ", length, " AND NOT ", base::length(data))
                }
            }
            if(text == ""){
                text <- base::paste0(base::ifelse(base::is.null(fun.name), "", base::paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT")
            }
        }
    }else if( ! base::is.null(options)){
        problem <- TRUE
        text <- base::paste0(base::ifelse(base::is.null(fun.name), "ERROR", base::paste0("ERROR IN ", fun.name)), ": THE ", data.name, " OBJECT MUST BE SOME OF THESE OPTIONS: ", base::paste(options, collapse = " "), "\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER")
    }
    arg.names <- base::c("class", "typeof", "mode", "length")
    if( ! base::is.null(class)){
        if(class == "matrix"){ # because of class(matric()) since R4.0.0
            class <- base::c("matrix", "array")
        }else if(class == "factor" & base::all(base::class(data) %in% base::c("factor", "ordered"))){ # to deal with ordered factors # base::all() without na.rm -> ok because class(NA) is "logical"
            class <- base::c("factor", "ordered")
        }
    }
    if(base::is.null(options)){
        for(i2 in 1:base::length(arg.names)){
            if( ! base::is.null(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE))){
                # script to execute
                tempo.script <- '
                    problem <- TRUE ;
                    if(base::identical(text, base::paste0(base::ifelse(base::is.null(fun.name), "", base::paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT"))){
                        text <- base::paste0(base::ifelse(base::is.null(fun.name), "ERROR", base::paste0("ERROR IN ", fun.name)), ": THE ", data.name, " OBJECT MUST BE ") ;
                    }else{
                        text <- base::paste0(text, " AND ") ; 
                    }
                    text <- base::paste0(text, base::toupper(arg.names[i2]), " ", if(base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("matrix", "array"))){"matrix"}else if(base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("factor", "ordered"))){"factor"}else{base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE)})
                ' # no need of na.rm = TRUE for base::all() because %in% does not output NA
                # end script to execute
                if(base::typeof(data) == "double" & double.as.integer.allowed == TRUE & ((arg.names[i2] == "class" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")) | (arg.names[i2] == "typeof" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")))){ # no need of na.rm = TRUE for base::all() because == does not output NA if no NA in left of ==, which is the case for arg.names # typeof(data) == "double" means no factor allowed
                    if( ! base::all(data %% 1 == 0L, na.rm = TRUE)){ # to check integers (use %%, meaning the remaining of a division): see the precedent line. isTRUE(all.equal(data%%1, rep(0, length(data)))) not used because we strictly need zero as a result. Warning: na.rm = TRUE required here for base::all()
                        base::eval(base::parse(text = tempo.script)) # execute tempo.script
                    }
                }else if( ! base::any(base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg.names[i2], "(data)"))) %in% base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE))){ # test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function. No need of na.rm = TRUE for base::all() because %in% does not output NA # no need of na.rm = TRUE for base::all() because %in% does not output NA # no need of na.rm = TRUE for any() because get get(arg.names) does not contain NA
                    base::eval(base::parse(text = tempo.script)) # execute tempo.script
                }else if(arg.names[i2] == "class" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "vector") & ! (base::all(base::class(data) %in% "numeric") | base::all(base::class(data) %in% "integer") | base::all(base::class(data) %in% "character") | base::all(base::class(data) %in% "logical") | base::all(base::class(data) %in% "complex") | base::all(base::class(data) %in% "expression"))){ # test class == "vector". base::all(get(arg.names[i2], envir = sys.nframe(), inherits = FALSE) check is user as used the argument class = "vector". If TRUE and length(data) > 1, the class "numeric" "integer" "character" "logical" "complex" "expression" should be returned. No need of na.rm = TRUE for base::all() because %in% does not output NA # no need of na.rm = TRUE for base::all() because == does not output NA if no NA in left of ==, which is the case for arg.names. Other classes "list", "name", "symbol", "function", "environment", "S4", "call" return a list if length of data > 1
                    base::eval(base::parse(text = tempo.script)) # execute tempo.script
                }else if(arg.names[i2] == "class" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "ggplot2") & ! base::all(base::class(data) %in% base::c("gg", "ggplot"))){ # test ggplot object # no need of na.rm = TRUE for base::all() because == does not output NA if no NA in left of ==, which is the case for arg.names # no need of na.rm = TRUE for base::all() because %in% does not output NA
                    base::eval(base::parse(text = tempo.script)) # execute tempo.script
                }
            }
        }
    }
    if(prop == TRUE & base::all(base::typeof(data) == "double")){ # base::all() without na.rm -> ok because typeof(NA) is "logical"
        if(base::is.null(data) | base::any(data < 0 | data > 1, na.rm = TRUE)){ # works if data is NULL # Warning: na.rm = TRUE required here for any() # typeof(data) == "double" means no factor allowed
            problem <- TRUE
            if(base::identical(text, base::paste0(base::ifelse(base::is.null(fun.name), "", base::paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT"))){
                text <- base::paste0(base::ifelse(base::is.null(fun.name), "ERROR", base::paste0("ERROR IN ", fun.name)), ": ")
            }else{
                text <- base::paste0(text, " AND ")
            }
            text <- base::paste0(text, "THE ", data.name, " OBJECT MUST BE DECIMAL VALUES BETWEEN 0 AND 1")
        }
    }else if(prop == TRUE){
        problem <- TRUE
        if(base::identical(text, base::paste0(base::ifelse(base::is.null(fun.name), "", base::paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT"))){
            text <- base::paste0(base::ifelse(base::is.null(fun.name), "ERROR", base::paste0("ERROR IN ", fun.name)), ": ")
        }else{
            text <- base::paste0(text, " AND ")
        }
        text <- base::paste0(text, "THE ", data.name, " OBJECT MUST BE DECIMAL VALUES BETWEEN 0 AND 1")
    }
    if(base::all(base::class(data) %in% "expression")){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
        data <- base::as.character(data) # to evaluate the presence of NA
    }
    if(na.contain == FALSE & (base::mode(data) %in% base::c("logical", "numeric", "complex", "character", "list"))){ # before it was ! (class(data) %in% base::c("function", "environment"))
        if(base::any(base::is.na(data)) == TRUE){ # not on the same line because when data is class envir or function , do not like that # normally no NA with is.na()
            problem <- TRUE
            if(base::identical(text, base::paste0(base::ifelse(base::is.null(fun.name), "", base::paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT"))){
                text <- base::paste0(base::ifelse(base::is.null(fun.name), "ERROR", base::paste0("ERROR IN ", fun.name)), ": ")
            }else{
                text <- base::paste0(text, " AND ")
            }
            text <- base::paste0(text, "THE ", data.name, " OBJECT CONTAINS NA WHILE NOT AUTHORIZED")
        }
    }
    if(neg.values == FALSE & base::all(base::mode(data) %in% "numeric") & ! base::any(base::class(data) %in% "factor")){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
        if(base::any(data < 0, na.rm = TRUE)){ # Warning: na.rm = TRUE required here for any()
            problem <- TRUE
            if(base::identical(text, base::paste0(base::ifelse(base::is.null(fun.name), "", base::paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT"))){
                text <- base::paste0(base::ifelse(base::is.null(fun.name), "ERROR", base::paste0("ERROR IN ", fun.name)), ": ")
            }else{
                text <- base::paste0(text, " AND ")
            }
            text <- base::paste0(text, "THE ", data.name, " OBJECT MUST BE MADE OF NON NEGATIVE NUMERIC VALUES")
        }
    }else if(neg.values == FALSE){
        problem <- TRUE
        if(base::identical(text, base::paste0(base::ifelse(base::is.null(fun.name), "", base::paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT"))){
            text <- base::paste0(base::ifelse(base::is.null(fun.name), "ERROR", base::paste0("ERROR IN ", fun.name)), ": ")
        }else{
            text <- base::paste0(text, " AND ")
        }
        text <- base::paste0(text, "THE ", data.name, " OBJECT MUST BE MADE OF NON NEGATIVE VALUES BUT IS ", base::ifelse(base::any(base::class(data) %in% "factor"), "A FACTOR", "NOT EVEN MODE NUMERIC")) # no need of na.rm = TRUE
    }
    if(inf.values == FALSE & base::all(base::typeof(data) %in% "double") & ! base::any(base::class(data) %in% "factor")){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
        if(base::any(base::is.infinite(data), na.rm = TRUE)){ # Warning: na.rm = TRUE required here for any()
            problem <- TRUE
            if(base::identical(text, base::paste0(base::ifelse(base::is.null(fun.name), "", base::paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT"))){
                text <- base::paste0(base::ifelse(base::is.null(fun.name), "ERROR", base::paste0("ERROR IN ", fun.name)), ": ")
            }else{
                text <- base::paste0(text, " AND ")
            }
            text <- base::paste0(text, "THE ", data.name, " OBJECT MUST BE MADE OF NON INFINITE NUMERIC VALUES")
        }
    }else if(inf.values == FALSE){
        problem <- TRUE
        if(base::identical(text, base::paste0(base::ifelse(base::is.null(fun.name), "", base::paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT"))){
            text <- base::paste0(base::ifelse(base::is.null(fun.name), "ERROR", base::paste0("ERROR IN ", fun.name)), ": ")
        }else{
            text <- base::paste0(text, " AND ")
        }
        text <- base::paste0(text, "THE ", data.name, " OBJECT MUST BE MADE OF NON INFINITE VALUES BUT IS ", base::ifelse(base::any(base::class(data) %in% "factor"), "A FACTOR", "NOT EVEN TYPE DOUBLE")) # no need of na.rm = TRUE
    }
    if(print == TRUE & problem == TRUE){
        base::cat(base::paste0("\n\n================\n\n", text, "\n\n================\n\n"))
    }
    # end main code
    
    # output
    # warning output
    # end warning output
    output <- base::list(problem = problem, text = text, object.name = data.name)
    base::return(output)
    # end output
}
