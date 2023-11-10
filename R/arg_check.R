#' @title arg_check
#' @description
#' Check the class, type, mode and length of the data argument.
#' 
#' Mainly used to check the arguments of other functions.
#' 
#' Check also other kind of data parameters, is it a proportion? Is it type double but numbers without decimal part?
#' @param data Object to test.
#' @param class Single character string. Either one of the class() result (But see the warning section above) or "vector" or "ggplot2" (i.e., objects of class c("gg", "ggplot")) or NULL.
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
#' - §problem: logical. Is there any problem detected?
#' - §text: message indicating the details of the problem, or the absence of problem.
#' - §object.name: value of the data.name argument (i.e., name of the checked object if provided, NULL otherwise).
#' @details
#'  - If options == NULL, then at least class or type or mode or length argument must be non-null.
#'  - If options is non-null, then class, type and mode must be NULL, and length can be NULL or specified.
#'  
#' 
#' WARNINGS
#' 
#' The function tests what is written in its arguments, even if what is written is incoherent. For instance, arg_check(data = factor(1), class = "factor", mode = "character") will return a problem, whatever the object tested in the data argument, because no object can be class "factor" and mode "character" (factors are class "factor" and mode "numeric"). Of note, length of object of class "environment" is always 0.
#' 
#' If the tested object is NULL, then the function will always return a checking problem.
#' 
#' Since R >= 4.0.0, class(matrix()) returns "matrix" "array", and not "matrix" alone as before. However, use argument class = "matrix" to check for matrix object (of class "matrix" "array" in R >= 4.0.0) and use argument class = "array" to check for array object (of class "array" in R >= 4.0.0).
#' 
#' 
#' REQUIRED PACKAGES
#' 
#' None
#' 
#' 
#' REQUIRED FUNCTIONS FROM THE cute PACKAGE
#' 
#' None
#' 
#' @examples
#' test <- matrix(1:3)
#' arg_check(data = test, print = TRUE, class = "vector", mode = "numeric")
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
    
    # function name
    # no used in this function for the error message, to avoid env colliding
    # end function name
    # required function checking
    # end required function checking
    # reserved words
    # end reserved words
    # fun.name checked first because required next
    if( ! is.null(fun.name)){ # I have to use this way to deal with every kind of class for fun.name
        if(all(base::class(fun.name) == "character")){ # all() without na.rm -> ok because class(NA) is "logical"
            if(base::length(fun.name) != 1){
                tempo.cat <- paste0("ERROR IN arg_check(): THE fun.name ARGUMENT MUST BE A CHARACTER VECTOR OF LENGTH 1: ", paste(fun.name, collapse = " "))
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else if(any(is.na(fun.name))){ # normally no NA with is.na()
                tempo.cat <- paste0("ERROR IN arg_check(): NO ARGUMENT EXCEPT data AND options CAN HAVE NA VALUES\nPROBLEMATIC ARGUMENT IS fun.name")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
        }else{
            tempo.cat <- paste0("ERROR IN arg_check(): THE fun.name ARGUMENT MUST BE A CHARACTER VECTOR OF LENGTH 1") # paste(fun.name, collapse = " ") removed here because does not work with objects like function
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end fun.name checked first because required next
    # arg with no default values
    mandat.args <- c(
        "data"
    )
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, collapse = "), missing("), "))")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", fun.name, "\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args[tempo], collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument primary checking
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) # activate this line and use the function to check arguments status
    # end argument primary checking
    # second round of checking and data preparation
    # management of special classes
    basic.class <- c(
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
    tempo.arg.base <-c( # no names(formals(fun = sys.function(sys.parent(n = 2)))) used with arg_check() to be sure to deal with the correct environment
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
    tempo.class <-list( # no get() used to be sure to deal with the correct environment
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
    tempo <- ! sapply(lapply(tempo.class, FUN = "%in%", basic.class), FUN = all)
    if(any(tempo)){
        tempo.cat1 <- tempo.arg.base[tempo]
        tempo.cat2 <- sapply(tempo.class[tempo], FUN = paste0, collapse = " ")
        tempo.sep <- sapply(mapply(" ", max(nchar(tempo.cat1)) - nchar(tempo.cat1) + 3, FUN = rep, SIMPLIFY = FALSE), FUN = paste0, collapse = "")
        tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": ANY ARGUMENT EXCEPT data MUST HAVE A BASIC CLASS\nPROBLEMATIC ARGUMENT", ifelse(base::length(tempo.cat1) > 1, "S", ""), " AND ASSOCIATED CLASS", ifelse(base::length(tempo.cat1) > 1, "ES ARE", " IS"), ":\n", paste0(tempo.cat1, tempo.sep, tempo.cat2, collapse = "\n")) # normally no NA with is.na()
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of special classes
    # management of NA arguments
    if(any(is.na(data.name)) | any(is.na(class)) | any(is.na(typeof)) | any(is.na(mode)) | any(is.na(length)) | any(is.na(prop)) | any(is.na(double.as.integer.allowed)) | any(is.na(all.options.in.data)) | any(is.na(na.contain)) | any(is.na(neg.values)) | any(is.na(inf.values)) | any(is.na(print)) | any(is.na(fun.name))){ # normally no NA with is.na()
        tempo <- c("data.name", "class", "typeof", "mode", "length", "prop", "double.as.integer.allowed", "all.options.in.data", "na.contain", "neg.values", "inf.values", "print", "fun.name")[c(any(is.na(data.name)), any(is.na(class)), any(is.na(typeof)), any(is.na(mode)), any(is.na(length)), any(is.na(prop)), any(is.na(double.as.integer.allowed)), any(is.na(all.options.in.data)), any(is.na(na.contain)), any(is.na(neg.values)), any(is.na(inf.values)), any(is.na(print)), any(is.na(fun.name)))]
        tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": NO ARGUMENT EXCEPT data AND options CAN HAVE NA VALUES\nPROBLEMATIC ARGUMENT", ifelse(length(tempo) > 1, "S ARE", " IS"), ":\n", paste(tempo, collapse = "\n")) # normally no NA with is.na()
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-c(
        "prop", 
        "double.as.integer.allowed", 
        "all.options.in.data", 
        "na.contain",
        "neg.values",
        "inf.values",
        "print"
    )
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, envir = sys.nframe(), inherits = FALSE), FUN = is.null)
    if(any(tempo.log) == TRUE){ # normally no NA with is.null()
        tempo.cat <- paste0("ERROR IN arg_check():\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT BE NULL:\n", paste0(tempo.arg[tempo.log], collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
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
    if( ! is.null(data.name)){
        if( ! (base::length(data.name) == 1L & all(base::class(data.name) == "character"))){ # all() without na.rm -> ok because class(NA) is "logical"
            tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": data.name ARGUMENT MUST BE A SINGLE CHARACTER ELEMENT AND NOT ", paste(data.name, collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if(is.null(options) & is.null(class) & is.null(typeof) & is.null(mode) &  prop == FALSE & is.null(length)){
        tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": AT LEAST ONE OF THE options, class, typeof, mode, prop, OR length ARGUMENT MUST BE SPECIFIED (I.E, TRUE FOR prop)")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! is.null(options) & ( ! is.null(class) | ! is.null(typeof) | ! is.null(mode) | prop == TRUE)){
        tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": THE class, typeof, mode ARGUMENTS MUST BE NULL, AND prop FALSE, IF THE options ARGUMENT IS SPECIFIED\nTHE options ARGUMENT MUST BE NULL IF THE class AND/OR typeof AND/OR mode AND/OR prop ARGUMENT IS SPECIFIED")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! (all(base::class(neg.values) == "logical") & base::length(neg.values) == 1L)){ # all() without na.rm -> ok because class(NA) is "logical" 
        tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": THE neg.values ARGUMENT MUST BE TRUE OR FALSE ONLY")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(neg.values == FALSE & is.null(class) & is.null(typeof) & is.null(mode)){
        tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": THE neg.values ARGUMENT CANNOT BE SWITCHED TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! (all(base::class(inf.values) == "logical") & base::length(inf.values) == 1L)){ # all() without na.rm -> ok because class(NA) is "logical" 
        tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": THE inf.values ARGUMENT MUST BE TRUE OR FALSE ONLY")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(inf.values == FALSE & is.null(class) & is.null(typeof) & is.null(mode)){
        tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": THE inf.values ARGUMENT CANNOT BE SWITCHED TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! is.null(class)){ # may add "formula" and "Date" as in https://renenyffenegger.ch/notes/development/languages/R/functions/class
        if( ! all(class %in% c("vector", "logical", "integer", "numeric", "complex", "character", "matrix", "array", "data.frame", "list", "factor", "table", "expression", "name", "symbol", "function", "uneval", "environment", "ggplot2", "ggplot_built", "call") & base::length(class) == 1L)){ # length == 1L here because of class(matrix()) since R4.0.0  # all() without na.rm -> ok because class cannot be NA (tested above)
            tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": class ARGUMENT MUST BE ONE OF THESE VALUE:\n\"vector\", \"logical\", \"integer\", \"numeric\", \"complex\", \"character\", \"matrix\", \"array\", \"data.frame\", \"list\", \"factor\", \"table\", \"expression\", \"name\", \"symbol\", \"function\", \"environment\", \"ggplot2\", \"ggplot_built\", \"call\"")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(neg.values == FALSE & ! any(class %in% c("vector", "numeric", "integer", "matrix", "array", "data.frame", "table"))){ # no need of na.rm = TRUE for any() because %in% does not output NA
            tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": class ARGUMENT CANNOT BE OTHER THAN \"vector\", \"numeric\", \"integer\", \"matrix\", \"array\", \"data.frame\", \"table\" IF neg.values ARGUMENT IS SWITCHED TO FALSE")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(inf.values == FALSE & ! any(class %in% c("vector", "numeric", "matrix", "array", "data.frame", "table"))){ # no need of na.rm = TRUE for any() because %in% does not output NA
            tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": class ARGUMENT CANNOT BE OTHER THAN \"vector\", \"numeric\", \"matrix\", \"array\", \"data.frame\", \"table\" IF inf.values ARGUMENT IS SWITCHED TO FALSE. \"integer IS NOT ALLOWED BECAUSE IFINITE VALUES ARE NOT INTEGERS\"")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if( ! is.null(typeof)){ # all the types are here: https://renenyffenegger.ch/notes/development/languages/R/functions/typeof
        if( ! (all(typeof %in% c("logical", "integer", "double", "complex", "character", "list", "expression", "symbol", "closure", "special", "builtin", "environment", "S4", "language")) & base::length(typeof) == 1L)){ # "language" is the type of object of class "call" # all() without na.rm -> ok because typeof cannot be NA (tested above)
            tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": typeof ARGUMENT MUST BE ONE OF THESE VALUE:\n\"logical\", \"integer\", \"double\", \"complex\", \"character\", \"list\", \"expression\", \"name\", \"symbol\", \"closure\", \"special\", \"builtin\", \"environment\", \"S4\", \"language\"")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(neg.values == FALSE & ! typeof %in% c("double", "integer")){
            tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": typeof ARGUMENT CANNOT BE OTHER THAN \"double\" OR \"integer\" IF neg.values ARGUMENT IS SWITCHED TO FALSE")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(inf.values == FALSE & typeof != "double"){
            tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": typeof ARGUMENT CANNOT BE OTHER THAN \"double\" IF inf.values ARGUMENT IS SWITCHED TO FALSE. \"integer IS NOT ALLOWED BECAUSE IFINITE VALUES ARE NOT INTEGERS\"")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if( ! is.null(mode)){ # all the types are here: https://renenyffenegger.ch/notes/development/languages/R/functions/typeof
        if( ! (all(mode %in% c("logical", "numeric", "complex", "character", "list", "expression", "name", "symbol", "function", "environment", "S4", "call")) & base::length(mode) == 1L)){ # all() without na.rm -> ok because mode cannot be NA (tested above)
            tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": mode ARGUMENT MUST BE ONE OF THESE VALUE:\n\"logical\", \"numeric\", \"complex\", \"character\", \"list\", \"expression\", \"name\", \"symbol\", \"function\", \"environment\", \"S4\", \"call\"")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(neg.values == FALSE & mode != "numeric"){
            tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": mode ARGUMENT CANNOT BE OTHER THAN \"numeric\" IF neg.values ARGUMENT IS SWITCHED TO FALSE")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(inf.values == FALSE & mode != "numeric"){
            tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": mode ARGUMENT CANNOT BE OTHER THAN \"numeric\" IF inf.values ARGUMENT IS SWITCHED TO FALSE")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if( ! is.null(length)){
        if( ! (is.numeric(length) & base::length(length) == 1L & all( ! grepl(length, pattern = "\\.")))){ # is.na() already arg_checked for length
            tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": length ARGUMENT MUST BE A SINGLE INTEGER VALUE")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if( ! (is.logical(prop) & base::length(prop) == 1L)){ # is.na() already checked for prop
        tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": prop ARGUMENT MUST BE TRUE OR FALSE ONLY")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else if(prop == TRUE){
        if( ! is.null(class)){
            if( ! any(class %in% c("vector", "numeric", "matrix", "array", "data.frame", "table"))){ # no need of na.rm = TRUE for any() because %in% does not output NA
                tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": class ARGUMENT CANNOT BE OTHER THAN NULL, \"vector\", \"numeric\", \"matrix\", \"array\", \"data.frame\", \"table\" IF prop ARGUMENT IS TRUE") # not integer because prop
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
        }
        if( ! is.null(mode)){
            if(mode != "numeric"){
                tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": mode ARGUMENT CANNOT BE OTHER THAN NULL OR \"numeric\" IF prop ARGUMENT IS TRUE")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
        }
        if( ! is.null(typeof)){
            if(typeof != "double"){
                tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": typeof ARGUMENT CANNOT BE OTHER THAN NULL OR \"double\" IF prop ARGUMENT IS TRUE")
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
        }
    }
    if( ! (all(base::class(double.as.integer.allowed) == "logical") & base::length(double.as.integer.allowed) == 1L)){ # all() without na.rm -> ok because class() never returns NA
        tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": THE double.as.integer.allowed ARGUMENT MUST BE TRUE OR FALSE ONLY: ", paste(double.as.integer.allowed, collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! (is.logical(all.options.in.data) & base::length(all.options.in.data) == 1L)){
        tempo.cat <- paste0("ERROR IN arg_check()", ifelse(is.null(fun.name), "", paste0(" INSIDE ", fun.name)), ": all.options.in.data ARGUMENT MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY): ", paste(all.options.in.data, collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! (all(base::class(na.contain) == "logical") & base::length(na.contain) == 1L)){ # all() without na.rm -> ok because class() never returns NA
        tempo.cat <- paste0("ERROR IN arg_check(): THE na.contain ARGUMENT MUST BE TRUE OR FALSE ONLY: ", paste(na.contain, collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! (all(base::class(print) == "logical") & base::length(print) == 1L)){ # all() without na.rm -> ok because class() never returns NA
        tempo.cat <- paste0("ERROR IN arg_check(): THE print ARGUMENT MUST BE TRUE OR FALSE ONLY: ", paste(print, collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # data.name and fun.name tested at the beginning
    # end other checkings
    # end second round of checking and data preparation
    # package checking
    # end package checking
    # main code
    if(is.null(data.name)){
        data.name <- deparse(substitute(data))
    }
    problem <- FALSE
    text <- paste0(ifelse(is.null(fun.name), "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT")
    if(( ! is.null(options)) & (all(base::typeof(data) == "character") | all(base::typeof(data) == "integer") | all(base::typeof(data) == "double"))){ # all() without na.rm -> ok because typeof() never returns NA
        test.log <- TRUE
        if(all(base::typeof(data) == "double")){
            if( ! all(data %% 1 == 0L, na.rm = TRUE)){
                problem <- TRUE
                text <- paste0(ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": THE ", data.name, " OBJECT MUST BE SOME OF THESE OPTIONS: ", paste(options, collapse = " "), "\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER")
                test.log <- FALSE
            }
        }
        if(test.log == TRUE){
            text <- ""
            if( ! all(data %in% options)){ # no need of na.rm = TRUE for all() because %in% does not output NA
                problem <- TRUE
                text <- paste0(ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": THE ", data.name, " OBJECT MUST BE SOME OF THESE OPTIONS: ", paste(options, collapse = " "), "\nTHE PROBLEMATIC ELEMENTS OF ", data.name, " ARE: ", paste(unique(data[ ! (data %in% options)]), collapse = " "))
            }
            if(all.options.in.data == TRUE){
                if( ! all(options %in% data)){ # no need of na.rm = TRUE for all() because %in% does not output NA
                    problem <- TRUE
                    text <- paste0(ifelse(text == "", "", paste0(text, "\n")), ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": THE ", data.name, " OBJECT MUST BE MADE OF ALL THESE OPTIONS: ", paste(options, collapse = " "), "\nTHE MISSING ELEMENTS OF THE options ARGUMENT ARE: ",  paste(unique(options[ ! (options %in% data)]), collapse = " "))
                }
            }
            if( ! is.null(length)){
                if(base::length(data) != length){
                    problem <- TRUE
                    text <- paste0(ifelse(text == "", "", paste0(text, "\n")), ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": THE LENGTH OF ", data.name, " MUST BE ", length, " AND NOT ", base::length(data))
                }
            }
            if(text == ""){
                text <- paste0(ifelse(is.null(fun.name), "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT")
            }
        }
    }else if( ! is.null(options)){
        problem <- TRUE
        text <- paste0(ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": THE ", data.name, " OBJECT MUST BE SOME OF THESE OPTIONS: ", paste(options, collapse = " "), "\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER")
    }
    arg.names <- c("class", "typeof", "mode", "length")
    if( ! is.null(class)){
        if(class == "matrix"){ # because of class(matric()) since R4.0.0
            class <- c("matrix", "array")
        }else if(class == "factor" & all(base::class(data) %in% c("factor", "ordered"))){ # to deal with ordered factors # all() without na.rm -> ok because class(NA) is "logical"
            class <- c("factor", "ordered")
        }
    }
    if(is.null(options)){
        for(i2 in 1:base::length(arg.names)){
            if( ! is.null(get(arg.names[i2], envir = sys.nframe(), inherits = FALSE))){
                # script to execute
                tempo.script <- '
problem <- TRUE ;
if(identical(text, paste0(ifelse(is.null(fun.name), "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT"))){
text <- paste0(ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": THE ", data.name, " OBJECT MUST BE ") ;
}else{
text <- paste0(text, " AND ") ; 
}
text <- paste0(text, toupper(arg.names[i2]), " ", if(all(get(arg.names[i2], envir = sys.nframe(), inherits = FALSE) %in% c("matrix", "array"))){"matrix"}else if(all(get(arg.names[i2], envir = sys.nframe(), inherits = FALSE) %in% c("factor", "ordered"))){"factor"}else{get(arg.names[i2], envir = sys.nframe(), inherits = FALSE)})
' # no need of na.rm = TRUE for all() because %in% does not output NA
            # end script to execute
            if(base::typeof(data) == "double" & double.as.integer.allowed == TRUE & ((arg.names[i2] == "class" & all(get(arg.names[i2], envir = sys.nframe(), inherits = FALSE) == "integer")) | (arg.names[i2] == "typeof" & all(get(arg.names[i2], envir = sys.nframe(), inherits = FALSE) == "integer")))){ # no need of na.rm = TRUE for all() because == does not output NA if no NA in left of ==, which is the case for arg.names # typeof(data) == "double" means no factor allowed
                if( ! all(data %% 1 == 0L, na.rm = TRUE)){ # to check integers (use %%, meaning the remaining of a division): see the precedent line. isTRUE(all.equal(data%%1, rep(0, length(data)))) not used because we strictly need zero as a result. Warning: na.rm = TRUE required here for all()
                    eval(parse(text = tempo.script)) # execute tempo.script
                }
            }else if( ! any(all(get(arg.names[i2], envir = sys.nframe(), inherits = FALSE) %in% c("vector", "ggplot2"))) & ! all(eval(parse(text = paste0(arg.names[i2], "(data)"))) %in% get(arg.names[i2], envir = sys.nframe(), inherits = FALSE))){ # test the four c("class", "typeof", "mode", "length") arguments with their corresponding function. No need of na.rm = TRUE for all() because %in% does not output NA # no need of na.rm = TRUE for all() because %in% does not output NA # no need of na.rm = TRUE for any() because get get(arg.names) does not contain NA
                eval(parse(text = tempo.script)) # execute tempo.script
            }else if(arg.names[i2] == "class" & all(get(arg.names[i2], envir = sys.nframe(), inherits = FALSE) == "vector") & ! (all(base::class(data) %in% "numeric") | all(base::class(data) %in% "integer") | all(base::class(data) %in% "character") | all(base::class(data) %in% "logical"))){ # test class == "vector". No need of na.rm = TRUE for all() because %in% does not output NA # no need of na.rm = TRUE for all() because == does not output NA if no NA in left of ==, which is the case for arg.names
                eval(parse(text = tempo.script)) # execute tempo.script
            }else if(arg.names[i2] == "class" & all(get(arg.names[i2], envir = sys.nframe(), inherits = FALSE) == "ggplot2") & ! all(base::class(data) %in% c("gg", "ggplot"))){ # test ggplot object # no need of na.rm = TRUE for all() because == does not output NA if no NA in left of ==, which is the case for arg.names # no need of na.rm = TRUE for all() because %in% does not output NA
                eval(parse(text = tempo.script)) # execute tempo.script
            }
            }
        }
    }
if(prop == TRUE & all(base::typeof(data) == "double")){ # all() without na.rm -> ok because typeof(NA) is "logical"
    if(is.null(data) | any(data < 0 | data > 1, na.rm = TRUE)){ # works if data is NULL # Warning: na.rm = TRUE required here for any() # typeof(data) == "double" means no factor allowed
        problem <- TRUE
        if(identical(text, paste0(ifelse(is.null(fun.name), "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT"))){
            text <- paste0(ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": ")
        }else{
            text <- paste0(text, " AND ")
        }
        text <- paste0(text, "THE ", data.name, " OBJECT MUST BE DECIMAL VALUES BETWEEN 0 AND 1")
    }
}else if(prop == TRUE){
    problem <- TRUE
    if(identical(text, paste0(ifelse(is.null(fun.name), "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT"))){
        text <- paste0(ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": ")
    }else{
        text <- paste0(text, " AND ")
    }
    text <- paste0(text, "THE ", data.name, " OBJECT MUST BE DECIMAL VALUES BETWEEN 0 AND 1")
}
if(all(base::class(data) %in% "expression")){ # no need of na.rm = TRUE for all() because %in% does not output NA
    data <- as.character(data) # to evaluate the presence of NA
}
if(na.contain == FALSE & (base::mode(data) %in% c("logical", "numeric", "complex", "character", "list"))){ # before it was ! (class(data) %in% c("function", "environment"))
    if(any(is.na(data)) == TRUE){ # not on the same line because when data is class envir or function , do not like that # normally no NA with is.na()
        problem <- TRUE
        if(identical(text, paste0(ifelse(is.null(fun.name), "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT"))){
            text <- paste0(ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": ")
        }else{
            text <- paste0(text, " AND ")
        }
        text <- paste0(text, "THE ", data.name, " OBJECT CONTAINS NA WHILE NOT AUTHORIZED")
    }
}
if(neg.values == FALSE & all(base::mode(data) %in% "numeric") & ! any(base::class(data) %in% "factor")){ # no need of na.rm = TRUE for all() because %in% does not output NA
    if(any(data < 0, na.rm = TRUE)){ # Warning: na.rm = TRUE required here for any()
        problem <- TRUE
        if(identical(text, paste0(ifelse(is.null(fun.name), "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT"))){
            text <- paste0(ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": ")
        }else{
            text <- paste0(text, " AND ")
        }
        text <- paste0(text, "THE ", data.name, " OBJECT MUST BE MADE OF NON NEGATIVE NUMERIC VALUES")
    }
}else if(neg.values == FALSE){
    problem <- TRUE
    if(identical(text, paste0(ifelse(is.null(fun.name), "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT"))){
        text <- paste0(ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": ")
    }else{
        text <- paste0(text, " AND ")
    }
    text <- paste0(text, "THE ", data.name, " OBJECT MUST BE MADE OF NON NEGATIVE VALUES BUT IS ", ifelse(any(base::class(data) %in% "factor"), "A FACTOR", "NOT EVEN MODE NUMERIC")) # no need of na.rm = TRUE
}
if(inf.values == FALSE & all(base::typeof(data) %in% "double") & ! any(base::class(data) %in% "factor")){ # no need of na.rm = TRUE for all() because %in% does not output NA
    if(any(is.infinite(data), na.rm = TRUE)){ # Warning: na.rm = TRUE required here for any()
        problem <- TRUE
        if(identical(text, paste0(ifelse(is.null(fun.name), "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT"))){
            text <- paste0(ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": ")
        }else{
            text <- paste0(text, " AND ")
        }
        text <- paste0(text, "THE ", data.name, " OBJECT MUST BE MADE OF NON INFINITE NUMERIC VALUES")
    }
}else if(inf.values == FALSE){
    problem <- TRUE
    if(identical(text, paste0(ifelse(is.null(fun.name), "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, " OBJECT"))){
        text <- paste0(ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", fun.name)), ": ")
    }else{
        text <- paste0(text, " AND ")
    }
    text <- paste0(text, "THE ", data.name, " OBJECT MUST BE MADE OF NON INFINITE VALUES BUT IS ", ifelse(any(base::class(data) %in% "factor"), "A FACTOR", "NOT EVEN TYPE DOUBLE")) # no need of na.rm = TRUE
}
if(print == TRUE & problem == TRUE){
    cat(paste0("\n\n================\n\n", text, "\n\n================\n\n"))
}
# output
output <- list(problem = problem, text = text, object.name = data.name)
return(output)
# end output
# end main code
}
