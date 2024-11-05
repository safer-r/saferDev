#' @title arg_check
#' @description
#' Check expected values of an argument of functions: class, type, mode, length, restricted values panel, kind of numeric values in addition to the distinction between 'integer' and 'double' (proportion only? Inf values authorized? negative values authorized? Integers of type 'double'?)
#' @param data Object to test.
#' @param class Single character string. Either one of the class() result or "vector" or "ggplot2" (i.e., objects of class c("gg", "ggplot")) or NULL. See the warning section below.
#' @param typeof Single character string. Either one of the typeof() result or NULL.
#' @param mode Single character string. Either one of the mode() result (for non-vector object) or NULL.
#' @param length Single numeric value indicating the length of the object. Not considered if NULL.
#' @param prop Single logical value. Are the numeric values between 0 and 1 (proportion)? If TRUE, can be used alone, without considering class, etc.
#' @param double_as_integer_allowed Single logical value. If TRUE, no error is reported in the cheking message if argument is set to typeof == "integer" or class == "integer", while the reality is typeof == "double" or class == "numeric" but the numbers strictly have zero as modulo (remainder of a division). This means that i <- 1, which is typeof(i) -> "double" is considered as integer with double_as_integer_allowed = TRUE. WARNING: data mod 1 == 0L but not isTRUE(all.equal(data mod 1, 0)) is used here because the argument checks for integers stored as double (does not check for decimal numbers that are approximate integers).
#' @param options Vector of character strings or integers indicating all the possible option values for the data argument, or NULL. Numbers of type "double" are accepted if they have a 0 modulo.
#' @param all_options_in_data Single logical value. If TRUE, all of the options must be present at least once in the data argument, and nothing else. If FALSE, some or all of the options must be present in the data argument, and nothing else. Ignored if options base::is.null.
#' @param na_contain Single logical value. Can the data argument contain NA?
#' @param neg_values Single logical value. Are negative numeric values authorized? Warning: the default setting is TRUE, meaning that, in that case, no check is performed for the presence of negative values. The neg_values argument is activated only when set to FALSE. In addition, (1) neg_values = FALSE can only be used when class, typeof or mode arguments are not NULL, otherwise return an error message, (2) the presence of negative values is not checked with neg_values = FALSE if the tested object is a factor and the following checking message is returned "OBJECT MUST BE MADE OF NON NEGATIVE VALUES BUT IS A FACTOR".
#' @param inf_values Single logical value. Are infinite numeric values authorized (Inf or -Inf)? Identical remarks as for the neg_values argument.
#' @param print Single logical value. Print the message if $problem is TRUE? Warning: set by default to FALSE, which facilitates the control of the checking message output when using arg_check() inside functions. See the example section.
#' @param data_name Single character string indicating the name of the object to test. If NULL, use what is assigned to the data argument for the returned message.
#' @param fun_name Single character string indicating the name of the function checked (i.e., when arg_check() is used to check the arguments of this function). If non-null, the value of fun_name will be added into the message returned by arg_check().
#' @param pack_name Single character string indicating the name of the package of fun_name. If non-null, the value of pack_name will be added into the message returned by arg_check().
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns 
#' A list containing:
#' 
#'- §problem: logical. Is there any problem detected?
#' 
#'- §text: message indicating the details of the problem, or the absence of problem.
#' 
#' - §object.name: value of the data_name argument (i.e., name of the checked object if provided, NULL otherwise).
#' @details
#' - If options == NULL, then at least class or type or mode or length argument must be non-null.
#'  
#' - If options is non-null, then class, type and mode must be NULL, and length can be NULL or specified.
#'  
#' - The function tests what is written in its arguments, even if what is written is incoherent. For instance, arg_check(data = factor(1), class = "factor", mode = "character") will return a problem, whatever the object tested in the data argument, because no object can be class "factor" and mode "character" (factors are class "factor" and mode "numeric"). Of note, length of object of class "environment" is always 0.
#'  
#' - If the tested object base::is.null, then the function will always return a checking problem.
#'  
#' - Argument "class" with value "vector" means that the object is tested for class(data) returning only "numeric", "integer", "character", "logical", "complex" or "expression". Please, use another value of class (e.g., class = "call" or class = "list") for other types and class of objects
#'  
#' - Since R >= 4.0.0, class(matrix()) returns "matrix" "array", and not "matrix" alone as before. However, use argument class = "matrix" to check for matrix object (of class "matrix" "array" in R >= 4.0.0) and use argument class = "array" to check for array object (of class "array" in R >= 4.0.0).
#' @seealso \code{\link{match.arg}} et \code{\link{arg_test}}.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
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
        double_as_integer_allowed = FALSE, 
        options = NULL, 
        all_options_in_data = FALSE, 
        na_contain = FALSE, 
        neg_values = TRUE, 
        inf_values = TRUE, 
        print = FALSE, 
        data_name = NULL, 
        fun_name = NULL, 
        pack_name = NULL,
        safer_check = TRUE
){
    # DEBUGGING
    # data = mean ; class = NULL ; typeof = NULL ; mode = NULL ; length = NULL ; prop = FALSE ; double_as_integer_allowed = FALSE ; options = "a" ; all_options_in_data = FALSE ; na_contain = FALSE ; neg_values = TRUE ; inf_values = TRUE ; print = TRUE ; data_name = NULL ; fun_name = NULL; safer_check = TRUE
    # package name
    package_name <- "saferDev"
    # end package name
    # function name
    function_name <- "arg_check"
    # classical safer code no used in this function for the error message, to avoid env colliding
    # fun_name checked first because required next
    if( ! base::is.null(fun_name)){ # I have to use this way to deal with every kind of class for fun_name
        if(base::all(base::class(fun_name) == "character")){ # base::all() without na.rm -> ok because base::class(NA) is "logical"
            if(base::length(fun_name) != 1){
                tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE\nTHE fun_name ARGUMENT MUST BE A CHARACTER VECTOR OF LENGTH 1: ", base::paste(fun_name, collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
            }else if(base::any(base::is.na(fun_name))){ # normally no NA with base::is.na()
                tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE\nNO ARGUMENT EXCEPT data AND options CAN HAVE NA VALUES\nPROBLEMATIC ARGUMENT IS fun_name")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
            }
        }else{
            tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE\nTHE fun_name ARGUMENT MUST BE A CHARACTER VECTOR OF LENGTH 1") # base::paste(fun_name, collapse = " ") removed here because does not work with objects like function
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    # end fun_name checked first because required next
    # pack_name checked first because required next
    if( ! base::is.null(pack_name)){ # I have to use this way to deal with every kind of class for pack_name
        if(base::is.null(fun_name)){
            tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE\nTHE fun_name ARGUMENT CANNOT BE NULL IF THE pack_name ARGUMENT IS NOT NULL")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(base::all(base::class(pack_name) == "character")){ # base::all() without na.rm -> ok because base::class(NA) is "logical"
            if(base::length(pack_name) != 1){
                tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE\nTHE pack_name ARGUMENT MUST BE A CHARACTER VECTOR OF LENGTH 1: ", base::paste(pack_name, collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
            }else if(base::any(base::is.na(pack_name))){ # normally no NA with base::is.na()
                tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE\nNO ARGUMENT EXCEPT data AND options CAN HAVE NA VALUES\nPROBLEMATIC ARGUMENT IS pack_name")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
            }
        }else{
            tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE\nTHE pack_name ARGUMENT MUST BE A CHARACTER VECTOR OF LENGTH 1") # base::paste(pack_name, collapse = " ") removed here because does not work with objects like function
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    # end fun_name checked first because required next
    # end function name
    # critical operator checking
    if(safer_check == TRUE){
        saferDev:::.base_op_check(
            external_function_name = fun_name,
            external_package_name = package_name
        )
    }
    # end critical operator checking
    # check of lib_path
    # end check of lib_path
    # saferdev required function checking
    # end saferdev required function checking
    # check of the required function from the required packages
    # end check of the required function from the required packages

    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "data"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat.args, collapse = "), base::missing("), "))")))
    if(base::any(tempo)){ # normally no NA for base::missing() output
        tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args[tempo], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
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
        "NULL", # because base::class(NULL) is "NULL". The NULL aspect will be dealt later
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
    tempo.arg.base <-base::c( # no base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) used with arg_check() to be sure to deal with the correct environment
        "class", 
        "typeof", 
        "mode", 
        "length", 
        "prop", 
        "double_as_integer_allowed", 
        "options", 
        "all_options_in_data", 
        "na_contain", 
        "neg_values", 
        "inf_values", 
        "print", 
        "data_name", 
        "fun_name"
    )
    tempo.class <-base::list( # no base::get() used to be sure to deal with the correct environment
        base::class(class), 
        base::class(typeof), 
        base::class(mode), 
        base::class(length), 
        base::class(prop), 
        base::class(double_as_integer_allowed), 
        base::class(options), 
        base::class(all_options_in_data), 
        base::class(na_contain), 
        base::class(neg_values), 
        base::class(inf_values), 
        base::class(print), 
        base::class(data_name), 
        base::class(fun_name)
    )
    tempo <- ! base::sapply(base::lapply(tempo.class, FUN = "%in%", basic.class), FUN = base::all)
    if(base::any(tempo)){
        tempo.cat1 <- tempo.arg.base[tempo]
        tempo.cat2 <- base::sapply(tempo.class[tempo], FUN = base::paste0, collapse = " ")
        tempo.sep <- base::sapply(base::mapply(" ", base::max(base::nchar(tempo.cat1)) - base::nchar(tempo.cat1) + 3, FUN = base::rep, SIMPLIFY = FALSE), FUN = base::paste0, collapse = "")
        tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nANY ARGUMENT EXCEPT data MUST HAVE A BASIC CLASS\nPROBLEMATIC ARGUMENT", base::ifelse(base::length(tempo.cat1) > 1, "S", ""), " AND ASSOCIATED CLASS", base::ifelse(base::length(tempo.cat1) > 1, "ES ARE", " IS"), ":\n", base::paste0(tempo.cat1, tempo.sep, tempo.cat2, collapse = "\n")) # normally no NA with base::is.na()
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end management of special classes
    # management of NA arguments
    if(base::any(base::is.na(data_name)) | base::any(base::is.na(class)) | base::any(base::is.na(typeof)) | base::any(base::is.na(mode)) | base::any(base::is.na(length)) | base::any(base::is.na(prop)) | base::any(base::is.na(double_as_integer_allowed)) | base::any(base::is.na(all_options_in_data)) | base::any(base::is.na(na_contain)) | base::any(base::is.na(neg_values)) | base::any(base::is.na(inf_values)) | base::any(base::is.na(print)) | base::any(base::is.na(fun_name))){ # normally no NA with base::is.na()
        tempo <- base::c("data_name", "class", "typeof", "mode", "length", "prop", "double_as_integer_allowed", "all_options_in_data", "na_contain", "neg_values", "inf_values", "print", "fun_name")[base::c(base::any(base::is.na(data_name)), base::any(base::is.na(class)), base::any(base::is.na(typeof)), base::any(base::is.na(mode)), base::any(base::is.na(length)), base::any(base::is.na(prop)), base::any(base::is.na(double_as_integer_allowed)), base::any(base::is.na(all_options_in_data)), base::any(base::is.na(na_contain)), base::any(base::is.na(neg_values)), base::any(base::is.na(inf_values)), base::any(base::is.na(print)), base::any(base::is.na(fun_name)))]
        tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nNO ARGUMENT EXCEPT data AND options CAN HAVE NA VALUES\nPROBLEMATIC ARGUMENT", base::ifelse(base::length(tempo) > 1, "S ARE", " IS"), ":\n", base::paste(tempo, collapse = "\n")) # normally no NA with base::is.na()
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <- base::c(
        "prop", 
        "double_as_integer_allowed", 
        "all_options_in_data", 
        "na_contain",
        "neg_values",
        "inf_values",
        "print",
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){ # normally no NA with base::is.null()
        tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT BE NULL:\n", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
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
    if( ! base::is.null(data_name)){
        if( ! (base::length(data_name) == 1L & base::all(base::class(data_name) == "character"))){ # base::all() without na.rm -> ok because base::class(NA) is "logical"
            tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\ndata_name ARGUMENT MUST BE A SINGLE CHARACTER ELEMENT AND NOT ", base::paste(data_name, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    if(base::is.null(options) & base::is.null(class) & base::is.null(typeof) & base::is.null(mode) &  prop == FALSE & base::is.null(length)){
        tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nAT LEAST ONE OF THE options, class, typeof, mode, prop, OR length ARGUMENT MUST BE SPECIFIED (I.E, TRUE FOR prop)")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! base::is.null(options) & ( ! base::is.null(class) | ! base::is.null(typeof) | ! base::is.null(mode) | prop == TRUE)){
        tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nTHE class, typeof, mode ARGUMENTS MUST BE NULL, AND prop FALSE, IF THE options ARGUMENT IS SPECIFIED\nTHE options ARGUMENT MUST BE NULL IF THE class AND/OR typeof AND/OR mode AND/OR prop ARGUMENT IS SPECIFIED")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! (base::all(base::class(neg_values) == "logical") & base::length(neg_values) == 1L)){ # base::all() without na.rm -> ok because base::class(NA) is "logical" 
        tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nTHE neg_values ARGUMENT MUST BE TRUE OR FALSE ONLY")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if(neg_values == FALSE & base::is.null(class) & base::is.null(typeof) & base::is.null(mode)){
        tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nTHE neg_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! (base::all(base::class(inf_values) == "logical") & base::length(inf_values) == 1L)){ # base::all() without na.rm -> ok because base::class(NA) is "logical" 
        tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nTHE inf_values ARGUMENT MUST BE TRUE OR FALSE ONLY")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if(inf_values == FALSE & base::is.null(class) & base::is.null(typeof) & base::is.null(mode)){
        tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nTHE inf_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! base::is.null(class)){ # may add "formula" and "Date" as in https://renenyffenegger.ch/notes/development/languages/R/functions/class
        if( ! base::all(class %in% base::c("vector", "logical", "integer", "numeric", "complex", "character", "matrix", "array", "data.frame", "list", "factor", "table", "expression", "name", "symbol", "function", "uneval", "environment", "ggplot2", "ggplot_built", "call") & base::length(class) == 1L)){ # length == 1L here because of base::class(base::matrix()) since R4.0.0  # base::all() without na.rm -> ok because class cannot be NA (tested above)
            tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nclass ARGUMENT MUST BE ONE OF THESE VALUE:\n\"vector\", \"logical\", \"integer\", \"numeric\", \"complex\", \"character\", \"matrix\", \"array\", \"data.frame\", \"list\", \"factor\", \"table\", \"expression\", \"name\", \"symbol\", \"function\", \"environment\", \"ggplot2\", \"ggplot_built\", \"call\"")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(neg_values == FALSE & ! base::any(class %in% base::c("vector", "numeric", "integer", "matrix", "array", "data.frame", "table"))){ # no need of na.rm = TRUE for base::any() because %in% does not output NA
            tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nclass ARGUMENT CANNOT BE OTHER THAN \"vector\", \"numeric\", \"integer\", \"matrix\", \"array\", \"data.frame\", \"table\" IF ABSENCE OF NEGATIVE VALUES IS CONTROLED BY SWITCHING THE neg_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(inf_values == FALSE & ! base::any(class %in% base::c("vector", "numeric", "matrix", "array", "data.frame", "table"))){ # no need of na.rm = TRUE for base::any() because %in% does not output NA
            tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nclass ARGUMENT CANNOT BE OTHER THAN \"vector\", \"numeric\", \"matrix\", \"array\", \"data.frame\", \"table\" IF ABSENCE OF INFINITE VALUE IS CONTROLED BY SWITCHING THE inf_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE\n\"integer\" IS NOT ALLOWED BECAUSE OBJECTS WITH INFINITE VALUES ARE NOT INTEGERS")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    if( ! base::is.null(typeof)){ # all the types are here: https://renenyffenegger.ch/notes/development/languages/R/functions/typeof
        if( ! (base::all(typeof %in% base::c("logical", "integer", "double", "complex", "character", "list", "expression", "symbol", "closure", "special", "builtin", "environment", "S4", "language")) & base::length(typeof) == 1L)){ # "language" is the type of object of class "call" # base::all() without na.rm -> ok because typeof cannot be NA (tested above)
            tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\ntypeof ARGUMENT MUST BE ONE OF THESE VALUE:\n\"logical\", \"integer\", \"double\", \"complex\", \"character\", \"list\", \"expression\", \"name\", \"symbol\", \"closure\", \"special\", \"builtin\", \"environment\", \"S4\", \"language\"")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(neg_values == FALSE & ! typeof %in% base::c("double", "integer")){
            tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\ntypeof ARGUMENT CANNOT BE OTHER THAN \"double\" OR \"integer\" IF ABSENCE OF NEGATIVE VALUES IS CONTROLED BY SWITCHING THE neg_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(inf_values == FALSE & typeof != "double"){
            tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\ntypeof ARGUMENT CANNOT BE OTHER THAN \"double\" IF ABSENCE OF INFINITE VALUE ARE CONTROLED BY SWITCHING THE inf_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE\n\"integer\" IS NOT ALLOWED BECAUSE OBJECTS WITH INFINITE VALUES ARE NOT INTEGERS")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    if( ! base::is.null(mode)){ # all the types are here: https://renenyffenegger.ch/notes/development/languages/R/functions/typeof
        if( ! (base::all(mode %in% base::c("logical", "numeric", "complex", "character", "list", "expression", "name", "symbol", "function", "environment", "S4", "call")) & base::length(mode) == 1L)){ # base::all() without na.rm -> ok because mode cannot be NA (tested above)
            tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nmode ARGUMENT MUST BE ONE OF THESE VALUE:\n\"logical\", \"numeric\", \"complex\", \"character\", \"list\", \"expression\", \"name\", \"symbol\", \"function\", \"environment\", \"S4\", \"call\"")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(neg_values == FALSE & mode != "numeric"){
            tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nmode ARGUMENT CANNOT BE OTHER THAN \"numeric\" IF ABSENCE OF NEGATIVE VALUES IS CONTROLED BY SWITCHING THE neg_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(inf_values == FALSE & mode != "numeric"){
            tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nmode ARGUMENT CANNOT BE OTHER THAN \"numeric\" IF ABSENCE OF INFINITE VALUE ARE CONTROLED BY SWITCHING THE inf_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE\nOTHER VALUES NOT ALLOWED BECAUSE OBJECTS WITH INFINITE VALUES ARE ONLY \"numeric\"")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    if( ! base::is.null(length)){
        if( ! (base::is.numeric(length) & base::length(length) == 1L & base::all( ! base::grepl(length, pattern = "\\.")))){ # base::is.na() already arg_checked for length
            tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nlength ARGUMENT MUST BE A SINGLE INTEGER VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    if( ! (base::is.logical(prop) & base::length(prop) == 1L)){ # base::is.na() already checked for prop
        tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nprop ARGUMENT MUST BE TRUE OR FALSE ONLY")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }else if(prop == TRUE){
        if( ! base::is.null(class)){
            if( ! base::any(class %in% base::c("vector", "numeric", "matrix", "array", "data.frame", "table"))){ # no need of na.rm = TRUE for base::any() because %in% does not output NA
                tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nclass ARGUMENT CANNOT BE OTHER THAN NULL, \"vector\", \"numeric\", \"matrix\", \"array\", \"data.frame\", \"table\" IF prop ARGUMENT IS TRUE") # not integer because prop
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
            }
        }
        if( ! base::is.null(mode)){
            if(mode != "numeric"){
                tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nmode ARGUMENT CANNOT BE OTHER THAN NULL OR \"numeric\" IF prop ARGUMENT IS TRUE")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
            }
        }
        if( ! base::is.null(typeof)){
            if(typeof != "double"){
                tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\ntypeof ARGUMENT CANNOT BE OTHER THAN NULL OR \"double\" IF prop ARGUMENT IS TRUE")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
            }
        }
    }
    if( ! (base::all(base::class(double_as_integer_allowed) == "logical") & base::length(double_as_integer_allowed) == 1L)){ # base::all() without na.rm -> ok because base::class() never returns NA
        tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nTHE double_as_integer_allowed ARGUMENT MUST BE TRUE OR FALSE ONLY: ", base::paste(double_as_integer_allowed, collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! (base::is.logical(all_options_in_data) & base::length(all_options_in_data) == 1L)){
        tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nall_options_in_data ARGUMENT MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY): ", base::paste(all_options_in_data, collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! (base::all(base::class(na_contain) == "logical") & base::length(na_contain) == 1L)){ # base::all() without na.rm -> ok because base::class() never returns NA
        tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nTHE na_contain ARGUMENT MUST BE TRUE OR FALSE ONLY: ", base::paste(na_contain, collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! (base::all(base::class(print) == "logical") & base::length(print) == 1L)){ # base::all() without na.rm -> ok because base::class() never returns NA
        tempo.cat <- base::paste0("ERROR IN ", function_name, "() OF THE ", package_name, " PACKAGE", base::ifelse(base::is.null(fun_name), "", base::paste0(" INSIDE ", fun_name, base::ifelse(test = base::is.null(pack_name), yes = "", no = base::paste0(" OF THE PACKAGE ", pack_name)))), "\nTHE print ARGUMENT MUST BE TRUE OR FALSE ONLY: ", base::paste(print, collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # data_name and fun_name tested at the beginning
    # end other checkings
    # end second round of checking and data preparation

    # main code
    if(base::is.null(data_name)){
        data_name <- base::deparse(base::substitute(data))
    }
    problem <- FALSE
    text <- base::paste0(base::ifelse(base::is.null(fun_name), "", base::paste0("IN ", fun_name, "\n")), "NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT")
    if(( ! base::is.null(options)) & (base::all(base::typeof(data) == "character") | base::all(base::typeof(data) == "integer") | base::all(base::typeof(data) == "double"))){ # base::all() without na.rm -> ok because base::typeof() never returns NA
        test.log <- TRUE
        if(base::all(base::typeof(data) == "double")){
            if( ! base::all(data %% 1 == 0L, na.rm = TRUE)){
                problem <- TRUE
                text <- base::paste0(base::ifelse(base::is.null(fun_name), "ERROR", base::paste0("ERROR IN ", fun_name)), "\nTHE ", data_name, " OBJECT MUST BE SOME OF THESE OPTIONS: ", base::paste(options, collapse = " "), "\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER")
                test.log <- FALSE
            }
        }
        if(test.log == TRUE){
            text <- ""
            if( ! base::all(data %in% options)){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
                problem <- TRUE
                text <- base::paste0(base::ifelse(base::is.null(fun_name), "ERROR", base::paste0("ERROR IN ", fun_name)), "\nTHE ", data_name, " OBJECT MUST BE SOME OF THESE OPTIONS: ", base::paste(options, collapse = " "), "\nTHE PROBLEMATIC ELEMENTS OF ", data_name, " ARE: ", base::paste(base::unique(data[ ! (data %in% options)]), collapse = " "))
            }
            if(all_options_in_data == TRUE){
                if( ! base::all(options %in% data)){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
                    problem <- TRUE
                    text <- base::paste0(base::ifelse(text == "", "", base::paste0(text, "\n")), base::ifelse(base::is.null(fun_name), "ERROR", base::paste0("ERROR IN ", fun_name)), "\nTHE ", data_name, " OBJECT MUST BE MADE OF ALL THESE OPTIONS: ", base::paste(options, collapse = " "), "\nTHE MISSING ELEMENTS OF THE options ARGUMENT ARE: ",  base::paste(base::unique(options[ ! (options %in% data)]), collapse = " "))
                }
            }
            if( ! base::is.null(length)){
                if(base::length(data) != length){
                    problem <- TRUE
                    text <- base::paste0(base::ifelse(text == "", "", base::paste0(text, "\n")), base::ifelse(base::is.null(fun_name), "ERROR", base::paste0("ERROR IN ", fun_name)), "\nTHE LENGTH OF ", data_name, " MUST BE ", length, " AND NOT ", base::length(data))
                }
            }
            if(text == ""){
                text <- base::paste0(base::ifelse(base::is.null(fun_name), "", base::paste0("IN ", fun_name, "\n")), "NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT")
            }
        }
    }else if( ! base::is.null(options)){
        problem <- TRUE
        text <- base::paste0(base::ifelse(base::is.null(fun_name), "ERROR", base::paste0("ERROR IN ", fun_name)), "\nTHE ", data_name, " OBJECT MUST BE SOME OF THESE OPTIONS: ", base::paste(options, collapse = " "), "\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER")
    }
    arg.names <- base::c("class", "typeof", "mode", "length")
    if( ! base::is.null(class)){
        if(class == "matrix"){ # because of base::class(base::matrix()) since R4.0.0
            class <- base::c("matrix", "array")
        }else if(class == "factor" & base::all(base::class(data) %in% base::c("factor", "ordered"))){ # to deal with ordered factors # base::all() without na.rm -> ok because base::class(NA) is "logical"
            class <- base::c("factor", "ordered")
        }
    }
    if(base::is.null(options)){
        for(i2 in 1:base::length(arg.names)){
            if( ! base::is.null(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE))){
                # script to execute
                tempo.script <- '
                    problem <- TRUE ;
                    if(base::identical(text, base::paste0(base::ifelse(base::is.null(fun_name), "", base::paste0("IN ", fun_name, "\n")), "NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT"))){
                        text <- base::paste0(base::ifelse(base::is.null(fun_name), "ERROR", base::paste0("ERROR IN ", fun_name)), "\nTHE ", data_name, " OBJECT MUST BE ") ;
                    }else{
                        text <- base::paste0(text, " AND ") ; 
                    }
                    text <- base::paste0(text, base::toupper(arg.names[i2]), " ", if(base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("matrix", "array"))){"matrix"}else if(base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("factor", "ordered"))){"factor"}else{base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE)})
                ' # no need of na.rm = TRUE for base::all() because %in% does not output NA
                # end script to execute
                if(base::typeof(data) == "double" & double_as_integer_allowed == TRUE & ((arg.names[i2] == "class" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")) | (arg.names[i2] == "typeof" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")))){ # no need of na.rm = TRUE for base::all() because == does not output NA if no NA in left of ==, which is the case for arg.names # base::typeof(data) == "double" means no factor allowed
                    if( ! base::all(data %% 1 == 0L, na.rm = TRUE)){ # to check integers (use %%, meaning the remaining of a division): see the precedent line. base::isTRUE(base::all.equal(data%%1, base::rep(0, base::length(data)))) not used because we strictly need zero as a result. Warning: na.rm = TRUE required here for base::all()
                        base::eval(base::parse(text = tempo.script)) # execute tempo.script
                    }
                }else if( ! base::any(base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg.names[i2], "(data)"))) %in% base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE))){ # test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function. No need of na.rm = TRUE for base::all() because %in% does not output NA # no need of na.rm = TRUE for base::all() because %in% does not output NA # no need of na.rm = TRUE for base::any() because get base::get(arg.names) does not contain NA
                    base::eval(base::parse(text = tempo.script)) # execute tempo.script
                }else if(arg.names[i2] == "class" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "vector") & ! (base::all(base::class(data) %in% "numeric") | base::all(base::class(data) %in% "integer") | base::all(base::class(data) %in% "character") | base::all(base::class(data) %in% "logical") | base::all(base::class(data) %in% "complex") | base::all(base::class(data) %in% "expression"))){ # test class == "vector". base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) check is user as used the argument class = "vector". If TRUE and base::length(data) > 1, the class "numeric" "integer" "character" "logical" "complex" "expression" should be returned. No need of na.rm = TRUE for base::all() because %in% does not output NA # no need of na.rm = TRUE for base::all() because == does not output NA if no NA in left of ==, which is the case for arg.names. Other classes "list", "name", "symbol", "function", "environment", "S4", "call" return a list if length of data > 1
                    base::eval(base::parse(text = tempo.script)) # execute tempo.script
                }else if(arg.names[i2] == "class" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "ggplot2") & ! base::all(base::class(data) %in% base::c("gg", "ggplot"))){ # test ggplot object # no need of na.rm = TRUE for base::all() because == does not output NA if no NA in left of ==, which is the case for arg.names # no need of na.rm = TRUE for base::all() because %in% does not output NA
                    base::eval(base::parse(text = tempo.script)) # execute tempo.script
                }
            }
        }
    }
    if(prop == TRUE & base::all(base::typeof(data) == "double")){ # base::all() without na.rm -> ok because base::typeof(NA) is "logical"
        if(base::is.null(data) | base::any(data < 0 | data > 1, na.rm = TRUE)){ # works if data base::is.null # Warning: na.rm = TRUE required here for base::any() # base::typeof(data) == "double" means no factor allowed
            problem <- TRUE
            if(base::identical(text, base::paste0(base::ifelse(base::is.null(fun_name), "", base::paste0("IN ", fun_name, "\n")), "NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT"))){
                text <- base::paste0(base::ifelse(base::is.null(fun_name), "ERROR", base::paste0("ERROR IN ", fun_name)), "\n")
            }else{
                text <- base::paste0(text, " AND ")
            }
            text <- base::paste0(text, "THE ", data_name, " OBJECT MUST BE DECIMAL VALUES BETWEEN 0 AND 1")
        }
    }else if(prop == TRUE){
        problem <- TRUE
        if(base::identical(text, base::paste0(base::ifelse(base::is.null(fun_name), "", base::paste0("IN ", fun_name, "\n")), "NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT"))){
            text <- base::paste0(base::ifelse(base::is.null(fun_name), "ERROR", base::paste0("ERROR IN ", fun_name)), "\n")
        }else{
            text <- base::paste0(text, " AND ")
        }
        text <- base::paste0(text, "THE ", data_name, " OBJECT MUST BE DECIMAL VALUES BETWEEN 0 AND 1")
    }
    if(base::all(base::class(data) %in% "expression")){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
        data <- base::as.character(data) # to evaluate the presence of NA
    }
    if(na_contain == FALSE & (base::mode(data) %in% base::c("logical", "numeric", "complex", "character", "list"))){ # before it was ! (base::class(data) %in% base::c("function", "environment"))
        if(base::any(base::is.na(data)) == TRUE){ # not on the same line because when data is class envir or function , do not like that # normally no NA with base::is.na()
            problem <- TRUE
            if(base::identical(text, base::paste0(base::ifelse(base::is.null(fun_name), "", base::paste0("IN ", fun_name, "\n")), "NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT"))){
                text <- base::paste0(base::ifelse(base::is.null(fun_name), "ERROR", base::paste0("ERROR IN ", fun_name)), "\n")
            }else{
                text <- base::paste0(text, " AND ")
            }
            text <- base::paste0(text, "THE ", data_name, " OBJECT CONTAINS NA WHILE NOT AUTHORIZED")
        }
    }
    if(neg_values == FALSE & base::all(base::mode(data) %in% "numeric") & ! base::any(base::class(data) %in% "factor")){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
        if(base::any(data < 0, na.rm = TRUE)){ # Warning: na.rm = TRUE required here for base::any()
            problem <- TRUE
            if(base::identical(text, base::paste0(base::ifelse(base::is.null(fun_name), "", base::paste0("IN ", fun_name, "\n")), "NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT"))){
                text <- base::paste0(base::ifelse(base::is.null(fun_name), "ERROR", base::paste0("ERROR IN ", fun_name)), "\n")
            }else{
                text <- base::paste0(text, " AND ")
            }
            text <- base::paste0(text, "THE ", data_name, " OBJECT MUST BE MADE OF NON NEGATIVE NUMERIC VALUES")
        }
    }else if(neg_values == FALSE){
        problem <- TRUE
        if(base::identical(text, base::paste0(base::ifelse(base::is.null(fun_name), "", base::paste0("IN ", fun_name, "\n")), "NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT"))){
            text <- base::paste0(base::ifelse(base::is.null(fun_name), "ERROR", base::paste0("ERROR IN ", fun_name)), "\n")
        }else{
            text <- base::paste0(text, " AND ")
        }
        text <- base::paste0(text, "THE ", data_name, " OBJECT MUST BE MADE OF NON NEGATIVE VALUES BUT IS ", base::ifelse(base::any(base::class(data) %in% "factor"), "A FACTOR", "NOT EVEN MODE NUMERIC")) # no need of na.rm = TRUE
    }
    if(inf_values == FALSE & base::all(base::typeof(data) %in% "double") & ! base::any(base::class(data) %in% "factor")){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
        if(base::any(base::is.infinite(data), na.rm = TRUE)){ # Warning: na.rm = TRUE required here for base::any()
            problem <- TRUE
            if(base::identical(text, base::paste0(base::ifelse(base::is.null(fun_name), "", base::paste0("IN ", fun_name, "\n")), "NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT"))){
                text <- base::paste0(base::ifelse(base::is.null(fun_name), "ERROR", base::paste0("ERROR IN ", fun_name)), "\n")
            }else{
                text <- base::paste0(text, " AND ")
            }
            text <- base::paste0(text, "THE ", data_name, " OBJECT MUST BE MADE OF NON INFINITE NUMERIC VALUES")
        }
    }else if(inf_values == FALSE){
        problem <- TRUE
        if(base::identical(text, base::paste0(base::ifelse(base::is.null(fun_name), "", base::paste0("IN ", fun_name, "\n")), "NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT"))){
            text <- base::paste0(base::ifelse(base::is.null(fun_name), "ERROR", base::paste0("ERROR IN ", fun_name)), "\n")
        }else{
            text <- base::paste0(text, " AND ")
        }
        text <- base::paste0(text, "THE ", data_name, " OBJECT MUST BE MADE OF NON INFINITE VALUES BUT IS ", base::ifelse(base::any(base::class(data) %in% "factor"), "A FACTOR", "NOT EVEN TYPE DOUBLE")) # no need of na.rm = TRUE
    }
    if(print == TRUE & problem == TRUE){
        base::cat(base::paste0("\n\n================\n\n", text, "\n\n================\n\n"))
    }
    # end main code
    
    # output
    # warning output
    # end warning output
    output <- base::list(problem = problem, text = text, object.name = data_name)
    base::return(output)
    # end output
}
