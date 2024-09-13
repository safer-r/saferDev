#' @title .pack_and_function_check
#' @description
#' Check if 1) required functions are present in required packages and 2) required packages are installed locally.
#' Simplified version of saferDev::is_function_here(), used as internal function for the other functions of the package.
#' @param fun Character vector of the names of the required functions, preceded by the name of the package they belong to and a double colon. Example: c("ggplot2::geom_point", "grid::gpar").
#' @param lib.path Character vector specifying the absolute pathways of the directories containing the listed packages in the fun argument, if not in the default directories. If NULL, the function checks only in the base::.libPaths() default R library folders.
#' @param external.function.name Name of the function using the .pack_and_function_check() function.
#' @param external.package.name Name of the package of the function using the .pack_and_function_check() function.
#' @returns An error message if at least one of the checked packages is missing in lib.path, or if at least one of the checked functions is missing in the required package, nothing otherwise.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function
#' .pack_and_function_check(fun = "ggplot2::notgood", lib.path = base::.libPaths(), external.function.name = "fun1") # this example returns an error
#' .pack_and_function_check(fun = c("ggplot2::geom_point", "grid::gpar"), lib.path = base::.libPaths(), external.function.name = "fun1")
#' }
#' @keywords internal
#' @rdname internal_function
.pack_and_function_check <- function(
    fun, 
    lib.path,
    external.function.name,
    external.package.name
){
    # AIM
    # Check for the presence of required package::functions in the system  
    # WARNING
    # arguments of the .pack_and_function_check() function are not checked, so use carefully inside other functions
    # ARGUMENTS
    # fun: vector of string of the package::function names to check
    # lib.path: path of the library folder in the system
    # external.function.name: function name
    # external.package.name: package name
    # RETURN
    # An error message or nothing 
    # DEBUGGING
    # fun = "ggplot2::geom_point" ; lib.path = "C:/Program Files/R/R-4.3.1/library" ; external.function.name = "fun1" ; external.package.name = "1"
    # check of lib.path
    # full check already done in the main function
    if(base::is.null(lib.path)){
        lib.path <- base::.libPaths() # base::.libPaths(new = lib.path) # or base::.libPaths(new = c(base::.libPaths(), lib.path))
    }
    # end check of lib.path
    # main code
    tempo.log <- base::grepl(x = fun, pattern = "^.+::.+$")
    if( ! base::all(tempo.log)){
        tempo.cat <- base::paste0("ERROR IN THE CODE OF THE ", external.function.name, " OF THE ", external.package.name, " PACKAGE\nTHE STRING IN fun ARGUMENT MUST CONTAIN \"::\":\n", base::paste(fun[ ! tempo.log], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    pkg.fun.name.list <- base::strsplit(fun, "::") # package in 1 and function in 2
    pkg.name <- base::sapply(X = pkg.fun.name.list, FUN = function(x){x[1]})
    pkg.log <- pkg.name %in% base::rownames(utils::installed.packages(lib.loc = lib.path))
    if( ! base::all(pkg.log)){
        tempo <- pkg.name[ ! pkg.log]
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            external.function.name, 
            " OF THE ", external.package.name, " PACKAGE\nREQUIRED PACKAGE", 
            base::ifelse(base::length(tempo) == 1L, base::paste0(":\n", tempo), base::paste0("S:\n", base::paste(tempo, collapse = "\n"))), 
            "\nMUST BE INSTALLED IN", 
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
            external.function.name, 
            " OF THE ", external.package.name, " PACKAGE\nREQUIRED FUNCTION",
            base::ifelse(base::length(tempo) == 1L, " IS ", "S ARE "), 
            "MISSING IN THE INSTALLED PACKAGE", 
            base::ifelse(base::length(tempo) == 1L, base::paste0(":\n", tempo), base::paste0("S:\n", base::paste(tempo, collapse = "\n")))
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end main code
}


#' @title .base_op_check
#' @description
#' Check if critical operators of R are not present in other packages or in the global env.
#' Others functions of the R scope can be overwritten because safer functions always use :: when using any function.
#' @param external.function.name Name of the function using the .pack_and_function_check() function.
#' @param external.package.name Name of the package of the function using the .pack_and_function_check() function.
#' @returns An error message if at least one of the checked operator is present in the R scope, nothing otherwise.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' \dontrun{
#' # Example that shouldn't be run because this is an internal function
#' assign("!", 1) ; .base_op_check(external.function.name = "fun1") # commented because this example returns an error
#' }
#' @keywords internal
#' @rdname internal_function
.base_op_check <- function(
    external.function.name,
    external.package.name
){
    # AIM
    # Check if basic operator names have been used in the scope of the opened environement
    # WARNING
    # arguments of the .base_op_check() function are not checked, so use carefully inside other functions
    # ARGUMENTS
    # external.function.name: function name
    # external.package.name: package name
    # RETURN
    # An error message or nothing 
    # DEBUGGING
    # external.function.name = "test" ; external.package.name = "p1"
    # main code
    reserved.objects <- base::c(
        "-", 
        "!", 
        "!=", 
        "$", 
        "%%", 
        "%*%", 
        "%/%", 
        "%in%", 
        "&", 
        "&&", 
        "(", 
        "*", 
        "/", 
        ":", 
        "::", 
        ":::", 
        "@", 
        "[", 
        "[[", 
        "^", 
        "{", 
        "|", 
        "||", 
        "~", 
        "+", 
        "<", 
        "<-", 
        "<<-", 
        "<=", 
        "=", 
        "==", 
        ">", 
        ">=", 
        "\\", 
        "if", 
        "else", 
        "function",
        "for",
        "while",
        "repeat"
    )
    tempo.log <- base::sapply(X = reserved.objects, FUN = function(x){ 
        if( ! base::all(utils::find(x) == "package:base")){
            base::return(TRUE)
        }else{
            base::return(FALSE)
        }
    })
    if(base::any(tempo.log)){
        tempo.name <-  reserved.objects[tempo.log]
        tempo.pos <- base::sapply(X = tempo.name, FUN = function(x){base::paste(utils::find(x), collapse = " ")})
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            external.function.name, 
            " OF THE ", external.package.name, " PACKAGE\nCRITICAL R OBJECT",
            base::ifelse(base::length(tempo.log) == 1L, " ", "S "), 
            "CANNOT BE PRESENT SOMEWHERE ELSE IN THE R SCOPE THAN IN \"package::base\":\n", 
            base::paste(base::paste(tempo.name, tempo.pos, sep = "\t"), collapse = "\n")
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end main code
}

#' @title .has_odd_number_of_quotes
#' @description
#' Determine if a string has a odd number of quotes.
#' @param input_string Single string.
#' @param pattern: Either '"' or "'".
#' @returns TRUE or FALSE.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function
#' .has_odd_number_of_quotes(input_string = 'This is a "test" string with "even" quotes', pattern = '"')
#' }
#' @keywords internal
#' @rdname internal_function
.has_odd_number_of_quotes <- function(
    input_string, 
    pattern = '"'
){
    # AIM
    # determine if a string has a odd number of quotes
    # ARGUMENTS
    # input_string: single strings
    # pattern: either '"' or "'"
    # RETURN
    # TRUE or FALSE
    # DEBUGGING
    # input_string = 'This is a "test" string with "even" quotes' ; pattern = '"'
    # input_string = "This is a 'test' string with 'even' quotes" ; pattern = "'"
    # Count the number of quote characters
    quote_count <- base::gregexpr(pattern, input_string)[[1]]
    if (quote_count[1] == -1) {
        quote_count <- 0
    } else {
        # Length of the vector gives the count of quotes
        quote_count <- base::length(quote_count)
    }
    # Check if the count of quotes is odd
    output <- quote_count %% 2 == 1
    base::return(output)
}


#' @title .extract_all
#' @description
#' Extract all function names.
#' @param text A vector of strings.
#' @param pattern: A perl regex to extract function names.
#' @returns A list containing the functions names, each compartment being one of the string of the input vector.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function
#' .extract_all(text = 'This is a test string with sum()', pattern = "[a-zA-Z.][a-zA-Z0-9._]*\\s*\\(")
#' }
#' @keywords internal
#' @rdname internal_function
.extract_all <- function(
    text, 
    pattern
){
    # AIM
    # extract all function names
    # ARGUMENTS
    # text: vector of strings
    # pattern: regex to extract function names
    # RETURN
    # A list containing the functions names, each compartment being one of the string of the input vector
    # DEBUGGING
    # text = ini[19] ; pattern = pattern1
    # Find all matches, including trailing '('
    matches <- base::gregexpr(pattern = pattern, text = text, perl = TRUE)
    matched_strings <- base::regmatches(x = text, m = matches)[[1]]
    # Remove trailing '(' from each match
    result <- base::sub(pattern = "\\s*\\($", replacement = "", x = matched_strings, perl = TRUE)
    base::return(result)
}



#' @title .create_message
#' @description
#' Create the message for the colons_check() function.
#' @param list.fun list of names of all the basic functions.
#' @param list.fun.uni vector of all the unique function names.
#' @param list.line.nb vector of corresponding line number.
#' @param ini vector of string of the initial function code analyzed.
#' @param arg.user.setting list of arg user settings.
#' @param function.name function name.
#' @param package.name package name.
#' @param text either "BASIC" or "OTHER".
#' @param internal_fun_names vector of string of names of internal functions in the function code analyzed.
#' @returns
#'  A list:
#'  $output.cat: the message (string).
#'  $colon_not_here: logical vector. Does list.fun contain function names without :: or ::: ?
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @keywords internal
#' @rdname internal_function
.create_message <- function(
    list.fun, 
    list.fun.uni, 
    list.line.nb, 
    ini, 
    arg.user.setting, 
    function.name, 
    package.name, 
    text,
    internal_fun_names
){
    # AIM
    # create the message for the colons_check() function
    # ARGUMENTS
    # list.fun: list of names of all the basic functions
    # list.fun.uni: vector of all the unique function names
    # list.line.nb: vector of corresponding line number
    # ini: vector of string of the initial function code analyzed
    # arg.user.setting: list of arg user settings
    # function.name: function name
    # package.name: package name
    # text: either "BASIC" or "OTHER"
    # internal_fun_names: vector of string of names of internal functions in the function code analyzed
    # RETURN
    # A list 
    # $output.cat: the message (string)
    # $colon_not_here: logical vector. Does list.fun contain function names without :: or ::: ?
    # DEBUGGING
    # list.fun = in_basic_fun ; list.fun.uni = in_basic_fun_uni ; list.line.nb = in_basic_code_line_nb ; ini = ini ; arg.user.setting = arg.user.setting ; function.name = function.name ; package.name = package.name ; text = "BASIC" ; internal_fun_names = internal_fun_names
    # list.fun = in_other_fun ; list.fun.uni = in_other_fun_uni ; list.line.nb = in_other_code_line_nb ; ini = ini ; arg.user.setting = arg.user.setting ; function.name = function.name ; package.name = package.name ; text = "OTHER" ; internal_fun_names = internal_fun_names
    if(base::length(text) != 1 & base::any( ! text %in% base::c("BASIC", "OTHER"))){
        tempo.cat <- base::paste0("INTERNAL ERROR 1 IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE text ARGUMENT OF create_message MUST BE \"BASIC\" OR \"OTHER\".\nTHE PROBLEM IS:\n",
            base::paste(text, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    pattern2 <- base::paste(base::paste0("(?<![A-Za-z0-9._])", list.fun.uni, "\\s*\\("), collapse = "|") # to split string according to basic function name as splitter. Pattern (?<![A-Za-z0-9._]) means "must not be preceeded by any alphanum or .or _
    pattern3 <- base::paste(base::paste0("(?<![A-Za-z0-9._])", list.fun.uni, "\\s*\\($"), collapse = "|") # same as pattern2 but used to know if the seeked function is at the end of the string
    basic_ini <- ini[list.line.nb]
    res <- base::strsplit(x = basic_ini, split = pattern2, perl = TRUE) # in res, all the strings should finish by ::
    tempo.log <- ! base::grepl(x = basic_ini, pattern = pattern3, perl = TRUE) # strings of basic_ini that does not finish by the function name
    # in each compartment of res, the last split section is removed because nothing to test at the end (end of code)
    if(base::sum(tempo.log, na.rm = TRUE) > 0){
        res[tempo.log] <- base::lapply(X = res[tempo.log], FUN = function(x){x[-base::length(x)]})
    }
    # end in each compartment of res, the last split section is removed because nothing to test at the end (end of code)
    res2 <- base::lapply(X = res, FUN = function(x){base::substr(x, base::nchar(x)-1, base::nchar(x))}) # base::nchar(x)-1 takes only :: if the strings ends by :::
    base::names(res2) <- NULL
    if( ! base::all(base::sapply(X = res2, FUN = function(x){base::length(x)}) == base::sapply(X = res, FUN = function(x){base::length(x)}))){
        tempo.cat <- base::paste0("INTERNAL ERROR 2 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL\nres2: ", base::paste(base::sapply(X = res2, FUN = function(x){base::length(x)}), collapse = " "), "\nres: ", base::paste(base::sapply(X = res, FUN = function(x){base::length(x)}), collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    colon_not_here <- base::lapply(X = res2, FUN = function(x){ ! x %in% "::"}) # no need to check for ":::" because base::nchar(x)-1 takes only :: if the strings ends by :::
    if( ! base::all(base::sapply(X = res2, FUN = function(x){base::length(x)}) == base::sapply(X = colon_not_here, FUN = function(x){base::length(x)}))){
        tempo.cat <- base::paste0("INTERNAL ERROR 3 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL\nres2: ", base::paste(base::sapply(X = res2, FUN = function(x){base::length(x)}), collapse = " "), "\ncolon_not_here: ", base::paste(base::sapply(X = colon_not_here, FUN = function(x){base::length(x)}), collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if(base::any(base::unlist(colon_not_here))){
        col1 <- as.vector(base::unlist(base::mapply(FUN = function(x, y){base::rep(y, base::sum(x))}, x = colon_not_here, y = list.line.nb)))
        col2 <- as.vector(base::unlist(base::mapply(FUN = function(x, y){y[x]}, x = colon_not_here, y = list.fun)))
        col3 <- as.vector(base::unlist(base::mapply(FUN = function(x, y){y[x]}, x = colon_not_here, y = res)))
        if( ! (base::length(col1) == base::length(col2) & base::length(col1) == base::length(col3) & base::length(col2) == base::length(col3))){
            tempo.cat <- base::paste0("INTERNAL ERROR 4 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS OF col1 (", base::length(col1), "), col2 (", base::length(col2), "), AND col3 (", base::length(col3), "), SHOULD BE EQUAL\n")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        # removal of a$fun() pattern
        if(base::length(col1) > 0){
            tempo.log <- base::grepl(x = col3, pattern = "[a-zA-Z.][a-zA-Z0-9._]* *\\$ *") 
            if(base::any(tempo.log, na.rm = TRUE)){
                col1 <- col1[ ! tempo.log]
                col2 <- col2[ ! tempo.log]
                col3 <- col3[ ! tempo.log]
            }
        }else{
            colon_not_here <- FALSE
            output.cat <- NULL
        }
        # end removal of a$fun() pattern
        # removal of functions between quotes
        if(base::length(col1) > 0){
            tempo.ini.order <- 1:base::length(col1) # to recover the initial order at the end
            tempo.order <- base::order(col2) # order according to function name
            tempo.ini.order <- tempo.ini.order[tempo.order]
            tempo.col1 <- col1[tempo.order] # reorder to work only once with duplicated functions
            tempo.col2 <- col2[tempo.order] # reorder to work only once with duplicated functions
            tempo.col3 <- col3[tempo.order] # reorder to work only once with duplicated functions
            tempo.ini <- ini
            pos.rm <- NULL # positions to remove (functions between quotes)
            for(i3 in 1:base::length(tempo.col1)){
                lines.split <- base::strsplit(tempo.ini[tempo.col1[i3]], split = tempo.col2[i3])[[1]][1]
                # if odds number of quotes, it means that # has broken the string in the middle of a quoted part
                double.quote.test <- .has_odd_number_of_quotes(input_string = lines.split, pattern = '"') # here FALSE means even number of quotes, thus that the function is not between quotes, thus has to be kept. TRUE means that the function is between quotes, thus has to be removed
                simple.quote.test <- .has_odd_number_of_quotes(input_string = lines.split, pattern = "'") # idem
                odds.quotes.log <- double.quote.test |  simple.quote.test
                if(odds.quotes.log == TRUE){
                    pos.rm <- base::c(pos.rm, i3)
                }else{
                    pos.rm <- base::c(pos.rm, NA) #becomes double if integer added, otherwise remains logical. Thus, do not use any()
                }
                tempo.ini[tempo.col1[i3]] <- base::sub(pattern = tempo.col2[i3], replacement = "", x = tempo.ini[tempo.col1[i3]], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove the first fonction in the line, in case of identical function names in a code line. Like, that, the next round for the next same function can be easily tested for "between quotes" 
            }
            # initial order
            pos.rm.fin <- pos.rm[order(tempo.ini.order)]
            pos.rm.fin2 <- pos.rm.fin[ ! is.na(pos.rm.fin)]
            # end initial order
            if(base::length(pos.rm.fin2) > 0){
                col1 <- col1[-pos.rm.fin2]
                col2 <- col2[-pos.rm.fin2]
                col3 <- col3[-pos.rm.fin2]
            }
        }
        # end removal of functions between quotes
        if(base::length(col1) > 0){
            tempo.pos <- base::paste0(col1, "\t", col2, "\t\t", col3)
            output.cat <- base::paste0(
                "INSIDE ", arg.user.setting$x, "(), SOME :: OR ::: ARE MISSING AT ", text, " FUNCTION POSITIONS:\n\n", 
                "LINE\tFUN\t\tSTRING_BEFORE\n",
                base::paste(tempo.pos, collapse = "\n")
            )
        }else{
            output.cat <- NULL
            colon_not_here <- FALSE
        }
    }else{
        output.cat <- NULL
    }
    if(text == "OTHER" & base::length(internal_fun_names) > 0){
        output.cat <- base::paste0(
            "INSIDE ", arg.user.setting$x, "(), INTERNAL FUNCTION", base::ifelse(base::length(internal_fun_names) == 1, "", "S"), " DETECTED:\n", 
            base::paste(internal_fun_names, collapse = "\n"), 
            "\n\n", 
            output.cat
        )
    }
    base::return(base::list(output.cat = output.cat, colon_not_here = base::unlist(colon_not_here)))
}

