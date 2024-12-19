#' @title .pack_and_function_check
#' @description
#' Check if 1) required functions are present in required packages and 2) required packages are installed locally.
#' Simplified version of saferDev::is_function_here(), used as internal function for the other functions of the package.
#' @param fun Character vector of the names of the required functions, preceded by the name of the package they belong to and a double or triple colon. Example: c("ggplot2::geom_point", "grid::gpar").
#' @param lib_path Character vector specifying the absolute pathways of the directories containing the listed packages in the fun argument, if not in the default directories. If NULL, the function checks only in the base::.libPaths() default R library folders.
#' @param external_function_name Name of the function using this internal function.
#' @param external_package_name Name of the package of the function using this internal function.
#' @param internal_error_report_link String of the report link for internal error messages.
#' @returns An error message if at least one of the checked packages is missing in lib_path, or if at least one of the checked functions is missing in the required package, nothing otherwise.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function
#' .pack_and_function_check(fun = "ggplot2::notgood", lib_path = base::.libPaths(), external_function_name = "F1", external_package_name = "P1") # this example returns an error
#' .pack_and_function_check(fun = c("ggplot2::geom_point", "grid::gpar"), lib_path = base::.libPaths(), external_function_name = "F1", external_package_name = "P1")
#' }
#' @keywords internal
#' @rdname internal_function
.pack_and_function_check <- function(
    fun, 
    lib_path,
    external_function_name,
    external_package_name,
    internal_error_report_link
){
    # AIM
    # Check for the presence of required package::functions in the system  
    # WARNING
    # arguments of the .pack_and_function_check() function are not checked, so use carefully inside other functions
    # ARGUMENTS
    # fun: vector of string of the package::function names to check
    # lib_path: path of the library folder in the system
    # external_function_name: function name
    # external_package_name: package name
    # RETURN
    # An error message or nothing 
    # DEBUGGING
    # fun = "ggplot2::geom_point" ; lib_path = "C:/Program Files/R/R-4.3.1/library" ; external_function_name = "fun1" ; external_package_name = "1"
    # fun = "saferDev:::.colons_check_message" ; lib_path = "C:/Program Files/R/R-4.3.1/library" ; external_function_name = "fun1" ; external_package_name = "1"
    # check of lib_path
    # full check already done in the main function
    if(base::is.null(lib_path)){
        lib_path <- base::.libPaths() # base::.libPaths(new = lib_path) # or base::.libPaths(new = c(base::.libPaths(), lib_path))
    }
    # end check of lib_path
    # main code
    tempo.log <- base::grepl(x = fun, pattern = "^[a-zA-Z][a-zA-Z0-9.]*(:{2}[a-zA-Z]|:{3}\\.[a-zA-Z._])[a-zA-Z0-9._]*$")
    # [a-zA-Z][a-zA-Z0-9.]+ means any single alphabet character (package name cannot start by dot or underscore or num), then any alphanum and dots
    # (:{2}[a-zA-Z]|:{3}\\.[a-zA-Z._]) means either double colon and any single alphabet character or triple colon followed by a dot and any single alphabet character or dot (because .. is ok for function name) or underscore (because ._ is ok for function name). Starting "dot and num" or underscore is not authorized for function name
    # [a-zA-Z0-9._]* means any several of these characters or nothing
    if( ! base::all(tempo.log)){
        tempo.cat <- base::paste0("INTERNAL ERROR 1 IN .pack_and_function_check() INSIDE ", external_function_name, " OF THE ", external_package_name, " PACKAGE\nTHE STRING IN fun ARGUMENT MUST CONTAIN \"::\" OR \":::.\":\n", base::paste(fun[ ! tempo.log], collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    pkg.fun.name.list <- base::strsplit(x = fun, split = ":{2,3}") # package in 1 and function in 2
    pkg.name <- base::sapply(X = pkg.fun.name.list, FUN = function(x){x[1]})
    pkg.log <- pkg.name %in% base::rownames(utils::installed.packages(lib.loc = lib_path))
    if( ! base::all(pkg.log)){
        tempo <- pkg.name[ ! pkg.log]
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            external_function_name, 
            " OF THE ", external_package_name, " PACKAGE\nREQUIRED PACKAGE", 
            base::ifelse(base::length(tempo) == 1L, base::paste0(":\n", tempo), base::paste0("S:\n", base::paste(tempo, collapse = "\n"))), 
            "\nMUST BE INSTALLED IN", 
            base::ifelse(base::length(lib_path) == 1L, "", " ONE OF THESE FOLDERS"), 
            ":\n", 
            base::paste(lib_path, collapse = "\n")
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    fun.log <- base::sapply(X = pkg.fun.name.list, FUN = function(x){base::exists(x[2], envir = base::getNamespace(x[1]), inherits = FALSE)})
    if( ! base::all(fun.log)){
        tempo <- fun[ ! fun.log]
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            external_function_name, 
            " OF THE ", external_package_name, " PACKAGE\nREQUIRED FUNCTION",
            base::ifelse(base::length(tempo) == 1L, " IS ", "S ARE "), 
            "MISSING IN THE INSTALLED PACKAGE", 
            base::ifelse(base::length(tempo) == 1L, base::paste0(":\n", tempo), base::paste0("S:\n", base::paste(tempo, collapse = "\n"))),
            "\n\nIN", 
            base::ifelse(base::length(lib_path) == 1L, "", " ONE OF THESE FOLDERS"), 
            ":\n", 
            base::paste(lib_path, collapse = "\n")
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end main code
}


#' @title .base_op_check
#' @description
#' Check if critical operators of R are not present in other packages or in the global env.
#' Others functions of the R scope can be overwritten because safer functions always use :: when using any function.
#' @param external_function_name Name of the function using the .pack_and_function_check() function.
#' @param external_package_name Name of the package of the function using the .pack_and_function_check() function.
#' @returns An error message if at least one of the checked operator is present in the R scope, nothing otherwise.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' \dontrun{
#' # Example that shouldn't be run because this is an internal function
#' assign("!", 1) ; .base_op_check(external_function_name = "fun1") # commented because this example returns an error
#' }
#' @keywords internal
#' @rdname internal_function
.base_op_check <- function(
    external_function_name,
    external_package_name
){
    # AIM
    # Check if basic operator names have been used in the scope of the opened environement
    # WARNING
    # arguments of the .base_op_check() function are not checked, so use carefully inside other functions
    # ARGUMENTS
    # external_function_name: function name
    # external_package_name: package name
    # RETURN
    # An error message or nothing 
    # DEBUGGING
    # external_function_name = "f1" ; external_package_name = "p1"
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
            external_function_name, 
            " OF THE ", external_package_name, " PACKAGE\nCRITICAL R OBJECT",
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


#' @title .extract_all_fun_names
#' @description
#' Extract all function names.
#' @param text A single strings.
#' @param pattern: A perl regex to extract function names.
#' @returns A list containing:
#' $string: the function names without parenthesis.
#' $pos: position of the first character of the function names in the input string
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function
#' .extract_all_fun_names(text = 'This is a test string with sum()', pattern = "[a-zA-Z.][a-zA-Z0-9._]*\\s*\\(")
#' }
#' @keywords internal
#' @rdname internal_function
.extract_all_fun_names <- function(
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
    # text = ini[20] ; pattern = pattern1
    # Find all matches, including trailing '('
    matches <- base::gregexpr(pattern = pattern, text = text, perl = TRUE)
    att <- base::attributes(matches[[1]]) # attributes
    pos <- base::as.vector(att$capture.start)
    matched_strings <- base::regmatches(x = text, m = matches)[[1]]
    # Remove trailing '(' from each match
    string <- base::sub(pattern = "\\s*\\($", replacement = "", x = matched_strings, perl = TRUE)
    # end Remove trailing '(' from each match
    base::return(base::list(string = string, pos = pos))
}





#' @title .in_quotes_replacement
#' @description
#' Replace any pattern inside simple ou double quotes by another replacement pattern and get the position of replacements
#' @param string Single string.
#' @param pattern Single string indicating the pattern to detect. Warning : must be very simple pattern, like "\\(".
#' @param no_regex_pattern Single string of the pattern to detect but without escape characters or list, etc.
#' @param replacement Single string for pattern replacement. Is not regex.
#' @param perl Single logical value. Use Perl regex in pattern ?
#' @param function_name Name of the function using this internal function.
#' @param package_name Name of the package of the function using this internal function.
#' @param internal_error_report_link String of the report link for internal error messages.
#' @returns A list containing:
#' $string: The input string with all pattern replaced by the replacement pattern.
#' $pos: vector of the positions of the 1rst character of the replaced pattern. NULL if no replaced pattern. In that case, $string is identical to the input string.
#' @details
#' Warning : must be very simple pattern, like "\\(".
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function
#' source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") ; .in_quotes_replacement(string = paste(deparse(test), collapse = ""), pattern = "\\)", no_regex_pattern = ")", replacement = " ", perl = TRUE, function_name = "F1", package_name = "P1")
#' .in_quotes_replacement(string = 'paste0("IAGE((", paste0(1:3, collapse = " "), "A)B()")', pattern = "\\)", no_regex_pattern = ")", replacement = " ", perl = TRUE, function_name = "F1", package_name = "P1")
#' }
#' @keywords internal
#' @rdname internal_function
.in_quotes_replacement <- function(
    string, 
    pattern, 
    no_regex_pattern, 
    replacement, 
    perl,
    function_name,
    package_name,
    internal_error_report_link
){
    # DEBUGGING
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") ; string = paste(deparse(test), collapse = "") ; pattern = "\\)" ; no_regex_pattern = ")" ; replacement = " " ; perl = FALSE ; function_name = "F1" ; package_name = "P1"
    # string = 'paste0("IAGE((", paste0(1:3, collapse = " "), "A)B()")' ; pattern = "\\)" ; no_regex_pattern = ")" ; replacement = " " ; perl = FALSE ; function_name = "F1" ; package_name = "P1"
    # string = 'paste0("IAGE((", paste0(1:3, collapse = " "), "A)B()' ; pattern = "\\)" ; no_regex_pattern = ")" ; replacement = " " ; perl = FALSE ; function_name = "F1" ; package_name = "P1" # last is ) between strings because odds number in front of
    # string = '\"INTERNAL ERROR 4 IN \", function_name, \" OF THE \", package_name, \" PACKAGE\\nLENGTHS OF col1 (\", base::length(roc1()), \"), col2 (\", base::length(roc2), \"), AND col3 (\", base::length(roc3), \"), SHOULD BE EQUAL\\n\"' ; pattern = "," ; no_regex_pattern = "," ; replacement = " " ; perl = FALSE ; function_name = "F1" ; package_name = "P1"
    if(base::nchar(no_regex_pattern) != base::nchar(replacement)){
        tempo.cat <- base::paste0("INTERNAL ERROR 1 IN .in_quotes_replacement() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nARGUMENTS no_regex_pattern AND replacement MUST HAVE THE SAME NUMBER OF CHARACTERS\nno_regex_pattern (", base::nchar(no_regex_pattern), " characters):\n", base::paste(no_regex_pattern, collapse = "\n"), "\nreplacement (", base::nchar(replacement), " characters):\n", base::paste(replacement, collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)), )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    string_split <- base::strsplit(string, split = pattern, perl = perl)[[1]]
    string_out <- string_split[1]
    pos <- NULL
    if(base::length(string_split) > 1){
        count <- 1
        while(count < base::length(string_split)){
            count <- count + 1
            # if odds number of quotes, it means that # has broken the string in the middle of a quoted part
            double.quote.test <- saferDev:::.has_odd_number_of_quotes(input_string = string_out, pattern = '"') # here FALSE means even number of quotes, thus that ")" is not between quotes, thus has to be kept. TRUE means that ")" is between quotes, thus has to be removed
            simple.quote.test <- saferDev:::.has_odd_number_of_quotes(input_string = string_out, pattern = "'") # idem
            odds.quotes.log <- double.quote.test |  simple.quote.test # remove ")" ?
            if(odds.quotes.log == TRUE){
                pos <- base::c(pos, base::nchar(string_out) + 1)
                string_out <- base::paste0(string_out, replacement, string_split[count]) # to keep the same length of the tested function on a single string output_1_line
            }else{
                string_out <- base::paste0(string_out, no_regex_pattern, string_split[count])
                pos <- base::c(pos, NA)
            }
        }
    }
    if(base::nchar(string) == base::nchar(string_out) + 1){ # this is when the pattern is the last character of string. strsplit("a)", split = "\\)") gives "a". Should also deal when while loop has run, i.e., when several pattern in string including the last one: "a)vb)"
        double.quote.test <- saferDev:::.has_odd_number_of_quotes(input_string = string_out, pattern = '"') # here FALSE means even number of quotes, thus that ")" is not between quotes, thus has to be kept. TRUE means that ")" is between quotes, thus has to be removed
        simple.quote.test <- saferDev:::.has_odd_number_of_quotes(input_string = string_out, pattern = "'") # idem
        odds.quotes.log <- double.quote.test |  simple.quote.test # remove ")" ?
        if(odds.quotes.log == TRUE){
            pos <- base::c(pos, base::nchar(string_out) + 1)
            string_out <- base::paste0(string_out, replacement) # to keep the same length of the tested function on a single string output_1_line
        }else{
            string_out <- base::paste0(string_out, no_regex_pattern)
            pos <- base::c(pos, NA)
        }
    } # no need of else: string_out == string_split == string and pos is NULL
    if(base::all(base::is.na(pos), na.rm = TRUE)){
        pos <- NULL
    }else if(base::any(base::is.na(pos), na.rm = TRUE)){
        pos <- pos[ ! base::is.na(pos)]
    }
    if( ! base::is.null(pos)){
        tempo <- base::substring(string, pos, pos)
        if( ! base::all(base::unique(tempo) == no_regex_pattern, na.rm = TRUE)){
            tempo.cat <- base::paste0("INTERNAL ERROR 2 IN .in_quotes_replacement() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nARGUMENT no_regex_pattern NOT CORRECTLY DETECTED\nno_regex_pattern: \"", no_regex_pattern, "\"\nREPLACED CHARACTERS IN string ARGUMENT:\n", base::paste(tempo, collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    base::return(base::list(string = string_out, pos = pos))
}



#' @title .in_parenthesis_replacement
#' @description
#' Replace any pattern inside () by another replacement pattern
#' @param string Single string.
#' @param pattern Single string indicating the pattern to detect. Warning : must be very simple pattern, like ",".
#' @param no_regex_pattern Single string of the pattern to detect but without escape characters or list, etc.
#' @param replacement Single string for pattern replacement. Is not regex.
#' @param perl Single logical value. Use Perl regex in pattern ?
#' @param open_pos single integer indicating the position of the opening parenthesis.
#' @param close_pos single integer indicating the position of the closing parenthesis.
#' @param function_name Name of the function using this internal function.
#' @param package_name Name of the package of the function using this internal function.
#' @param internal_error_report_link String of the report link for internal error messages.
#' @returns A list containing:
#' $string: The input string with all pattern replaced by the replacement pattern.
#' $pos: the positions of the 1rst character of the replaced pattern. NULL if no replaced pattern. In that case, $string is identical to the input string
#' @details
#' Warning : must be very simple pattern, like "\\(".
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function
#' .in_parenthesis_replacement(string = "pattern = base::paste0(pattern, \"\\\\(#\"), text = text", pattern = ",", no_regex_pattern = ",", replacement = " ", perl = TRUE, open_pos = 23, close_pos = 39, function_name = "F1", package_name = "P1")
#' }
#' @keywords internal
#' @rdname internal_function
.in_parenthesis_replacement <- function(
    string, 
    pattern, 
    no_regex_pattern, 
    replacement, 
    perl,
    open_pos,
    close_pos,
    function_name,
    package_name,
    internal_error_report_link
){
    # DEBUGGING
    # string = "pattern = base::paste0(pattern, \"\\\\(#\"), text = text" ; pattern = "," ; no_regex_pattern = "," ; replacement = " " ; perl = TRUE ; open_pos = 23 ; close_pos = 39 ; function_name = "F1" ; package_name = "P1"
    if(base::substr(string, open_pos, open_pos) != "("){
        tempo.cat <- base::paste0("INTERNAL ERROR 1 IN .in_parenthesis_replacement() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nARGUMENT open_pos DOES NOT REFER TO A POSITION OF OPENING PARENTHESIS\nopen_pos:\n", base::paste(open_pos, collapse = "\n"), "\nstring:\n", base::paste(string, collapse = "\n"), "\nsubstr(string, open_pos, open_pos):\n", base::paste(base::substr(string, open_pos, open_pos), collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if(base::substr(string, close_pos, close_pos) != ")"){
        tempo.cat <- base::paste0("INTERNAL ERROR 2 IN .in_parenthesis_replacement() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nARGUMENT close_pos DOES NOT REFER TO A POSITION OF CLOSING PARENTHESIS\nclose_pos:\n", base::paste(close_pos, collapse = "\n"), "\nstring:\n", base::paste(string, collapse = "\n"), "\nsubstr(string, close_pos, close_pos):\n", base::paste(base::substr(string, close_pos, close_pos), collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    string_out <- string
    # Extract the substring between the given open and close parentheses
    substring_in_parentheses <- base::substr(string_out, open_pos, close_pos)
    # Find the position of comma within that substring
    comma_position_in_substring <- base::gregexpr(pattern, substring_in_parentheses)[[1]]
    # Initialize a vector to store global positions of the replaced commas
    pos <- NULL
    if (comma_position_in_substring[1] != -1) {
        for (relative_comma_position in comma_position_in_substring) {
            # Calculate the global position of the comma in the original string
            global_comma_position <- open_pos + relative_comma_position - 1
            # Replace that comma using substring or substr
            base::substring(string_out, global_comma_position, global_comma_position) <- replacement
            # Store the global position
            pos <- base::c(pos, global_comma_position)
        }
    }
    if( ! base::is.null(pos)){
        tempo <- base::substring(string, pos, pos)
        if( ! base::all(base::unique(tempo) == no_regex_pattern, na.rm = TRUE)){
            tempo.cat <- base::paste0("INTERNAL ERROR 3 IN .in_parenthesis_replacement() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nARGUMENT no_regex_pattern NOT CORRECTLY DETECTED\nno_regex_pattern: \"", no_regex_pattern, "\"\nREPLACED CHARACTERS IN string ARGUMENT:\n", base::paste(tempo, collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    # Return both the modified string and positions of replaced commas
    base::return(base::list(string = string_out, pos = pos))
}




#' @title .noclean_functions
#' @description
#' Indicate if function names are inside quotes or after $
#' @param col1 vector of strings.
#' @param col2 vector of strings of the function names.
#' @param col3 vector of strings of the code before the function name.
#' @param ini vector of string of the initial function code analyzed.
#' @returns A logical vector indicating if function names of col2 are inside quotes or after $ (TRUE) in ini or not (FALSE). Can be length 0
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function
#' source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R")
#' .noclean_functions(col1 =  c(15, 17), col2 = c("gregexpr", "regmatches"), col3 = c("matches <- ",  "matched_strings <- " ), ini = utils::capture.output(test))
#' }
#' @keywords internal
#' @rdname internal_function
.noclean_functions <- function(
    col1, 
    col2, 
    col3, 
    ini
){
    # AIM
    # Indicate if function names are inside quotes or after $
    # ARGUMENTS
    # col1: vector of integers of code line number
    # col2: vector of strings of the function names
    # ini: vector of string of the initial function code analyzed.
    # RETURN
    # A logical vector indicating if function names of col2 are inside quotes or after $ in ini.
    # DEBUGGING
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") ; col1 = c(15, 17) ; col2 = c("gregexpr", "regmatches") ; col3 = c("matches <- ",  "matched_strings <- " ) ; ini = utils::capture.output(test)
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") ; col1 = c(15, 22, 22) ; col2 = c("gregexpr", "col1", "roc1") ; col3 = c("matches <- ",  "matched_strings <- " ) ; ini = utils::capture.output(test)
    output <- base::vector(mode = "logical", length = 0) # here sum(output) = 0
    if(base::length(col1) > 0){
        # detection of a$fun() pattern
        tempo.log <- base::grepl(x = col3, pattern = "([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]* *\\$ *$")
        # ([a-zA-Z]|\\.[a-zA-Z._]) is for the begining of R function name: either any single alphabet character or a dot and any single alphabet character or dot (because .. is ok for function name) or underscore (because ._ is ok for function name). Starting "dot and num" or underscore is not authorized for function name
        # [a-zA-Z0-9._]* is The rest of the function name: any several of these characters or nothing
        #  *\\$ *$ means end of the string is any space or not, a $ symbol and any space or not
        if(base::any(tempo.log, na.rm = TRUE)){
            output <- tempo.log
        }else{
            output <- base::rep(FALSE, base::length(col1))
        }
        # end detection of a$fun() pattern
        # detection of functions between quotes
        tempo.ini.order <- 1:base::length(col1) # to recover the initial order at the end
        tempo.order <- base::order(col2) # order according to function name
        tempo.ini.order <- tempo.ini.order[tempo.order]
        tempo.col1 <- col1[tempo.order] # reorder to work only once with duplicated functions
        tempo.col2 <- col2[tempo.order] # reorder to work only once with duplicated functions
        tempo.ini <- ini
        pos.rm <- NULL # positions to remove (functions between quotes)
        for(i2 in 1:base::length(tempo.col1)){
            pattern1 <- base::paste0(tempo.col2[i2], " *\\(")
            lines.split <- base::strsplit(tempo.ini[tempo.col1[i2]], split = pattern1)[[1]][1]
            # if odds number of quotes, it means that # has broken the string in the middle of a quoted part
            double.quote.test <- saferDev:::.has_odd_number_of_quotes(input_string = lines.split, pattern = '"') # here FALSE means even number of quotes, thus that the function is not between quotes, thus has to be kept. TRUE means that the function is between quotes, thus has to be removed
            simple.quote.test <- saferDev:::.has_odd_number_of_quotes(input_string = lines.split, pattern = "'") # idem
            odds.quotes.log <- double.quote.test |  simple.quote.test
            if(odds.quotes.log == FALSE){
                pos.rm <- base::c(pos.rm, i2)
            }else{
                pos.rm <- base::c(pos.rm, NA) # NA means betwwen quotes. pos.rm will becomes double if integer added, otherwise remains logical. Thus, do not use any()
            }
            tempo.ini[tempo.col1[i2]] <- base::sub(pattern = pattern1, replacement = "", x = tempo.ini[tempo.col1[i2]], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove the first fonction in the line, in case of identical function names in a code line. Like, that, the next round for the next same function can be easily tested for "between quotes" 
        }
        # initial order
        pos.rm.fin <- pos.rm[base::order(tempo.ini.order)]
        # end initial order
        if(base::any(base::is.na(pos.rm.fin), na.rm = TRUE)){
            output <- output | base::is.na(pos.rm.fin)
        }
        # end detection of functions between quotes
    }
    base::return(output)
}








#' @title .fun_args_pos
#' @description
#' Return the positions of 1st letter of the function name and opening and closing parenthesis, as well as positions of the internal parenthesis.
#' @param text A string.
#' @param pattern: A perl regex to extract function name and (), using generally paste0(<FUNCTION_NAME>, "[\\s\\r\\n]*\\(").
#' @param function_name Name of the function using this internal function.
#' @param package_name Name of the package of the function using this internal function.
#' @param internal_error_report_link String of the report link for internal error messages.
#' @returns A list containing two positions:
#' $begin_fun: position of 1st letter of the function name.
#' $begin: position of the "(" of the function.
#' $end: position of the closing ")" of the function.
#' $middle_bracket_pos: list of positions of the couple of brackets in the middle of the begin and end positions. In each compartment, the first number is the position of ( and the second the position of ). NULL if no inside brackets.
#' @details
#' Warning: the string must be cleaned form brackets between quotes. Use .in_quotes_replacement() for that.
#' Warning: quotes in strings are escaped, so that position of ( in \"a( is 3, not 4.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function
#' # Warning : examples only with strings that must be cleaned form brackets between quotes
#' .fun_args_pos(text = "a$regmatches(x = text, m = matches)[[1]]", pattern = paste0("regmatches", "[\\s\\r\\n]*\\("), function_name = "F1", package_name = "P1")
#' .fun_args_pos(text = ' "a" ; paste0("I", paste0(sum(1:3), collapse = " "), min(1) ) ; range(2)', pattern = paste0("paste0", "[\\s\\r\\n]*\\("), function_name = "F1", package_name = "P1")
#' }
#' @keywords internal
#' @rdname internal_function
.fun_args_pos <- function(
    text, 
    pattern,
    function_name, 
    package_name,
    internal_error_report_link
){
    # DEBUGGING
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R")
    # text = ' "a" ; paste0("I", paste0(sum(1:3), collapse = " "), min(1) ) ; range(2)' ; pattern = paste0("paste0", "[\\s\\r\\n]*\\(") ; function_name = "F1" ; package_name = "P1"
    # text = 'base::gregexpr(pattern = base::paste0(pattern, "\\(#"), text = text)' ; pattern = 'gregexpr[\\s\\r\\n]*\\(' ; function_name = "F1" ; package_name = "P1"
    check_pos <- function(x){
        if(base::length(x) != 1 | base::any(base::is.na(x), na.rm = TRUE) | base::is.null(x) | base::any(x < 0 , na.rm = TRUE)){
            tempo.cat <- base::paste0("INTERNAL ERROR 1 IN .fun_args_pos() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nINTERNAL FUNCTION DID NOT PROPERLY DETECT THE POSITION OF ", base::match.call(expand.dots = FALSE)$x, "\ntext: ", base::paste(text, collapse = "\n"), "\npattern: ", base::paste(pattern, collapse = "\n"), "\nfun_pos: ", base::paste(fun_pos, collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }

    while_loop <- function(
        start,
        all_pos, 
        open_pos,
        close_pos,
        function_name,
        package_name,
        internal_error_report_link, 
        text,
        pattern
    ){
        count <- 1 # 1 because ( of the function is already opened. When count == 0, we will have the closing )
        final_pos <- base::which(all_pos == start) #start by the first ( but will be incremented
        loop.nb <- 1 
        while(count != 0 & loop.nb < base::length(all_pos)){
            final_pos <- final_pos + 1
            if(all_pos[final_pos] %in% open_pos){
                count <- count + 1
            }
            if(all_pos[final_pos] %in% close_pos){
                count <- count - 1
            }
            loop.nb <- loop.nb + 1
        }
        if(count != 0){
            tempo.cat <- base::paste0("INTERNAL ERROR 1 IN while_loop() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nINTERNAL FUNCTION DID NOT PROPERLY DETECT THE POSITION OF THE CLOSING BRACKET IN ", base::match.call(expand.dots = FALSE)$x, "\ntext: ", base::paste(text, collapse = "\n"), "\npattern: ", base::paste(pattern, collapse = "\n"), "\ncount: ", base::paste(count, collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }else{
            base::return(final_pos)
        }
    }

    open_paren_pos <- base::as.vector(base::gregexpr(pattern = "\\(", text = text)[[1]])
    close_paren_pos <- base::as.vector(base::gregexpr(pattern = "\\)",  text = text)[[1]])
    # left position
    fun_pos <- base::as.vector(base::gregexpr(pattern = pattern,  text = text, perl = TRUE)[[1]][1]) # position of the 1st character of fun in text
    check_pos(x = fun_pos)
    # end left position
    fun_open_paren_pos <- open_paren_pos[open_paren_pos > fun_pos][1] # position of ( of the fonction
    check_pos(x = fun_open_paren_pos)
    # detection of the closing ) of the function
    all_pos <- base::sort(base::c(open_paren_pos, close_paren_pos))
    
    final_pos <- while_loop(
        start = fun_open_paren_pos,
        all_pos = all_pos, 
        open_pos = open_paren_pos, 
        close_pos = close_paren_pos, 
        function_name = function_name,
        package_name = package_name,
        internal_error_report_link = internal_error_report_link, 
        text = text,
        pattern = pattern
    )
    fun_close_paren_pos <- all_pos[final_pos]
    # end detection of the closing ) of the function
    # middle brackets
    tempo_log <- all_pos > fun_open_paren_pos & all_pos < fun_close_paren_pos
    if(base::any(tempo_log, na.rm = TRUE)){
        all_pos_inside <- all_pos[tempo_log] # all positions of the brackets inside fun(    )
        open_paren_pos_inside <- all_pos_inside[all_pos_inside %in% open_paren_pos]
        count_open_paren_pos_inside <- base::length(open_paren_pos_inside)
        close_paren_pos_inside <- all_pos_inside[all_pos_inside %in% close_paren_pos]
        count_close_paren_pos_inside <- base::length(open_paren_pos_inside)
        if(count_open_paren_pos_inside != count_close_paren_pos_inside | count_open_paren_pos_inside == 0){ #count_open_paren_pos_inside == 0 because tempo_log above has some TRUE
            tempo.cat <- base::paste0("INTERNAL ERROR 2 IN while_loop() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nTHE .fun_args_pos() INTERNAL FUNCTION DID NOT PROPERLY DETECT THE POSITION ALL THE BRACKETS INSIDE THE FUN(    ) BRACKETS IN ", base::match.call(expand.dots = FALSE)$x, "\ntext: ", base::paste(text, collapse = "\n"), "\npattern: ", base::paste(pattern, collapse = "\n"), "\nCOUNT OF OPENED BRACKETS: ", count_open_paren_pos_inside, "\nCOUNT OF CLOSING BRACKETS: ", count_close_paren_pos_inside, "\nCHECK THAT THE STRING HAS ALL THE BRACKETS BETWEEN QUOTES REMOVED", base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        middle_bracket_pos <- base::vector(mode = "list", length = count_open_paren_pos_inside)
        for(i2 in 1:count_open_paren_pos_inside){
            final_pos2 <- while_loop(
                start = open_paren_pos_inside[i2],
                all_pos = all_pos_inside, 
                open_pos = open_paren_pos_inside, 
                close_pos = close_paren_pos_inside, 
                function_name = function_name,
                package_name = package_name, 
                internal_error_report_link = internal_error_report_link, 
                text = text,
                pattern = pattern
            )
            middle_bracket_pos[[i2]] <- base::c(open_paren_pos_inside[i2], all_pos_inside[final_pos2])
        }
    }else{
        middle_bracket_pos <- NULL
    }
    # end middle brackets
    output <- base::list(
        begin_fun = fun_pos, 
        begin = fun_open_paren_pos,
        end = fun_close_paren_pos,
        middle_bracket_pos = middle_bracket_pos
    )
    base::return(output)
}






#' @title .functions_detect
#' @description
#' Detect all the functions names used inside a function.
#' @param x a function name, written without quotes and brackets.
#' @param arg_user_setting Argument user settings list.
#' @param function_name Name of the function using this internal function.
#' @param package_name Name of the package of the function using this internal function.
#' @param internal_error_report_link String of the report link for internal error messages.
#' @returns 
#'  A list:
#' $code: vector of strings of the code of the tested function.
#' $all_basic_funs: vector or strings of names of all the basic R functions.
#' $fun_names: list of names of all the functions, not considering base::c("function", "if", "for", "while", "repeat"). Compartment names indicate the code line number of the functions in $code.
#' $fun_names_pos: list of position of the first character of each $fun_names. Compartment names indicate the code line number of the functions in $code.
#' $code_line_nb: vector of integers of the code line numbers of code for each non empty compartment of $fun_names and $fun_names_pos.
#' $internal_fun_names: vector of string of names of internal functions in the code of the tested function.
#' $arg_user_setting: list of arg user settings of the tested function.
#' @details
#' - Does not check if the functions inside the code exist.
#' - Use the regex pattern "([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*\\(" to detect a function in the code.
#' - $all_basic_funs are all the functions in base::c("package:stats", "package:graphics",  "package:grDevices", "package:utils", "package:datasets", "package:methods", "Autoloads", "package:base")
#' 
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function
#' source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\dev\\other\\test.R") ; .functions_detect(x = test, arg_user_setting = base::list(x =  as.name(x = "test")), function_name = "F1", package_name = "P1")
#' }
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @keywords internal
#' @rdname internal_function
.functions_detect <- function(
    x, 
    arg_user_setting, 
    function_name, 
    package_name,
    internal_error_report_link
){
    # DEBUGGING
    # x = x ; arg_user_setting = arg_user_setting ; function_name = function_name ; package_name = package_name
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\dev\\other\\test2.R") ; x = test2 ; arg_user_setting = base::list(x = as.name(x = "test2"), export = TRUE) ; function_name = "F1" ; package_name = "P1"
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\.github\\profile\\backbone.R") ; x = BACKBONE ; arg_user_setting = base::list(x = as.name(x = "BACKBONE"), export = FALSE,  path_out = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE) ; function_name = "F1" ; package_name = "P1"
    # FUN1 <- function(x, y){middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, quote = FALSE, envir = base::parent.frame())} ; x = FUN1 ; arg_user_setting = base::list(x = as.name(x = "FUN1"), export = FALSE,  path_out = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE) ; function_name = "F1" ; package_name = "P1"
    # FUN1 <- function(x, y){FUN2 <- function(x){x = 1}} ; x = FUN1 ; arg_user_setting = base::list(x = as.name(x = "FUN1"), export = FALSE,  path_out = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE) ; function_name = "F1" ; package_name = "P1"
    # main code
    # modification of arg_user_setting$x for clean messages
    if(base::as.character(x = arg_user_setting$x)[1] == "::" | base::as.character(x = arg_user_setting$x)[1] == ":::"){
        arg_user_setting$x <- base::paste0(base::as.character(x = arg_user_setting$x)[3], "()")
    }
    # end modification of arg_user_setting$x for clean messages
    # recovering the basic functions of R
    s <- base::c("package:stats", "package:graphics",  "package:grDevices", "package:utils", "package:datasets", "package:methods", "Autoloads", "package:base") # basic base::search() scope
    if(base::any( ! s %in% base::search())){
        tempo.cat <- base::paste0("INTERNAL ERROR 1 IN .functions_detect() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nTHE base::search() SCOPE OF R HAS CHANGED.\nTHE PROBLEM IS:\n",
            base::paste(s[ ! s %in% base::search()], collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    fun <- base::unlist(base::sapply(X = s, FUN = function(x){base::ls(x, all.names = TRUE)})) # all the basic functions of R in all the scope
    # end recovering the basic functions of R
    # recovering the input function string
    code <- utils::capture.output(x) # no lines must be removed because it is to catch the lines of the full code
    code_line_nb <- 1:base::length(code)
    # code <- base::paste0(code, collapse = " \\n ") # recovering as single string separated by \\n (and not \n to avoid the eval(\n) when printing the error message)
    code <- base::gsub(x = code, pattern = " +", replacement = " ") # removal of multiple spaces
    code <- base::sub(x = code, pattern = "^ +", replacement = "") # removal of multiple spaces in the beginning od strings
    # end recovering the input function string

    # removal of empty lines
    empty_line.log <- base::grepl(code, pattern = "^\\s*$")
    # end removal of empty lines

    # removal of comments
    comment_line.log <- base::grepl(code, pattern = "^\\s*#") # removal of the lines starting by #
    if(base::length(code) == 0){
        tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nTHE TESTED FUNCTION ", arg_user_setting$x, " IS EMPTY OR ONLY MADE OF COMMENTS")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    comment.log <- base::grepl(x = code, pattern = "#")
    if(base::any(comment.log, na.rm = TRUE)){
        comment.line.to.rm <- base::which(comment.log) # elements among code that have #
        lines <- code[comment.log]
        for(i2 in 1:base::length(lines)){
            lines.split <- base::strsplit(lines[i2], split = "#")[[1]]
            # detection of the first left # that is not between quotes
            count <- 1
            tempo.line <- lines.split[1]
            while.loop <- TRUE
            while(while.loop == TRUE & count < base::length(lines.split)){
                # if odds number of quotes, it means that # has broken the string in the middle of a quoted part
                double.quote.test <- saferDev:::.has_odd_number_of_quotes(input_string = tempo.line, pattern = '"') # here FALSE means even number of quotes, thus that # is not between quotes, thus has to be removed. TRUE means that # is between quotes, thus has to be kept
                simple.quote.test <- saferDev:::.has_odd_number_of_quotes(input_string = tempo.line, pattern = "'") # idem
                odds.quotes.log <- double.quote.test |  simple.quote.test # lines to keep among commented lines
                if(odds.quotes.log == TRUE){
                    count <- count + 1
                    tempo.line <- base::paste0(tempo.line, "#", lines.split[count])
                }else{
                     while.loop <- FALSE
                }
            }
            # end detection of the first left # that is not between quotes
            lines[i2] <- tempo.line
        }
        code[comment.line.to.rm] <- lines
    }
    # end removal of comments
    # catch the internal function name created inside the tested function
    internal_fun_names <- base::unlist(base::lapply(X = code, FUN = function(x){
        output <- base::sub(pattern = "^\\s*([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*<-[\\s\\r\\n]*function[\\s\\r\\n]*\\(.*", replacement = "\\1", x = x, perl = TRUE)
        # ^\\s* means in perl: 0 or any spaces at the begining of the string
        # ([a-zA-Z]|\\.[a-zA-Z._]) is for the begining of R function name: either any single alphabet character or a dot and any single alphabet character or dot (because .. is ok for function name) or underscore (because ._ is ok for function name). Starting "dot and num" or underscore is not authorized for function name
        # [a-zA-Z0-9._]* is The rest of the function name: any several of these characters or nothing
        #  \\s*<-[\\s\\r\\n]*function[\\s\\r\\n]*\\(.* 0 or any space, assignation symbol, 0 or any (space, carriage return, end of line), an opening parenthesis, and any character
        if( ! output == x){
            base::return(output)
        }
    })) # To achieve the extraction of the function names, you need to wrap the part of the pattern that matches the function name in parentheses () to create a capturing group
    # end catch the internal function name created inside the tested function
    # trick to deal with end of lines between the name of the function and "("
    if(base::length(code) > 1){
        for (i2 in 2:base::length(code)) {
            # Check if the current string starts with spaces followed by a '('
            if (base::grepl("^\\s*\\(", code[i2])) {
                # Check if the previous string ends with the specified pattern
                if (base::grepl("([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*$", code[i2 - 1])) {
                # Append a '(' to the previous string
                code[i2 - 1] <- base::paste0(code[i2 - 1], "(")
                }
            }
        }
    }
    # end trick to deal with end of lines between the name of the function and "("
    # all function names in x
    pattern1 <- "([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*\\(" # pattern to detect a function name, a$fun( is removed in .noclean_functions()
    # ([a-zA-Z]|\\.[a-zA-Z._]) is for the begining of R function name: either any single alphabet character or a dot and any single alphabet character or dot (because .. is ok for function name) or underscore (because ._ is ok for function name). Starting "dot and num" or underscore is not authorized for function name
    # [a-zA-Z0-9._]* is The rest of the function name: any several of these characters or nothing
    # \\s*\\( means 0 or any space in perl, and an opening parenthesis
    # I could have used [\\s\\r\\n]* meaning any space or end of line or carriage return between the name and "(" but finally, another strategy used
        # - `this does not work well, as it does not take dots: "\\b[a-zA-Z\\.\\_]{1}[a-zA-Z0-9\\.\\_]+\\b", because of `\\b`: These are word boundaries. It ensures that the pattern matches only a complete word and not a part of a word.
        # - `[a-zA-Z.]{1}`: This portion of the pattern matches any uppercase letter (`A-Z`), lowercase letter (`a-z`), or a period (`.`) a single time ({1}).
        # - `[a-zA-Z0-9._]*`: This part of the pattern matches any uppercase letter (`A-Z`), lowercase letter (`a-z`), number (`0-9`), period (`.`), or underscore (`_`), repeated zero or more times (`*`). This represents the possible characters inside an R function name.
        # - `\\b`: Again, these are word boundaries, making sure the pattern captures the entire word and not just part of it.
        # -  not used: `(?= *\\()`: This is a lookahead assertion. It checks that the preceding pattern is followed by any spaces and a parenthesis (`\\(`), but doesn't include the spaces and parenthesis in the match. This is because, in R code, a function call is usually followed by a parenthesis, but the parenthesis is not part of the function name.

    fun_name <- base::list()
    fun_name_pos <- base::list()
    for(i1 in 1:base::length(code)){
        tempo <- saferDev:::.extract_all_fun_names(text = code[i1], pattern = pattern1) # recover all the function names, followed by "(", present in code, using a perl pattern
        fun_name <- base::c(fun_name, base::list(tempo$string))
        fun_name_pos <- base::c(fun_name_pos, base::list(tempo$pos))
    }
    # tempo <- base::lapply(code, FUN = function(x){saferDev:::.extract_all_fun_names(text = x, pattern = pattern1)})
    # removal of special functions
    tempo_log <- base::lapply(fun_name, FUN = function(x){ ! x %in% base::c("function", "if", "for", "while", "repeat")})
    fun_name_wo_op <- base::mapply(FUN = function(x, y){x[y]}, x = fun_name, y = tempo_log, SIMPLIFY = FALSE)
    fun_name_pos_wo_op <- base::mapply(FUN = function(x, y){x[y]}, x = fun_name_pos, y = tempo_log, SIMPLIFY = FALSE)
    # end removal of special functions
    # removal of empty string
    tempo.log <- base::sapply(fun_name_wo_op, FUN = function(x){base::length(x) == 0}) # detection of string with empty function names
    fun_name_wo_op <- fun_name_wo_op[ ! tempo.log]
    fun_name_pos_wo_op <- fun_name_pos_wo_op[ ! tempo.log]
    code_line_nb <- code_line_nb[( ! tempo.log) & ( ! comment_line.log) & ( ! empty_line.log)]
    if( ! (base::length(fun_name_wo_op) == base::length(fun_name_pos_wo_op) & base::length(fun_name_wo_op) == base::length(code_line_nb))){
        tempo.cat <- base::paste0("INTERNAL ERROR 2 IN .functions_detect() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL\nfun_name_wo_op: ", base::length(fun_name_wo_op), "\nfun_name_pos_wo_op: ", base::length(fun_name_pos_wo_op), "\ncode_line_nb: ", base::length(code_line_nb), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }else if(base::any(base::is.na(code_line_nb))){
        tempo.cat <- base::paste0("INTERNAL ERROR 3 IN .functions_detect() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\ncode_line_nb SHOULD NOT CONTAIN NA.\ncode_line_nb:\n", base::paste(code_line_nb, collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }else{
        # with that, now the code line of code is indicated in as compartment names
        base::names(fun_name_wo_op) <- base::paste0("c", code_line_nb)
        base::names(fun_name_pos_wo_op) <- base::paste0("c", code_line_nb)
    }
    # end removal of empty string
    test.log <- base::mapply(FUN = function(x, y){base::length(x) != base::length(y)}, x = fun_name_wo_op, y = fun_name_pos_wo_op, SIMPLIFY = TRUE)
    if(base::any(test.log, na.rm = TRUE)){
        tempo.cat <- base::paste0("INTERNAL ERROR 4 IN .functions_detect() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL IN COMPARTMENTS ", base::paste(base::which(test.log), collapse = ", "), " OF fun_name_wo_op AND fun_name_pos_wo_op", base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # fun_name_wo_op_uni <- base::unlist(base::unique(fun_name_wo_op)) # in case
    # end all function names in x
    #### output
    output <- base::list(
        code = code, 
        all_basic_funs = fun, 
        fun_names = fun_name_wo_op, 
        fun_names_pos = fun_name_pos_wo_op, 
        code_line_nb = code_line_nb, 
        internal_fun_names = internal_fun_names,
        arg_user_setting = arg_user_setting
    )
    base::return(output)
    #### end output
}



#' @title .colons_check_message
#' @description
#' Create the message for the colons_check() function.
#' @param list.fun list of names of all the functions.
#' @param fun.uni vector of all the unique function names.
#' @param list.fun.pos list of position of first character of names of all the functions in ini.
#' @param line.nb vector of corresponding line number.
#' @param ini vector of string of the initial function code analyzed.
#' @param arg_user_setting list of arg user settings.
#' @param function_name Name of the function using this internal function.
#' @param package_name Name of the package of the function using this internal function.
#' @param internal_error_report_link String of the report link for internal error messages.
#' @param text either "BASIC" or "OTHER".
#' @param internal_fun_names vector of string of names of internal functions in the function code analyzed.
#' @returns
#'  A list:
#'  $output.cat: the message (string).
#'  $colon_not_here: logical vector. Does list.fun contain function names without :: or ::: ?
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @keywords internal
#' @rdname internal_function
.colons_check_message <- function(
    list.fun, 
    fun.uni, 
    list.fun.pos, 
    line.nb, 
    ini, 
    arg_user_setting, 
    function_name, 
    package_name, 
    internal_error_report_link, 
    text,
    internal_fun_names
){
    # DEBUGGING
    # list.fun = in_basic_fun ; fun.uni = in_basic_fun_uni ; list.fun.pos = in_basic_fun_names_pos ; line.nb = in_basic_code_line_nb ; ini = out$code ; arg_user_setting = out$arg_user_setting ; function_name = function_name ; package_name = package_name ; text = "BASIC" ; internal_fun_names = out$internal_fun_names
    # list.fun = in_other_fun ; fun.uni = in_other_fun_uni ; list.fun.pos = in_other_fun_names_pos ; line.nb = in_other_code_line_nb ; ini = out$code ; arg_user_setting = out$arg_user_setting ; function_name = function_name ; package_name = package_name ; text = "OTHER" ; internal_fun_names = out$internal_fun_names
    if(base::length(text) != 1 & base::any( ! text %in% base::c("BASIC", "OTHER"))){
        tempo.cat <- base::paste0("INTERNAL ERROR 1 IN .colons_check_message() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nTHE text ARGUMENT OF .colons_check_message() MUST BE \"BASIC\" OR \"OTHER\".\nTHE PROBLEM IS:\n",
            base::paste(text, collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # pattern2 <- base::paste(base::paste0("(?<![A-Za-z0-9._])", fun.uni, "\\s*\\("), collapse = "|") # to split string according to function name as splitter. Pattern (?<![A-Za-z0-9._]) means "must not be preceeded by any alphanum or .or _
    # pattern3 <- base::paste(base::paste0("(?<![A-Za-z0-9._])", fun.uni, "\\s*\\($"), collapse = "|") # same as pattern2 but used to know if the seeked function is at the end of the string
    basic_ini <- ini[line.nb]
    if( ! (base::length(list.fun) == base::length(list.fun.pos) & base::length(list.fun) == base::length(line.nb) & base::length(list.fun) == base::length(basic_ini))){
        tempo.cat <- base::paste0("INTERNAL ERROR 2 IN .colons_check_message() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nIN .colons_check_message(), LENGTHS SHOULD BE IDENTICAL\nlist.fun: ", base::length(list.fun), "\nlist.fun.pos: ", base::length(list.fun.pos), "\nline.nb: ", base::length(line.nb), "\nbasic_ini: ", base::length(basic_ini), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    res <- list.fun.pos
    for(i1 in 1:base::length(basic_ini)){
        res[[i1]] <- base::mapply(FUN = function(x , y){z <- base::substr(x = x, start = 1, stop = y - 1)}, x = basic_ini[i1], y = list.fun.pos[[i1]], SIMPLIFY = TRUE, USE.NAMES = FALSE)
    }
    # res <- base::strsplit(x = basic_ini, split = pattern2, perl = TRUE) # in res, all the strings should finish by ::
    # tempo.log <- ! base::grepl(x = basic_ini, pattern = pattern3, perl = TRUE) # strings of basic_ini that does not finish by the function name
    # in each compartment of res, the last split section is removed because nothing to test at the end (end of code)
    # if(base::sum(tempo.log, na.rm = TRUE) > 0){
    #     res[tempo.log] <- base::lapply(X = res[tempo.log], FUN = function(x){x[-base::length(x)]})
    # }
    # end in each compartment of res, the last split section is removed because nothing to test at the end (end of code)
    res2 <- base::lapply(X = res, FUN = function(x){base::substr(x, base::nchar(x)-1, base::nchar(x))}) # base::nchar(x)-1 takes only :: if the strings ends by :::
    base::names(res2) <- NULL
    if( ! base::all(base::sapply(X = res2, FUN = function(x){base::length(x)}) == base::sapply(X = res, FUN = function(x){base::length(x)}))){
        tempo.cat <- base::paste0("INTERNAL ERROR 3 IN .colons_check_message() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL\nres2: ", base::paste(base::sapply(X = res2, FUN = function(x){base::length(x)}), collapse = " "), "\nres: ", base::paste(base::sapply(X = res, FUN = function(x){base::length(x)}), collapse = " "), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    colon_not_here <- base::lapply(X = res2, FUN = function(x){ ! x %in% "::"}) # no need to check for ":::" because base::nchar(x)-1 takes only :: if the strings ends by :::
    if( ! base::all(base::sapply(X = res2, FUN = function(x){base::length(x)}) == base::sapply(X = colon_not_here, FUN = function(x){base::length(x)}))){
        tempo.cat <- base::paste0("INTERNAL ERROR 4 IN .colons_check_message() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL\nres2: ", base::paste(base::sapply(X = res2, FUN = function(x){base::length(x)}), collapse = " "), "\ncolon_not_here: ", base::paste(base::sapply(X = colon_not_here, FUN = function(x){base::length(x)}), collapse = " "), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if(base::any(base::unlist(colon_not_here))){
        col1 <- base::as.vector(base::unlist(base::mapply(FUN = function(x, y){base::rep(y, base::sum(x))}, x = colon_not_here, y = line.nb, SIMPLIFY = TRUE)))
        col2 <- base::as.vector(base::unlist(base::mapply(FUN = function(x, y){y[x]}, x = colon_not_here, y = list.fun, SIMPLIFY = TRUE)))
        col3 <- base::as.vector(base::unlist(base::mapply(FUN = function(x, y){y[x]}, x = colon_not_here, y = res, SIMPLIFY = TRUE)))
        if( ! (base::length(col1) == base::length(col2) & base::length(col1) == base::length(col3) & base::length(col2) == base::length(col3))){
            tempo.cat <- base::paste0("INTERNAL ERROR 5 IN .colons_check_message() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTHS OF col1 (", base::length(col1), "), col2 (", base::length(col2), "), AND col3 (", base::length(col3), "), SHOULD BE EQUAL\n", base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        # removal of functions between quotes and after $
        tempo.log <- saferDev:::.noclean_functions(col1 = col1, col2 = col2, col3 = col3, ini = ini) # function names are inside quotes or after $ ?
        if(base::sum(tempo.log, na.rm = TRUE) > 0){
            col1 <- col1[ ! tempo.log] # keep clean functions
            col2 <- col2[ ! tempo.log] # keep clean functions
            col3 <- col3[ ! tempo.log] # keep clean functions 
        }
        # end removal of functions between quotes and after $
        if(base::length(col1) > 0){
            tempo.pos <- base::paste0(col1, "\t", col2, "\t\t", col3)
            output.cat <- base::paste0(
                "INSIDE ", arg_user_setting$x, "(), SOME :: OR ::: ARE MISSING AT ", text, " FUNCTION POSITIONS:\n\n", 
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
            "INSIDE ", arg_user_setting$x, "(), INTERNAL FUNCTION", base::ifelse(base::length(internal_fun_names) == 1, "", "S"), " DETECTED:\n", 
            base::paste(internal_fun_names, collapse = "\n"), 
            "\n\n", 
            output.cat
        )
    }
    base::return(base::list(output.cat = output.cat, colon_not_here = base::unlist(colon_not_here)))
}


#' @title .all_args_here_fill
#' @description
#' Get the $MISSING_ARG_NAMES, $MISSING_ARGS and $NEW of all_args_here()
#' @param arg_full list of all arguments of the function with default value
#' @param arg_full_names vector of strings of the names of the arguments of the function
#' @param tempo_split vector of strings of the observed argument writting of the function.
#' @param three_dots_log vector of logical. Is ... present among arg_full_names 
#' @param i2 loop number
#' @param col1_i2 code line number of the checked function
#' @param col2_i2 name of the checked sub function
#' @param arg_user_setting_x name of the checked function
#' @param function_name Name of the function using this internal function.
#' @param package_name Name of the package of the function using this internal function.
#' @param internal_error_report_link String of the report link for internal error messages.
#' @param warn warning string.
#' @param warn_count warning count.
#' @returns
#'  A list:
#'    $col6: the $MISSING_ARG_NAMES.
#'    $col7: the $MISSING_ARGS.
#'    $col8: the $STATUS.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @keywords internal
#' @rdname internal_function
.all_args_here_fill <- function(
    arg_full,
    arg_full_names, 
    tempo_split, 
    three_dots_log, 
    i2, 
    col1_i2, 
    col2_i2,
    arg_user_setting_x, 
    function_name, 
    package_name,
    internal_error_report_link, 
    warn,
    warn_count
){
    # DEBUGGING
    # arg_full = arg_full ; arg_full_names = arg_full_names ; tempo_split = tempo_split ; three_dots_log = three_dots_log ; i2 = i2 ; col1_i2 = col1[i2] ; col2_i2 = col2[i2] ; function_name = function_name ; package_name = package_name 
    #  arg_full = list(definition = "sys.function(sys.parent())", call = "sys.call(sys.parent())", expand.dots = TRUE, envir = "parent.frame(2L)") ; arg_full_names = c("definition", "call", "expand.dots", "envir") ; tempo_split = "expand.dots = FALSE" ;  three_dots_log = c(FALSE, FALSE, FALSE, FALSE) ; col2_i2 = "match.call" ; col3_i2 = "match.call(expand.dots = FALSE)" ; function_name = "F1" ; package_name = "P1"
    #  arg_full = list(definition = sys.function(sys.parent()), call = sys.call(sys.parent()), expand.dots = TRUE, envir = parent.frame(2L)) ; arg_full_names = c("definition", "call", "expand.dots", "envir") ; tempo_split = c("sys.function(sys.parent())", "expand.dots = FALSE", "sys.call(sys.parent())") ;  three_dots_log = c(FALSE, FALSE, FALSE, FALSE) ; col2_i2 = "match.call" ; col3_i2 = "match.call(sys.function(sys.parent()), expand.dots = FALSE, sys.call(sys.parent()))" ; function_name = "F1" ; package_name = "P1"
    #  arg_full = list(... = "", collapse = " ", recycle0 = FALSE) ; arg_full_names = c("...", "collapse", "recycle0") ; tempo_split = c("AA", "collapse = \" \"", "BB", "recycle0 = FALSE") ; three_dots_log = c(TRUE, FALSE, FALSE) ; col2_i2 = "paste0" ; col3_i2 = 'paste0("AA", collapse = " ", "BB", recycle0 = FALSE)' ; function_name = "F1" ; package_name = "P1"
    #  arg_full = list(... = "", collapse = " ", recycle0 = FALSE) ; arg_full_names = c("...", "collapse", "recycle0") ; tempo_split = c("AA", "collapse = \" \"", "BB") ; three_dots_log = c(TRUE, FALSE, FALSE) ; col2_i2 = "paste0" ; col3_i2 = 'paste0("AA", collapse = " ", "BB")' ; function_name = "F1" ; package_name = "P1"
    pattern1 <- "^\\s*([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*[\\s\\r\\n]*=" # looking for the arg name
    good_args <- NULL
    missing_args <- NULL
    missing_args_names <- NULL
    obs_arg_log <- base::logical()
    if(base::any(three_dots_log, na.rm = TRUE)){
        arg_full_names <- arg_full_names[ ! three_dots_log]
        arg_full <- arg_full[ ! three_dots_log]
    }
    if(base::length(arg_full) == 0){
        # col5 <- base::c(col5, "...") #inactivated because already filled above
        col6 <- ""
        col7 <- ""
        col8 <- ""
    }else{
        # scan for args names present in tempo_split
        good_count <- 0 # to define if all the args are written (not considering ...)
        if(base::length(tempo_split) == 0 & base::length(arg_full_names) > 0){
            missing_args_names <- arg_full_names
        }else{
            obs_arg_log <- base::rep(TRUE, base::length(tempo_split)) # will help for counting the tempo_split args without arg name before. All the remaining TRUE will be values that need an arg name
            for(i3 in 1:base::length(arg_full_names)){
                pattern3 <- base::paste0("^[\\s\\r\\n]*", arg_full_names[i3], "[\\s]*=") # looking for the arg name
                tempo.log <- base::grepl(x = tempo_split, pattern = pattern3, perl = TRUE)
                if(base::sum(tempo.log, na.rm = TRUE) == 1){ # arg i3 has its names written in the args between ()
                    good_args <- base::c(good_args, tempo_split[tempo.log])
                    obs_arg_log <- obs_arg_log & ! tempo.log # remove the position of the taken arg in tempo_split
                    good_count <- good_count + 1
                }else if(base::sum(tempo.log, na.rm = TRUE) == 0){ # arg i3 has not its names written in the args between ()
                    missing_args_names <- base::c(missing_args_names, arg_full_names[i3])
                }else{
                    tempo.cat <- base::paste0("INTERNAL ERROR 1 IN .all_args_here_fill() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\npattern3 DETECTED SEVERAL TIMES IN ARGUMENTS:\n\npattern3:\n", base::paste(pattern3, collapse = "\n"), "\n\ntempo_split:\n", base::paste(tempo_split[tempo.log], collapse = "\n"), "\n\nCHECK IF THE ARGUMENT IS PRESENT SEVERAL TIMES IN LINE ", col1_i2, ", INSIDE ", col2_i2, base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                }
            }
        }
        # end scan for args names present in tempo_split
        # removal of arguments without arg name before in obs_arg_log
        supp_args_in_three_dots <- NULL
        if(base::any(three_dots_log, na.rm = TRUE)){
            if(base::length(x = obs_arg_log) != 0){ # no need to add & base::length(tempo_split) != 0 because both have the same length
                for(i3 in 1:base::length(tempo_split)){
                    if(base::grepl(x = tempo_split[i3], pattern = pattern1, perl = TRUE) & obs_arg_log[i3] == TRUE){ # obs_arg_log[i3] == TRUE means values that need an arg name but detection of a = with only arg name rule before
                        obs_arg_log[i3] <- FALSE # remove this arg from the args that need an arg name
                        supp_args_in_three_dots <- base::c(supp_args_in_three_dots, tempo_split[i3])
                    }
                }
            }
        }
        # end removal of arguments without arg name before in obs_arg_log
        # detection of arguments that starts by the same string in the sub function
        tempo_col8_end <- NULL
        same_begin <- base::unlist(base::lapply(
            FUN = function(x){
                tempo_log <- base::grepl(x = arg_full_names, pattern = base::paste0("^", x), perl = FALSE)
                if(base::sum(tempo_log, na.rm = TRUE) > 1){ 
                    base::return(arg_full_names[tempo_log][base::which.min(base::nchar(arg_full_names[tempo_log]))])
                }
            }, 
            X = arg_full_names
        ))
        if( ! base::is.null(same_begin)){
            tempo_col8_end <- base::paste0("WARNING: SEVERAL ARGUMENT NAMES OF THE FUNCTION BEGINNING WITH ", base::paste0(same_begin[ ! base::is.null(same_begin)], collapse = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        }
        # end detection of arguments that starts by the same string in the sub function
        # checking if arg name are not fully written
        arg_full_symbol_type <- base::sapply(X = arg_full, FUN = function(x){base::all(base::typeof(x) == "symbol", na.rm =TRUE)}) # to check if any arg without optional value are completed with obs arg values
        if(base::any(arg_full_symbol_type, na.rm =TRUE) & base::length(tempo_split) == 0){
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nTHE TESTED FUNCTION ", arg_user_setting_x, " SEEMS TO HAVE A WRITTING ERROR IN LINE ",  col1_i2, " AND FUNCTION ", col2_i2, ".\nPLEASE, RUN THE TESTED FUNCTION FIRST.")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        tempo_col8 <- NULL
        if(( ! base::is.null(missing_args_names)) & base::length(tempo_split) != 0){
            for(i3 in 1:base::length(tempo_split)){
                if(base::grepl(x = tempo_split[i3], pattern = pattern1, perl = TRUE)){
                    tempo_arg_name <- base::strsplit(tempo_split[i3], split = "[\\s\\r\\n]*=", perl = TRUE)[[1]][1]
                    tempo_arg_name <- base::gsub(pattern = "^[\\s]*", replacement = "", x = tempo_arg_name) # removing leading space
                    if( ! base::is.null(same_begin)){
                        if( ! tempo_arg_name %in% same_begin){
                            tempo.log <- base::grepl(x = missing_args_names, pattern = base::paste0("^", tempo_arg_name), perl = FALSE)
                            if(base::sum(tempo.log, na.rm = TRUE) > 1){
                                tempo.cat <- base::paste0("INTERNAL ERROR 2 IN .all_args_here_fill() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nIN LINE ", i2, " IN THE ", col2_i2, " FUNCTION\ntempo_arg_name DETECTS SEVERAL TIMES ARGUMENT NAMES:\n\ntempo_arg_name:\n", tempo_arg_name, "\n\nmissing_args_names:\n", base::paste(missing_args_names, collapse = "\n"), "\n\nmissing_args_names[tempo.log]:\n", base::paste(missing_args_names[tempo.log], collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
                                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                            }
                            if(base::sum(tempo.log, na.rm = TRUE) == 1){
                                tempo_col8 <- base::c(
                                    tempo_col8, 
                                    base::paste0(
                                        base::ifelse(test = base::is.null(tempo_col8), yes = "", no = " ; "), 
                                        base::paste0(tempo_arg_name, " ARG NAME HAS TO BE FULLY WRITTEN ", missing_args_names[tempo.log])
                                    )
                                )
                            }
                        }
                    }
                }
            }
            if(( ! base::is.null(tempo_col8)) & ! base::is.null(tempo_col8_end)){
                tempo_col8 <- base::paste0(tempo_col8, " & ", tempo_col8_end, collapse = NULL, recycle0 = FALSE)
            }
        }
        # end checking if arg name are not fully written
        # when ... is present or not
            # Of note, when ... is present, argument values must be preceeded by their arg name. This means that values without arg names of the scanned function are ...
            # Otherwise, the first value without names must take the first arg name not already used, the second value without names must take the second, etc., then finish by the none used arg names with their default values
        missing_arg_log <- arg_full_names %in% missing_args_names
        if(base::any(three_dots_log, na.rm = TRUE) & base::all( ! arg_full_symbol_type, na.rm =TRUE)){ # ... present but no mandatory args with value to set 
            missing_args <-  base::unlist(base::mapply(FUN = function(x, y){base::paste0(x, " = ", if(base::is.null(y)){"NULL"}else{y})}, x = arg_full_names[missing_arg_log], y = arg_full[missing_arg_log], SIMPLIFY = TRUE)) # missing arg values with names
            good_args <- base::c(
                tempo_split[ ! tempo_split %in% good_args], # arg values without names
                good_args, # obs arg values with names
                base::paste0(" ", missing_args) # missing arg values with names #a space added to finally have  comma followed by a space
            )
        }else{
            count_good_args <- 0
            final <- NULL
            missing_args <-  NULL
            for(i3 in 1:base::length(arg_full_names)){ # here I cannot have more args than base::length(arg_full_names)
                if(missing_arg_log[i3] == TRUE){
                    if(base::sum(obs_arg_log) > 0){ # this means that remains obs arg with no arg names written
                        tempo <- base::paste0(arg_full_names[i3], " = ", tempo_split[base::which(obs_arg_log == TRUE)[1]])
                        obs_arg_log[base::which(obs_arg_log == TRUE)[1]] <- FALSE
                    }else{
                        tempo <- base::paste0(arg_full_names[i3], " = ", if(base::is.null(base::deparse(arg_full[[i3]]))){"NULL"}else{base::deparse(arg_full[[i3]])})
                    }
                    missing_args <- base::c(missing_args, tempo)
                    final <- base::c(final, base::ifelse(test = i3 == 1, yes = tempo, no = base::paste0(" ", tempo))) # take the first pos always of the args with no arg names
                }else{
                    count_good_args <- count_good_args + 1
                    final <- base::c(final, good_args[count_good_args])
                }
                arg_full_symbol_type[i3] <- FALSE
            }
            good_args <- final
            if(base::any(arg_full_symbol_type)){
                tempo.cat <- base::paste0("INTERNAL ERROR 3 IN .all_args_here_fill() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nARGUMENT WITHOUT OPTIONAL VALUES (MANDATORY ARGS) CANNOT REMAIN WITHOUT VALUE:\n\narg_full_symbol_type:\n", base::paste(arg_full_symbol_type, collapse = "\n"), "\n\narg_full_names:\n", base::paste(arg_full_names, collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
            if(( ! base::any(three_dots_log, na.rm = TRUE)) & base::any(obs_arg_log, na.rm =TRUE)){
                tempo.cat <- base::paste0("INTERNAL ERROR 4 IN .all_args_here_fill() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nCANNOT HAVE OBS ARGUMENT NOT INCORPORATED YET IF ! base::any(three_dots_log, na.rm = TRUE) IS TRUE:\n\nthree_dots_log:\n", base::paste(three_dots_log, collapse = " "), "\n\nobs_arg_log:\n", base::paste(obs_arg_log, collapse = " "), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
            if(count_good_args > base::length(tempo_split)){
                tempo.cat <- base::paste0("INTERNAL ERROR 5 IN .all_args_here_fill() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\ncount_good_args + 1 CANNOT BE MORE THAN length(tempo_split):\n\nlength(tempo_split): ", base::length(tempo_split), "\n\ncount_good_args + 1: ", count_good_args + 1, base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
            if(base::any(three_dots_log, na.rm = TRUE) & base::any(obs_arg_log, na.rm =TRUE)){ # obs values not yet in good_args
                if(count_good_args + 1 <= base::length(tempo_split)){
                    good_args <- base::c(good_args, tempo_split[obs_arg_log])
                }
            }
        }
        # end when ... is present or not
        # col5 done above
        col6 <- base::paste(missing_args_names, collapse = ", ") # if NULL return ""
        col7 <- base::paste(missing_args, collapse = ", ")  # if NULL return ""
        tempo <- base::paste0(
            col2_i2, 
            "(", 
            base::ifelse(test = ! base::is.null(supp_args_in_three_dots), yes = base::gsub(pattern = "^[\\s]*", replacement = "", x = base::paste0(supp_args_in_three_dots, collapse = ","), perl = TRUE), no = ""), 
            base::ifelse(test = ( ! base::is.null(supp_args_in_three_dots)) & ( ! base::is.null(good_args)) , yes = ",", no = ""), 
            base::ifelse(test = ! base::is.null(good_args), yes = base::paste0(good_args, collapse = ","), no = ""),
            ")"
        )
        if(base::length(arg_full_names) == good_count){
            col8 <- "GOOD"
        }else{
            col8 <- tempo
        }
        if( ! base::is.null(tempo_col8)){
            col8 <- tempo_col8
        }
    }
    base::return(base::list(col6 = col6, col7 = col7, col8 = col8))
}


