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

