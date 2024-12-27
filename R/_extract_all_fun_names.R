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


