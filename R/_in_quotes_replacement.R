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

