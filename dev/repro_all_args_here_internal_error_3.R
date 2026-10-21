# Minimal reproducible example for the INTERNAL ERROR 3 crash of
# saferDev::all_args_here() caused by a '\' string literal (an escaped
# backslash-quote) inside the checked function.
#
# Bug mechanism, verified step by step (2026-10-21):
# 1. all_args_here() assembles the checked function body into ONE line of
#    text in which quotes appear OVER-escaped, exactly as printed by
#    capture.output(): a literal '\' written in the source, e.g.
#        base::c("LAMBDA", "'\\'")
#    becomes on that one line (9 chars between the outer quotes):
#        \"'\\'\"
# 2. The preprocessing of .in_quotes_replacement() (4 gsub rules that remove
#    quote pairs: "\"'" , "'\"" , "'" , "\"") fails to neutralize this
#    construct. Verified: on the 6-char fragment \"'\\'\" the 4 rules remove
#    only the backslash escapes, leaving an UNBALANCED quote character
#    behind (verified parity: input 2 double quotes / 2 single quotes ->
#    output still 2 double quotes / 2 single quotes, but no longer paired,
#    so quote parsing is desynchronized from that point on).
# 3. From the literal onwards, .in_quotes_replacement() considers the whole
#    remaining text as being INSIDE quotes, and therefore replaces every
#    real ( and ) character by a space. The closing ) of a subsequent call
#    disappears from the cleaned text fun_1_line_replace, while the opening
#    ( remains (it was before the literal).
# 4. .fun_args_pos() is then called for that call with a text whose
#    open_paren_pos is present but whose close_paren_pos is missing: the
#    bracket-counting while loop never comes back to count == 0 and hits its
#    safety check:
#        "INTERNAL ERROR 3 ... DID NOT PROPERLY DETECT THE POSITION OF THE
#        CLOSING BRACKET"
#    (the checked text is dumped to
#    while_loop_fun_args_pos_internal_error_3.txt in the working directory).
#
# The same defect is present in curly_bracket_check() at line 614 of
# R/curly_bracket_check.R (token "'\\'" in the
# base::c("LAMBDA", "'\\'") list), which is why
# all_args_here(x = saferDev::curly_bracket_check) crashes: run
# all_args_here(x = my_fun_crash) below for the 3-line minimal case.
#
# Run with: Rscript dev/repro_all_args_here_internal_error_3.R

base::setwd("C:/Users/gmillot/Documents/Git_projects/safer-r/saferDev")
pkgload::load_all(".", quiet = TRUE)

cat("\n===== Step A: the same function WITHOUT a '\' literal (works) =====\n")
my_fun_ok <- function(x){
    kind <- "LAMBDA" # simple string literal: no problem
    base::return(base::paste0(x, kind))
}
out <- saferDev::all_args_here(
    x = my_fun_ok,
    table_export = FALSE,
    fun_export = FALSE,
    out_path = "C:/Users/gmillot/AppData/Local/Temp/opencode",
    df_name = "repro_ok",
    overwrite = TRUE
)
cat("-> NO error, returned object of class:", base::class(x = out)[1], "\n")

cat("\n===== Step B: the same function with ONE '\' literal (crashes) =====\n")
my_fun_crash <- function(x){
    kind <- base::c("LAMBDA", "'\\'") # escaped backslash-quote literal: TRIGGERS THE BUG
    base::return(base::paste0(x, kind))
}
crash <- base::try(
    expr = saferDev::all_args_here(
        x = my_fun_crash,
        table_export = FALSE,
        fun_export = FALSE,
        out_path = "C:/Users/gmillot/AppData/Local/Temp/opencode",
        df_name = "repro_crash",
        overwrite = TRUE
    ),
    silent = FALSE
)
cat("\n-> error caught:", base::inherits(x = crash, what = "try-error"), "\n")
if(base::inherits(x = crash, what = "try-error")){
    err_txt <- base::conditionMessage(c = attr(x = crash, which = "condition"))
    cat("-> INTERNAL ERROR 3 detected:", base::grepl(x = err_txt, pattern = "INTERNAL ERROR 3", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), "\n")
    cat("-> error message head:\n")
    cat(base::substr(x = err_txt, start = 1, stop = 700), "\n")
    # remove the dump file written in the current working directory
    base::file.remove("while_loop_fun_args_pos_internal_error_3.txt")
}
