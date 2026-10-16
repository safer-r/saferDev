# Teardown file, sourced by testthat at the end of every test run (see ?testthat::setup).
# Removes the files created as side effects by the tests, none of which is read back:
# - Rplots.pdf: created by the plot(1) calls of test-get_message.R (Rscript without an open graphic device)
# - results.txt: created by the report() calls of test-report.R (already removed at the end of that test file, but kept here in case that test file is interrupted)
# - check_pos_fun_args_pos_internal_error_*.txt and while_loop_fun_args_pos_internal_error_*.txt: created by the internal error reports of .fun_args_pos() in R/intern_.fun_args_pos.R
# - _snaps/: created by testthat at each run start and normally removed by testthat itself when empty at run end, but kept here in case the run is interrupted

base::unlink(
    x = base::c(
        "Rplots.pdf",
        "results.txt",
        base::list.files(path = ".", pattern = "fun_args_pos_internal_error_[0-9]+\\.txt$", all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE, fixed = FALSE)
    ),
    recursive = FALSE,
    force = TRUE,
    expand = TRUE
)
if(base::dir.exists(paths = "_snaps")){
    if(base::length(x = base::list.files(path = "_snaps", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE, fixed = FALSE)) == 0L){
        base::unlink(x = "_snaps", recursive = TRUE, force = TRUE, expand = TRUE)
    }
}
