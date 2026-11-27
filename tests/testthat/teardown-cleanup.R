# Teardown file, sourced by testthat at the end of every test run (see ?testthat::setup).
# Removes the files that the tests could leave as side effects:
# - Rplots.pdf: safety net, in case a test opens a graphic device without closing it (no test is expected to create it)
# - _snaps/: created by testthat at each run start and normally removed by testthat itself when empty at run end, but kept here in case the run is interrupted

base::unlink(
    x = "Rplots.pdf",
    recursive = FALSE,
    force = TRUE,
    expand = TRUE
)
if(base::dir.exists(paths = "_snaps")){
    if(base::length(x = base::list.files(path = "_snaps", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)) == 0L){
        base::unlink(x = "_snaps", recursive = TRUE, force = TRUE, expand = TRUE)
    }
}
