extract_hunks <- function(diff_path, marker, out_path){
    lines <- base::readLines(con = diff_path, warn = FALSE)
    # header lines before the first hunk
    first_hunk <- base::which(x = base::grepl(x = lines, pattern = "^@@", perl = TRUE))
    header <- lines[1:(first_hunk[1] - 1)]
    # hunk boundaries
    starts <- first_hunk
    ends <- base::c(starts[-1] - 1, base::length(x = lines))
    keep <- NULL
    for(i in base::seq_along(along.with = starts)){
        hunk <- lines[starts[i]:ends[i]]
        if(base::any(base::grepl(x = hunk, pattern = marker, fixed = TRUE))){
            keep <- base::c(keep, hunk)
        }
    }
    base::writeLines(text = base::c(header, keep), con = out_path, useBytes = TRUE)
}
extract_hunks("C:/Users/gmillot/AppData/Local/Temp/opencode/mand.diff", ".find_construct_body", "C:/Users/gmillot/AppData/Local/Temp/opencode/mand_mine.patch")
extract_hunks("C:/Users/gmillot/AppData/Local/Temp/opencode/rmd.diff", "curly_bracket_check", "C:/Users/gmillot/AppData/Local/Temp/opencode/rmd_mine.patch")
cat("patch written\n")