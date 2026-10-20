# rmd patch: remove the true_false_check -/+ pair from the hunk, adjust header counts
lines <- base::readLines(con = "C:/Users/gmillot/AppData/Local/Temp/opencode/rmd_mine.patch", warn = FALSE, ok = TRUE)
minus_idx <- base::grep(x = lines, pattern = "^-\\| \\*\\*true_false_check", fixed = FALSE)
plus_idx <- base::grep(x = lines, pattern = "^\\+\\| \\*\\*true_false_check", fixed = FALSE)
keep <- base::rep(x = TRUE, times = base::length(x = lines))
keep[minus_idx] <- FALSE
keep[plus_idx] <- FALSE
lines2 <- lines[keep]
# header: @@ -21,13 +21,13 @@ -> remove 1 - and 1 + line: -21,12 +21,12
lines2 <- base::sub(x = lines2, pattern = "@@ -21,13 \\+21,13 @@", replacement = "@@ -21,12 +21,12 @@", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = TRUE)
base::writeLines(text = lines2, con = "C:/Users/gmillot/AppData/Local/Temp/opencode/rmd_mine_clean.patch", useBytes = TRUE)
cat("rmd clean patch written\n")
base::cat(base::paste0(lines2, collapse = "\n"), sep = "\n")