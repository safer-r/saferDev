# Build a clean patch: remove the -/+ identical .all_args_insert pair (EOL artifact) from the hunk, keep only the .find_construct_body addition
lines <- base::readLines(con = "C:/Users/gmillot/AppData/Local/Temp/opencode/mand_mine.patch", warn = FALSE, ok = TRUE)
# find the -/+ identical pair
minus_line <- base::grep(x = lines, pattern = "^-        .all_args_insert", fixed = FALSE)
plus_line <- base::grep(x = lines, pattern = "^\\+        .all_args_insert", fixed = FALSE)
# the + line that is identical except EOL to the - line: compare stripped
strip <- function(x){base::sub(x = x, pattern = "^[+-]", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = TRUE)}
pairs_idx <- NULL
for(m in minus_line){
    for(p in plus_line){
        if(base::identical(x = strip(lines[m]), y = strip(lines[p]))){
            pairs_idx <- base::c(pairs_idx, m, p)
        }
    }
}
if(base::length(x = pairs_idx) != 0){
    lines <- lines[-pairs_idx]
}
# fix the hunk header: original @@ -60,9 +60,10 @@ -> after removing 1- and 1+: -60,8 +60,9
lines <- base::sub(x = lines, pattern = "@@ -60,9 \\+60,10 @@", replacement = "@@ -60,8 +60,9 @@", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = TRUE)
base::writeLines(text = lines, con = "C:/Users/gmillot/AppData/Local/Temp/opencode/mand_mine_clean.patch", useBytes = TRUE)
cat("clean patch written\n")
base::cat(base::paste0(lines, collapse = "\n"), sep = "\n")