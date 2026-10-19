# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(saferDev)

test_check("saferDev")

# Remove Rplots.pdf at the end of the tests, always (created by graphic devices opened during the ggplot2 tests, e.g. in test-get_message.R)
if(base::file.exists("Rplots.pdf")){ # relative path: Rplots.pdf is created in the working directory of the test process (usually the package root)
    base::unlink(x = "Rplots.pdf", recursive = FALSE, force = TRUE)
}
