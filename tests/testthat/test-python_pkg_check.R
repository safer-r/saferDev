test_that("test if the python package is in the computer", {
  result <- python_pkg_check(
    req.package = "serpentine", 
    python.exec.path = ".", 
    python.lib.path = ".",
    lib.path = "."
)
  expect_error(object = result, regexp = NULL)
})
