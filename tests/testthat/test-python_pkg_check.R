test_that("multiplication works", {
  result <- python_pkg_check(
    req.package = "serpentine", 
    python.exec.path = ".", 
    python.lib.path = ".",
    lib.path = "."
)
  expected <- "SOME VARIABLES OF mean ARE ALSO PRESENT IN :\npackage:base: mean\n"
  expect_error(result, expected)
})
