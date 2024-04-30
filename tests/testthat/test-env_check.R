test_that("check if the object is in the environment one step above the env_check() environment, and if yes, returns if the same name exists in above environments", {
    mean <- 2
    result <- env_check(
        pos = 1,
        name = "mean",
        safer_check = TRUE
    )
    expected <- "SOME VARIABLES OF mean ARE ALSO PRESENT IN :\npackage:base: mean\n"
    expect_equal(result, expected)

    sum <- "change"
    result2 <- env_check(
        pos = 1,
        name = "sum",
        safer_check = TRUE
    )
    expected2 <- "SOME VARIABLES OF sum ARE ALSO PRESENT IN :\npackage:base: mean sum\n"
    expect_equal(result2, expected2)
})
