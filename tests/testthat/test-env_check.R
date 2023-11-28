test_that("check if the object is in the environment one step above the env_check() environment, and if yes, returns if the same name exists in above environments", {
    mean <- 2
    result <- env_check(
        pos = 1,
        name = "mean"
    )
    expected <- "SOME VARIABLES OF mean ARE ALSO PRESENT IN :\npackage:base: mean\n"
    expect_equal(result, expected)
})
