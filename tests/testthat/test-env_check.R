test_that("check if the object is in the environment one step above the env_check() environment, and if yes, returns if the same name exists in above environments", {
    pos <- 2
    name <- "mean"
    
    result1 <- env_check(pos = 2)
    expect_null(result1)
    
    result2 <- env_check(
        pos = 2,
        name = "mean"
    )
    expect_null(result2)
    
    result3 <- env_check(name = "mean")
    expected3 <- "SOME VARIABLES OF mean ARE ALSO PRESENT IN :\npackage:base: mean\n"
    expected_equal(result3,expected3)
    
    result4 <- env_check(
        pos = 1,
        name = "mean"
    )
    expected4 <- "SOME VARIABLES OF mean ARE ALSO PRESENT IN :\npackage:base: mean\n"
    expect_equal(result4, expected4)
})
