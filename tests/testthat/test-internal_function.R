test_that(".pack_and_function_check() and .base_function_check()", {
    # .pack_and_function_check throws an error when fun argument doesn't contain ::
    expect_error(.pack_and_function_check(fun = "geom_point", lib.path = "/path/to/lib", external.function.name = "test_function", external.package.name = "testthat"))
    
    
    # .pack_and_function_check throws an error when package specified in fun is not installed
    expect_error(.pack_and_function_check(fun = "nonexistent_package::some_function", lib.path = "/path/to/lib", external.function.name = "test_function", external.package.name = "testthat"))
    
    
    # .pack_and_function_check throws an error when function specified in fun is not available in the specified package"
    
    # .pack_and_function_check throws an error when package specified in fun is installed
    expect_error(.pack_and_function_check(fun = "ggplot2::geom_point", lib.path = "/path/to/lib", external.function.name = "test_function", external.package.name = "testthat"))
    
    # Tests for .base_function_check function
    # .base_function_check throws an error when reserved objects are present outside base package"
    expect_error(.base_function_check(external.function.name = "test_function", external.package.name = "testthat"))

    # .base_function_check throws an error when reserved objects are present outside base package"
    expect_error(.base_function_check(external.function.name = "geom_point", external.package.name = "ggplot2"))

    # .base_function_check throws an error when reserved objects are present outside base package"
    expect_error(.base_function_check(external.function.name = "clusterApply", external.package.name = "parallel"))
    
})