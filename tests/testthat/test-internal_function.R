test_that(".pack_and_function_check throws an error when fun argument doesn't contain ::", {
    # .pack_and_function_check throws an error when fun argument doesn't contain ::
    expect_error(.pack_and_function_check(fun = "geom_point", lib.path = "/path/to/lib", external.function.name = "test_function"))
    
    
    # .pack_and_function_check throws an error when package specified in fun is not installed
    expect_error(.pack_and_function_check(fun = "nonexistent_package::some_function", lib.path = "/path/to/lib", external.function.name = "test_function"))
    
    
    # .pack_and_function_check throws an error when function specified in fun is not available in the specified package"
    expect_error(.pack_and_function_check(fun = "ggplot2::nonexistent_function", lib.path = "/path/to/lib", external.function.name = "test_function"))
    
    # Tests for .base_function_check function
    # .base_function_check throws an error when reserved objects are present outside base package"
    expect_error(.base_function_check(external.function.name = "test_function"))
    
})