# saferDev

## Description

Set of functions for the development of R functions and pipelines,
written according to the [safer-R project](https://github.com/safer-r)
specifications.

  

## Content

  

| Function | Description |
|:---|:---|
| **all_args_here()** | Verify that all the functions used inside a function are written with all their arguments. For instance: `base::paste0(letters[1:2], collapse = NULL, recycle0 = FALSE)` and not `paste0(letters[1:2])`. |
| **arg_check()** | Check expected values of arguments of functions: class, type, mode, length (each tested exactly, same values, in the same order), restricted values panel, kind of numeric values in addition to the distinction between ‘integer’ and ‘double’ (proportion only? Inf values authorized? negative values authorized? Integers of type ‘double’?). |
| **colons_check()** | Verify that all the functions used inside a function are all referenced by their package attribution. For instance: [`base::mean()`](https://rdrr.io/r/base/mean.html) and not [`mean()`](https://rdrr.io/r/base/mean.html), or `saferDev:::.base_op_check()` and not [`.base_op_check()`](../reference/dot-base_op_check.md). Use [prefixer](https://github.com/dreamrs/prefixer) to directly modify yor code. |
| **env_check()** | Verify that object names in the environment defined by the `pos` argument are identical or not to object names in the above environments (following R Scope). This can be used to verify that names used for objects inside a function or in the working environment do not override names of objects already present in the above R environments, following the R scope. |
| **get_message()** | Return the error, warning or simple (non error non warning) message if ever exist of an instruction written between quotes. |
| **is_function_here()** | Check if required functions are present in installed packages. This controls for instance modifications of function names depending on package versions. |
| **is_package_here()** | Check if required packages are installed locally. |
| **report()** | Print a character string or a data object into a same log file. |

  
