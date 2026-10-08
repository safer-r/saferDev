# arg_test

Test combinations of argument values of a function.

## Usage

``` r
arg_test(
  fun,
  arg,
  val,
  expect.error = NULL,
  parall = FALSE,
  thread.nb = NULL,
  print.count = 10,
  plot.fun = FALSE,
  export = FALSE,
  res.path = NULL,
  lib.path = NULL
)
```

## Arguments

- fun:

  Single character string indicating the name of the function tested
  (without brackets).

- arg:

  Vector of character strings of arguments of fun. At least arguments
  that do not have default values must be present in this vector.

- val:

  List with number of compartments equal to the length of arg, each
  compartment containing values of the corresponding argument in arg.
  Each different value must be in a list or in a vector. For instance,
  argument 3 in arg is a logical argument (values accepted TRUE, FALSE,
  NA). Thus, compartment 3 of val can be either list(TRUE, FALSE, NA),
  or c(TRUE, FALSE, NA). NULL value alone must be written list(NULL).

- expect.error:

  List of exactly the same structure as val argument, but containing
  FALSE or TRUE, depending on whether error is expected (TRUE) or not
  (FALSE) for each corresponding value of val. A message is returned
  depending on discrepancies between the expected and observed errors.
  See the examples below. BEWARE: not always possible to write the
  expected errors for all the combination of argument values. Ignored if
  NULL.

- parall:

  Single logical value. Force parallelization ?

- thread.nb:

  Single numeric integer indicating the number of threads to use if ever
  parallelization is required. If NULL, all the available threads will
  be used. Ignored if parall is FALSE.

- print.count:

  Single interger value. Print a working progress message every
  print.count during loops. BEWARE: can increase substentially the time
  to complete the process if using a small integer value, like 10 for
  instance. Use Inf if no loop message desired.

- plot.fun:

  Single logical value. Plot the plotting function tested for each test?
  Ignored if the tested function is not a graphic function.

- export:

  Single logical value. Export the results into a .RData file and into a
  .tsv file? If FALSE, return a list into the console (see below).
  BEWARE: will be automatically set to TRUE if parall is TRUE. This
  means that when using parallelization, the results are systematically
  exported, not returned into the console.

- res.path:

  Single character string indicating the absolute pathway of the folder
  where the txt results and pdfs, containing all the plots, will be
  saved. Several txt and pdf, one per thread, if parallelization.
  Ignored if export is FALSE. Must be specified if parall is TRUE or if
  export is TRUE.

- lib.path:

  Vector of characters specifying the absolute pathways of the
  directories containing the required packages if not in the default
  directories. Ignored if NULL.

## Value

One or several pdf if a plotting function is tested and if the plot.fun
argument is TRUE.

And then, if export is FALSE a list containing:

\- \$fun: the tested function.

\- \$ini: the initial input values.

\- \$data: a data frame of all the combination tested, containing the
following columns:

\# the different values tested, named by arguments - \$kind: a vector of
character strings indicating the kind of test result: either "ERROR", or
"WARNING", or "OK". - \$problem: a logical vector indicating if error or
not.

\- \$expected.error: optional logical vector indicating the expected
error specified in the expect.error argument. - \$message: either NULL
if \$kind is always "OK", or the messages.

\- \$sys.info: system and packages info.

If export is TRUE:

\- the same list object into a .RData file.

\- also the \$data data frame into a .tsv file.

\- if expect.error is non NULL and if any discrepancy, the \$data data
frame into a .tsv file but containing only the rows with discrepancies
between expected and observed errors.

## Details

Limited to 43 arguments with at least 2 values each. The total number of
arguments tested can be more if the additional arguments have a single
value. The limit is due to nested "for" loops
(https://stat.ethz.ch/pipermail/r-help/2008-March/157341.html), but this
limitation is away from the number of tests performed that would be
2^43.

## Examples

``` r
arg_test(fun = "unique", arg = c("x", "incomparables"), 
val = list(x = list(1:10, c(1,1,2,8), NA), incomparable = c(TRUE, FALSE, NA)))
#> 
#> test JOB IGNITION
#> 
#> THE TOTAL NUMBER OF TESTS IS: 9
#> 
#> LOOP PROCESS ENDED | LOOP 9 / 9 | TIME SPENT: 17S
#> 
#> $fun
#> [1] "unique"
#> 
#> $ini
#> arg_test(fun = "unique", arg = c("x", "incomparables"), val = list(x = list(1:10, 
#>     c(1, 1, 2, 8), NA), incomparable = c(TRUE, FALSE, NA)))
#> 
#> $data
#>                               x incomparables kind problem message
#> arg_test_1 1 2 3 4 5 6 7 8 9 10          TRUE   OK   FALSE        
#> arg_test_2 1 2 3 4 5 6 7 8 9 10         FALSE   OK   FALSE        
#> arg_test_3 1 2 3 4 5 6 7 8 9 10            NA   OK   FALSE        
#> arg_test_4              1 1 2 8          TRUE   OK   FALSE        
#> arg_test_5              1 1 2 8         FALSE   OK   FALSE        
#> arg_test_6              1 1 2 8            NA   OK   FALSE        
#> arg_test_7                   NA          TRUE   OK   FALSE        
#> arg_test_8                   NA         FALSE   OK   FALSE        
#> arg_test_9                   NA            NA   OK   FALSE        
#> 
#> $sys.info
#> R version 4.3.1 (2023-06-16 ucrt)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows 10 x64 (build 19045)
#> 
#> Matrix products: default
#> 
#> 
#> locale:
#> [1] LC_COLLATE=C                   LC_CTYPE=French_France.utf8   
#> [3] LC_MONETARY=French_France.utf8 LC_NUMERIC=C                  
#> [5] LC_TIME=French_France.utf8    
#> 
#> time zone: Europe/Paris
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] saferDev_1.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] Matrix_1.5-4.1    R6_2.5.1          Rcpp_1.0.11       askpass_1.1      
#>  [5] brio_1.1.3        bslib_0.5.1       cachem_1.0.8      cli_3.6.1        
#>  [9] colorspace_2.1-0  compiler_4.3.1    curl_5.0.2        desc_1.4.2       
#> [13] digest_0.6.33     downlit_0.4.3     dplyr_1.1.3       evaluate_0.22    
#> [17] fansi_1.0.4       fastmap_1.1.1     fs_1.6.3          generics_0.1.3   
#> [21] ggplot2_3.4.4     glue_1.6.2        grid_4.3.1        gtable_0.3.4     
#> [25] htmltools_0.5.7   httr_1.4.7        jquerylib_0.1.4   jsonlite_1.8.7   
#> [29] knitr_1.44        lattice_0.21-8    lifecycle_1.0.4   lubridate_1.9.2  
#> [33] magrittr_2.0.3    memoise_2.0.1     munsell_0.5.0     parallel_4.3.1   
#> [37] pdftools_3.3.3    pillar_1.9.0      pkgconfig_2.0.3   pkgdown_2.0.7    
#> [41] png_0.1-8         purrr_1.0.2       qpdf_1.3.2        ragg_1.2.5       
#> [45] remotes_2.4.2.1   reticulate_1.32.0 rlang_1.1.2       rmarkdown_2.25   
#> [49] rprojroot_2.0.3   rstudioapi_0.15.0 sass_0.4.7        scales_1.2.1     
#> [53] systemfonts_1.0.4 testthat_3.2.0    textshaping_0.3.6 tibble_3.2.1     
#> [57] tidyselect_1.2.0  timechange_0.2.0  tools_4.3.1       utf8_1.2.3       
#> [61] vctrs_0.6.5       whisker_0.4.1     withr_2.5.1       xfun_0.40        
#> [65] xml2_1.3.5        yaml_2.3.7       
#> 

arg_test(fun = "unique", arg = c("x", "incomparables"), 
val = list(x = list(1:10, c(1,1,2,8), NA), 
incomparable = c(TRUE, FALSE, NA)), expect.error = list(x = list(FALSE, FALSE, TRUE), 
incomparable = c(FALSE, FALSE, TRUE)))
#> 
#> test JOB IGNITION
#> 
#> THE TOTAL NUMBER OF TESTS IS: 9
#> 
#> LOOP PROCESS ENDED | LOOP 9 / 9 | TIME SPENT: 17S
#> 
#> DISCREPANCIES BETWEEN EXPECTED AND OBSERVED ERRORS (SEE THE $data RESULT)
#> 
#> $fun
#> [1] "unique"
#> 
#> $ini
#> arg_test(fun = "unique", arg = c("x", "incomparables"), val = list(x = list(1:10, 
#>     c(1, 1, 2, 8), NA), incomparable = c(TRUE, FALSE, NA)), expect.error = list(x = list(FALSE, 
#>     FALSE, TRUE), incomparable = c(FALSE, FALSE, TRUE)))
#> 
#> $data
#>                               x incomparables kind problem expected.error
#> arg_test_1 1 2 3 4 5 6 7 8 9 10          TRUE   OK   FALSE          FALSE
#> arg_test_2 1 2 3 4 5 6 7 8 9 10         FALSE   OK   FALSE          FALSE
#> arg_test_3 1 2 3 4 5 6 7 8 9 10            NA   OK   FALSE           TRUE
#> arg_test_4              1 1 2 8          TRUE   OK   FALSE          FALSE
#> arg_test_5              1 1 2 8         FALSE   OK   FALSE          FALSE
#> arg_test_6              1 1 2 8            NA   OK   FALSE           TRUE
#> arg_test_7                   NA          TRUE   OK   FALSE           TRUE
#> arg_test_8                   NA         FALSE   OK   FALSE           TRUE
#> arg_test_9                   NA            NA   OK   FALSE           TRUE
#>            message
#> arg_test_1        
#> arg_test_2        
#> arg_test_3        
#> arg_test_4        
#> arg_test_5        
#> arg_test_6        
#> arg_test_7        
#> arg_test_8        
#> arg_test_9        
#> 
#> $sys.info
#> R version 4.3.1 (2023-06-16 ucrt)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows 10 x64 (build 19045)
#> 
#> Matrix products: default
#> 
#> 
#> locale:
#> [1] LC_COLLATE=C                   LC_CTYPE=French_France.utf8   
#> [3] LC_MONETARY=French_France.utf8 LC_NUMERIC=C                  
#> [5] LC_TIME=French_France.utf8    
#> 
#> time zone: Europe/Paris
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] saferDev_1.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] Matrix_1.5-4.1    R6_2.5.1          Rcpp_1.0.11       askpass_1.1      
#>  [5] brio_1.1.3        bslib_0.5.1       cachem_1.0.8      cli_3.6.1        
#>  [9] colorspace_2.1-0  compiler_4.3.1    curl_5.0.2        desc_1.4.2       
#> [13] digest_0.6.33     downlit_0.4.3     dplyr_1.1.3       evaluate_0.22    
#> [17] fansi_1.0.4       fastmap_1.1.1     fs_1.6.3          generics_0.1.3   
#> [21] ggplot2_3.4.4     glue_1.6.2        grid_4.3.1        gtable_0.3.4     
#> [25] htmltools_0.5.7   httr_1.4.7        jquerylib_0.1.4   jsonlite_1.8.7   
#> [29] knitr_1.44        lattice_0.21-8    lifecycle_1.0.4   lubridate_1.9.2  
#> [33] magrittr_2.0.3    memoise_2.0.1     munsell_0.5.0     parallel_4.3.1   
#> [37] pdftools_3.3.3    pillar_1.9.0      pkgconfig_2.0.3   pkgdown_2.0.7    
#> [41] png_0.1-8         purrr_1.0.2       qpdf_1.3.2        ragg_1.2.5       
#> [45] remotes_2.4.2.1   reticulate_1.32.0 rlang_1.1.2       rmarkdown_2.25   
#> [49] rprojroot_2.0.3   rstudioapi_0.15.0 sass_0.4.7        scales_1.2.1     
#> [53] systemfonts_1.0.4 testthat_3.2.0    textshaping_0.3.6 tibble_3.2.1     
#> [57] tidyselect_1.2.0  timechange_0.2.0  tools_4.3.1       utf8_1.2.3       
#> [61] vctrs_0.6.5       whisker_0.4.1     withr_2.5.1       xfun_0.40        
#> [65] xml2_1.3.5        yaml_2.3.7       
#> 

arg_test(fun = "unique", arg = c("x", "incomparables"), 
val = list(x = list(1:10, c(1,1,2,8), NA), incomparable = c(TRUE, FALSE, NA)), 
expect.error = list(x = list(FALSE, FALSE, TRUE), incomparable = c(FALSE, FALSE, TRUE)), 
export = TRUE, res.path = getwd())
#> 
#> test JOB IGNITION
#> 
#> THE TOTAL NUMBER OF TESTS IS: 9
#> 
#> LOOP PROCESS ENDED | LOOP 9 / 9 | TIME SPENT: 17S
#> 
#> DISCREPANCIES BETWEEN EXPECTED AND OBSERVED ERRORS (SEE THE discrepancy_table_from_arg_test_1-9.tsv FILE)
#> 
#> test JOB END
#> 
#> TIME: 2024-01-29 16:16:30.412494
#> 
#> TOTAL TIME LAPSE: 17S
#> 
#> 

# Return an error if you run this example
# arg_test(fun = "round", arg = c("data", "dec.nb", "after.lead.zero"), val = list(L1 = list(c(1, 1.0002256, 1.23568), "a", NA), L2 = list(2, c(1,3), NA), L3 = c(TRUE, FALSE, NA)))

arg_test(fun = "plot", arg = c("x", "y"), val = list(x = list(1:10, 12:13, NA, (1:10)^2), 
y = list(1:10, NA, NA)),  expect.error = list(x = list(FALSE, TRUE, TRUE, FALSE), 
y = list(FALSE, TRUE, TRUE)), parall = FALSE, thread.nb = NULL, plot.fun = TRUE, 
res.path = ".", lib.path = NULL)
#> 
#> test JOB IGNITION
#> 
#> THE TOTAL NUMBER OF TESTS IS: 12
#> 
#> LOOP 10 / 12 | TIME SPENT: 19S | EXPECTED END: 2024-01-29 16:16:54.168458
#> LOOP PROCESS ENDED | LOOP 12 / 12 | TIME SPENT: 22S
#> 
#> NO DISCREPANCY BETWEEN EXPECTED AND OBSERVED ERRORS
#> 
#> $fun
#> [1] "plot"
#> 
#> $ini
#> arg_test(fun = "plot", arg = c("x", "y"), val = list(x = list(1:10, 
#>     12:13, NA, (1:10)^2), y = list(1:10, NA, NA)), expect.error = list(x = list(FALSE, 
#>     TRUE, TRUE, FALSE), y = list(FALSE, TRUE, TRUE)), parall = FALSE, 
#>     thread.nb = NULL, plot.fun = TRUE, res.path = ".", lib.path = NULL)
#> 
#> $data
#>                                       x                    y  kind problem
#> arg_test_01        1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10    OK   FALSE
#> arg_test_02        1 2 3 4 5 6 7 8 9 10                   NA ERROR    TRUE
#> arg_test_03        1 2 3 4 5 6 7 8 9 10                   NA ERROR    TRUE
#> arg_test_04                       12 13 1 2 3 4 5 6 7 8 9 10 ERROR    TRUE
#> arg_test_05                       12 13                   NA ERROR    TRUE
#> arg_test_06                       12 13                   NA ERROR    TRUE
#> arg_test_07                          NA 1 2 3 4 5 6 7 8 9 10 ERROR    TRUE
#> arg_test_08                          NA                   NA ERROR    TRUE
#> arg_test_09                          NA                   NA ERROR    TRUE
#> arg_test_10 1 4 9 16 25 36 49 64 81 100 1 2 3 4 5 6 7 8 9 10    OK   FALSE
#> arg_test_11 1 4 9 16 25 36 49 64 81 100                   NA ERROR    TRUE
#> arg_test_12 1 4 9 16 25 36 49 64 81 100                   NA ERROR    TRUE
#>             expected.error
#> arg_test_01          FALSE
#> arg_test_02           TRUE
#> arg_test_03           TRUE
#> arg_test_04           TRUE
#> arg_test_05           TRUE
#> arg_test_06           TRUE
#> arg_test_07           TRUE
#> arg_test_08           TRUE
#> arg_test_09           TRUE
#> arg_test_10          FALSE
#> arg_test_11           TRUE
#> arg_test_12           TRUE
#>                                                                                      message
#> arg_test_01                                                                                 
#> arg_test_02 Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n
#> arg_test_03 Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n
#> arg_test_04 Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n
#> arg_test_05 Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n
#> arg_test_06 Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n
#> arg_test_07 Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n
#> arg_test_08                          Error in plot.window(...) : need finite 'xlim' values\n
#> arg_test_09                          Error in plot.window(...) : need finite 'xlim' values\n
#> arg_test_10                                                                                 
#> arg_test_11 Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n
#> arg_test_12 Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n
#> 
#> $sys.info
#> R version 4.3.1 (2023-06-16 ucrt)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows 10 x64 (build 19045)
#> 
#> Matrix products: default
#> 
#> 
#> locale:
#> [1] LC_COLLATE=C                   LC_CTYPE=French_France.utf8   
#> [3] LC_MONETARY=French_France.utf8 LC_NUMERIC=C                  
#> [5] LC_TIME=French_France.utf8    
#> 
#> time zone: Europe/Paris
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] saferDev_1.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] Matrix_1.5-4.1    R6_2.5.1          Rcpp_1.0.11       askpass_1.1      
#>  [5] brio_1.1.3        bslib_0.5.1       cachem_1.0.8      cli_3.6.1        
#>  [9] colorspace_2.1-0  compiler_4.3.1    curl_5.0.2        desc_1.4.2       
#> [13] digest_0.6.33     downlit_0.4.3     dplyr_1.1.3       evaluate_0.22    
#> [17] fansi_1.0.4       fastmap_1.1.1     fs_1.6.3          generics_0.1.3   
#> [21] ggplot2_3.4.4     glue_1.6.2        grid_4.3.1        gtable_0.3.4     
#> [25] htmltools_0.5.7   httr_1.4.7        jquerylib_0.1.4   jsonlite_1.8.7   
#> [29] knitr_1.44        lattice_0.21-8    lifecycle_1.0.4   lubridate_1.9.2  
#> [33] magrittr_2.0.3    memoise_2.0.1     munsell_0.5.0     parallel_4.3.1   
#> [37] pdftools_3.3.3    pillar_1.9.0      pkgconfig_2.0.3   pkgdown_2.0.7    
#> [41] png_0.1-8         purrr_1.0.2       qpdf_1.3.2        ragg_1.2.5       
#> [45] remotes_2.4.2.1   reticulate_1.32.0 rlang_1.1.2       rmarkdown_2.25   
#> [49] rprojroot_2.0.3   rstudioapi_0.15.0 sass_0.4.7        scales_1.2.1     
#> [53] systemfonts_1.0.4 testthat_3.2.0    textshaping_0.3.6 tibble_3.2.1     
#> [57] tidyselect_1.2.0  timechange_0.2.0  tools_4.3.1       utf8_1.2.3       
#> [61] vctrs_0.6.5       whisker_0.4.1     withr_2.5.1       xfun_0.40        
#> [65] xml2_1.3.5        yaml_2.3.7       
#> 

arg_test(fun = "plot", arg = c("x", "y"), val = list(x = list(1:10, 12:13, NA, 
(1:10)^2), y = list(1:10, NA, NA)), parall = FALSE, thread.nb = 4, 
plot.fun = TRUE, res.path = ".", 
lib.path = NULL)
#> 
#> test JOB IGNITION
#> 
#> THE TOTAL NUMBER OF TESTS IS: 12
#> 
#> LOOP 10 / 12 | TIME SPENT: 18S | EXPECTED END: 2024-01-29 16:17:17.453953
#> LOOP PROCESS ENDED | LOOP 12 / 12 | TIME SPENT: 22S
#> 
#> $fun
#> [1] "plot"
#> 
#> $ini
#> arg_test(fun = "plot", arg = c("x", "y"), val = list(x = list(1:10, 
#>     12:13, NA, (1:10)^2), y = list(1:10, NA, NA)), parall = FALSE, 
#>     thread.nb = 4, plot.fun = TRUE, res.path = ".", lib.path = NULL)
#> 
#> $data
#>                                       x                    y  kind problem
#> arg_test_01        1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10    OK   FALSE
#> arg_test_02        1 2 3 4 5 6 7 8 9 10                   NA ERROR    TRUE
#> arg_test_03        1 2 3 4 5 6 7 8 9 10                   NA ERROR    TRUE
#> arg_test_04                       12 13 1 2 3 4 5 6 7 8 9 10 ERROR    TRUE
#> arg_test_05                       12 13                   NA ERROR    TRUE
#> arg_test_06                       12 13                   NA ERROR    TRUE
#> arg_test_07                          NA 1 2 3 4 5 6 7 8 9 10 ERROR    TRUE
#> arg_test_08                          NA                   NA ERROR    TRUE
#> arg_test_09                          NA                   NA ERROR    TRUE
#> arg_test_10 1 4 9 16 25 36 49 64 81 100 1 2 3 4 5 6 7 8 9 10    OK   FALSE
#> arg_test_11 1 4 9 16 25 36 49 64 81 100                   NA ERROR    TRUE
#> arg_test_12 1 4 9 16 25 36 49 64 81 100                   NA ERROR    TRUE
#>                                                                                      message
#> arg_test_01                                                                                 
#> arg_test_02 Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n
#> arg_test_03 Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n
#> arg_test_04 Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n
#> arg_test_05 Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n
#> arg_test_06 Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n
#> arg_test_07 Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n
#> arg_test_08                          Error in plot.window(...) : need finite 'xlim' values\n
#> arg_test_09                          Error in plot.window(...) : need finite 'xlim' values\n
#> arg_test_10                                                                                 
#> arg_test_11 Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n
#> arg_test_12 Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n
#> 
#> $sys.info
#> R version 4.3.1 (2023-06-16 ucrt)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows 10 x64 (build 19045)
#> 
#> Matrix products: default
#> 
#> 
#> locale:
#> [1] LC_COLLATE=C                   LC_CTYPE=French_France.utf8   
#> [3] LC_MONETARY=French_France.utf8 LC_NUMERIC=C                  
#> [5] LC_TIME=French_France.utf8    
#> 
#> time zone: Europe/Paris
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] saferDev_1.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] Matrix_1.5-4.1    R6_2.5.1          Rcpp_1.0.11       askpass_1.1      
#>  [5] brio_1.1.3        bslib_0.5.1       cachem_1.0.8      cli_3.6.1        
#>  [9] colorspace_2.1-0  compiler_4.3.1    curl_5.0.2        desc_1.4.2       
#> [13] digest_0.6.33     downlit_0.4.3     dplyr_1.1.3       evaluate_0.22    
#> [17] fansi_1.0.4       fastmap_1.1.1     fs_1.6.3          generics_0.1.3   
#> [21] ggplot2_3.4.4     glue_1.6.2        grid_4.3.1        gtable_0.3.4     
#> [25] htmltools_0.5.7   httr_1.4.7        jquerylib_0.1.4   jsonlite_1.8.7   
#> [29] knitr_1.44        lattice_0.21-8    lifecycle_1.0.4   lubridate_1.9.2  
#> [33] magrittr_2.0.3    memoise_2.0.1     munsell_0.5.0     parallel_4.3.1   
#> [37] pdftools_3.3.3    pillar_1.9.0      pkgconfig_2.0.3   pkgdown_2.0.7    
#> [41] png_0.1-8         purrr_1.0.2       qpdf_1.3.2        ragg_1.2.5       
#> [45] remotes_2.4.2.1   reticulate_1.32.0 rlang_1.1.2       rmarkdown_2.25   
#> [49] rprojroot_2.0.3   rstudioapi_0.15.0 sass_0.4.7        scales_1.2.1     
#> [53] systemfonts_1.0.4 testthat_3.2.0    textshaping_0.3.6 tibble_3.2.1     
#> [57] tidyselect_1.2.0  timechange_0.2.0  tools_4.3.1       utf8_1.2.3       
#> [61] vctrs_0.6.5       whisker_0.4.1     withr_2.5.1       xfun_0.40        
#> [65] xml2_1.3.5        yaml_2.3.7       
#> 

# set.seed(1) ; 
# obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10), 
# stringsAsFactors = TRUE) ; arg_test(fun = "fun_gg_boxplot", arg = c("data1", "y", "categ"), 
# val = list(L1 = list(L1 = obs1), L2 = list(L1 = "Time"), L3 = list(L1 = "Group1")))

# set.seed(1) ; 
# obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10), 
# stringsAsFactors = TRUE) ; 
# arg_test(fun = "fun_gg_boxplot", arg = c("data1", "y", "categ"), val = list(L1 = list(obs1), 
# L2 = "Time", L3 = "Group1"), parall = FALSE, thread.nb = NULL, plot.fun = TRUE, 
# res.path = "C:\Users\yhan\Desktop\", lib.path = "C:\Program Files\R\R-4.3.1\library\")

# library(ggplot2) ; arg_test(fun = "geom_histogram", arg = c("data", "mapping"), 
# val = list(x = list(data.frame(X = "a", stringsAsFactors = TRUE)), 
# y = list(ggplot2::aes(x = X))), parall = FALSE, thread.nb = NULL, 
# plot.fun = TRUE, res.path = "C:\Users\yhan\Desktop\", 
# lib.path = "C:\Program Files\R\R-4.3.1\library\") 
# BEWARE: ggplot2::geom_histogram does not work
```
