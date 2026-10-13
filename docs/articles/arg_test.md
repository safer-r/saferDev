# arg_test

  

``` r
library(saferDev)
```

  

### Datasets

``` r
f <- "unique"
argum <- c("x", "incomparables")
value <- list(x = list(1:10, c(1,1,2,8), NA), incomparable = c(TRUE, FALSE, NA))
error <- list(x = list(FALSE, FALSE, TRUE), incomparable = c(FALSE, FALSE, TRUE))
```

  

### Datasets info

``` r
f # character string indicating the name of the function tested 
> [1] "unique"
argum # character vector of the arguments of fun
> [1] "x"             "incomparables"
value # List in which each compartment containing values of the corresponding argument in arg
> $x
> $x[[1]]
>  [1]  1  2  3  4  5  6  7  8  9 10
> 
> $x[[2]]
> [1] 1 1 2 8
> 
> $x[[3]]
> [1] NA
> 
> 
> $incomparable
> [1]  TRUE FALSE    NA
error # List indicating if an error is expected
> $x
> $x[[1]]
> [1] FALSE
> 
> $x[[2]]
> [1] FALSE
> 
> $x[[3]]
> [1] TRUE
> 
> 
> $incomparable
> [1] FALSE FALSE  TRUE
```

  

### Simple examples

``` r
arg_test(
    fun = f, 
    arg = argum, 
    val = value
)
> 
> test JOB IGNITION
> 
> THE TOTAL NUMBER OF TESTS IS: 9
> 
> LOOP PROCESS ENDED | LOOP 9 / 9 | TIME SPENT: 52S
> $fun
> [1] "unique"
> 
> $ini
> arg_test(fun = f, arg = argum, val = value)
> 
> $data
>                               x incomparables kind problem message
> arg_test_1 1 2 3 4 5 6 7 8 9 10          TRUE   OK   FALSE        
> arg_test_2 1 2 3 4 5 6 7 8 9 10         FALSE   OK   FALSE        
> arg_test_3 1 2 3 4 5 6 7 8 9 10            NA   OK   FALSE        
> arg_test_4              1 1 2 8          TRUE   OK   FALSE        
> arg_test_5              1 1 2 8         FALSE   OK   FALSE        
> arg_test_6              1 1 2 8            NA   OK   FALSE        
> arg_test_7                   NA          TRUE   OK   FALSE        
> arg_test_8                   NA         FALSE   OK   FALSE        
> arg_test_9                   NA            NA   OK   FALSE        
> 
> $sys.info
> R version 4.3.1 (2023-06-16 ucrt)
> Platform: x86_64-w64-mingw32/x64 (64-bit)
> Running under: Windows 10 x64 (build 19045)
> 
> Matrix products: default
> 
> 
> locale:
> [1] LC_COLLATE=French_France.utf8  LC_CTYPE=French_France.utf8   
> [3] LC_MONETARY=French_France.utf8 LC_NUMERIC=C                  
> [5] LC_TIME=French_France.utf8    
> 
> time zone: Europe/Paris
> tzcode source: internal
> 
> attached base packages:
> [1] stats     graphics  grDevices utils     datasets  methods   base     
> 
> other attached packages:
> [1] saferDev_1.0
> 
> loaded via a namespace (and not attached):
>  [1] askpass_1.1       bslib_0.5.1       cachem_1.0.8      cli_3.6.1        
>  [5] colorspace_2.1-0  compiler_4.3.1    desc_1.4.2        digest_0.6.33    
>  [9] dplyr_1.1.3       evaluate_0.22     fansi_1.0.4       fastmap_1.1.1    
> [13] fs_1.6.3          generics_0.1.3    ggplot2_3.4.4     glue_1.6.2       
> [17] grid_4.3.1        gtable_0.3.4      htmltools_0.5.7   jquerylib_0.1.4  
> [21] jsonlite_1.8.7    knitr_1.44        lattice_0.21-8    lifecycle_1.0.4  
> [25] lubridate_1.9.2   magrittr_2.0.3    Matrix_1.5-4.1    memoise_2.0.1    
> [29] munsell_0.5.0     parallel_4.3.1    pdftools_3.3.3    pillar_1.9.0     
> [33] pkgconfig_2.0.3   pkgdown_2.0.7     png_0.1-8         purrr_1.0.2      
> [37] qpdf_1.3.2        R6_2.5.1          ragg_1.2.5        Rcpp_1.0.11      
> [41] reticulate_1.32.0 rlang_1.1.2       rmarkdown_2.25    rprojroot_2.0.3  
> [45] rstudioapi_0.15.0 sass_0.4.7        scales_1.2.1      stringi_1.7.12   
> [49] stringr_1.5.0     systemfonts_1.0.4 textshaping_0.3.6 tibble_3.2.1     
> [53] tidyselect_1.2.0  timechange_0.2.0  tools_4.3.1       utf8_1.2.3       
> [57] vctrs_0.6.5       xfun_0.40         yaml_2.3.7
```

  

### All the arguments

``` r
arg_test(
    fun = f, 
    arg = argum, 
    val= value, 
    expect.error = error, 
    parall = FALSE, 
    thread.nb = NULL, 
    print.count = 10, 
    plot.fun = FALSE, 
    export = TRUE, 
    res.path = ".", 
    lib.path = NULL
) # check the function "mean" in the environment one step above the local environment
> 
> test JOB IGNITION
> 
> THE TOTAL NUMBER OF TESTS IS: 9
> 
> LOOP PROCESS ENDED | LOOP 9 / 9 | TIME SPENT: 52S
> 
> DISCREPANCIES BETWEEN EXPECTED AND OBSERVED ERRORS (SEE THE discrepancy_table_from_arg_test_1-9.tsv FILE)
> 
> test JOB END
> 
> TIME: 2024-01-29 16:19:35.3553
> 
> TOTAL TIME LAPSE: 52S
```
