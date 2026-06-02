## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

There was 1 NOTE:
* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Gael Millot <gael.millot@pasteur.fr>'
  New submission
  Found the following (possibly) invalid URLs:
    URL: https://bugs.r-project.org/show_bug.cgi?id=18849
      From: man/all_args_here.Rd
      Status: 418
      Message: (Unused)

  This is a known false positive. The R bug tracker returns HTTP 418 to 
  programmatic requests, but the URL is valid in standard browsers.
