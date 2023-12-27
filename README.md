[//]: # "#to make links in gitlab: example with racon https://github.com/isovic/racon"
[//]: # "tricks in markdown: https://openclassrooms.com/fr/courses/1304236-redigez-en-markdown"

# cuteDev <a href=""><img src=".images/logo.png" align="right" height="140" /></a>

<!-- badges: start -->
[![CRAN status](https://badges.cranchecks.info/flavor/release/cuteDev.svg)](https://cran.r-project.org/web/checks/check_results_cuteDev.html)
[![R-CMD-check](https://github.com/Rdatatable/cuteDev/workflows/R-CMD-check/badge.svg)](https://github.com/Rdatatable/cuteDev/actions)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/kayjdh5qtgymhoxr/branch/master?svg=true)](https://ci.appveyor.com/project/Rdatatable/data-table)
[![Codecov test coverage](https://codecov.io/github/Rdatatable/cuteDev/coverage.svg?branch=master)](https://app.codecov.io/github/Rdatatable/cuteDev?branch=master)
[![GitLab CI build status](https://gitlab.com/Rdatatable/cuteDev/badges/master/pipeline.svg)](https://gitlab.com/Rdatatable/cuteDev/-/pipelines)
[![downloads](https://cranlogs.r-pkg.org/badges/cuteDev)](https://www.rdocumentation.org/trends)
[![CRAN usage](https://jangorecki.gitlab.io/rdeps/cuteDev/CRAN_usage.svg?sanitize=true)](https://gitlab.com/jangorecki/rdeps)
[![BioC usage](https://jangorecki.gitlab.io/rdeps/cuteDev/BioC_usage.svg?sanitize=true)](https://gitlab.com/jangorecki/rdeps)
[![indirect usage](https://jangorecki.gitlab.io/rdeps/cuteDev/indirect_usage.svg?sanitize=true)](https://gitlab.com/jangorecki/rdeps)

[//]: # From Serizai
[//]: # [![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[//]: # [![](https://img.shields.io/badge/license-MIT-green.svg)](https://opensource.org/licenses/MIT)
[//]: # [![rworkflows](https://github.com/js2264/HiContacts/actions/workflows/rworkflows.yml/badge.svg)](https://github.com/js2264/HiContacts/actions/workflows/rworkflows.yml)
[//]: # [![Documentation](https://github.com/js2264/HiContacts/workflows/pkgdown/badge.svg)](https://js2264.github.io/HiContacts)
[//]: # [![OHCA book](https://github.com/js2264/OHCA/actions/workflows/pages/pages-build-deployment/badge.svg)](https://js2264.github.io/OHCA/)
[//]: # <a href=http://bioconductor.org/packages/release/bioc/html/HiContacts.html><img alt="Static Badge" src="https://img.shields.io/badge/Bioc_(release)-Landing_page-green?link=http%3A%2F%2Fbioconductor.org%2FcheckResults%2Fdevel%2Fbioc-LATEST%2FHiContacts%2F"></a>
[//]: # <a href=http://bioconductor.org/checkResults/release/bioc-LATEST/HiContacts/><img alt="Bioc build (release)" src="https://img.shields.io/badge/dynamic/yaml?url=https%3A%2F%2Fbioconductor.org%2FcheckResults%2Frelease%2Fbioc-LATEST%2FHiContacts%2Fraw-results%2Fnebbiolo1%2Fbuildsrc-summary.dcf&query=%24.Status&label=Bioc%20build%20(release)&link=https%3A%2F%2Fbioconductor.org%2FcheckResults%2Frelease%2Fbioc-LATEST%2FHiContacts%2F"></a>
[//]: # <a href=http://bioconductor.org/checkResults/devel/bioc-LATEST/HiContacts/><img alt="Bioc build (devel)" src="https://img.shields.io/badge/dynamic/yaml?url=https%3A%2F%2Fbioconductor.org%2FcheckResults%2Fdevel%2Fbioc-LATEST%2FHiContacts%2Fraw-results%2Fnebbiolo2%2Fbuildsrc-summary.dcf&query=%24.Status&label=Bioc%20build%20(devel)&link=https%3A%2F%2Fbioconductor.org%2FcheckResults%2Fdevel%2Fbioc-LATEST%2FHiContacts%2F"></a>
[//]: # End From Serizai

<!-- badges: end -->


## Table of content

   - [Description](#description)
   - [Content](#content)
   - [Versions](#versions)
   - [Installation](#installation)
   - [Licence](#licence)
   - [Citations](#citations)
   - [Credits](#credits)
   - [Acknowledgements](#acknowledgements)


## Description

Set of R functions for the development of R functions, written according to the [cute_project](https://github.com/gael-millot/cute_project) specifications.


## Content

| Function | Description |
| --- | --- |
| **arg_check()** | Check expected values of arguments of functions: class, type, mode, length, restricted values panel, kind of numeric values in addition to the distinction between 'integer' and 'double' (proportion only? Inf values authorized? negative values authorized? Integers of type 'double'?). |
| **arg_test()** | Test a set of values for a defined set of arguments of a function. Contrary to `testthat::test_that()`, the function test a batch of values and control only if an error is returned or not. |
| **env_check()** | Verify that object names in the environment defined by the pos argument are identical or not to object names in the above environments (following R Scope). This can be used to verify that names used for objects inside a function or in the working environment do not override names of objects already present in the above R environments, following the R scope. |
| **get_message()** | Return the error, warning or simple (non error non warning) message if ever exist of an instruction written between quotes. |
| **is_function_here()** | Check if required functions are present in installed packages. This controls modifications in of function names package versions. |
| **is_package_here()** | Check if required packages are installed locally. |
| **is_python_package_here()** | Check if required python packages are installed locally. |
| **report** | Print a character string or a data object into a same log file. |

Read `vignette("cuteDev")` for more details.


## Versions

The different *cuteDev* releases are tagged [here](https://github.com/yushiHn/cuteDev/tags)


## Installation

*cuteDev* can be currently be installed from GitHub:

```r
install.packages("remotes")
remotes::install_github("https://github.com/yushiHn/cuteDev")
```

Older versions can be installed like this:

```r
v <- "v1.0" # desired tag version
remotes::install_github(paste0("https://github.com/yushiHn/cuteDev/tree/", v))
```


## Licence

This package can be redistributed and/or modified under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
Distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchandability or fitness for a particular purpose.
See the GNU General Public License for more details at https://www.gnu.org/licenses.


## Citation

If you are using functions of *cuteDev*, please cite: 

> Han Y, Serizay J, Millot GA (2023). _The R cuteDev package_.
> <https://github.com/yushiHn/cuteDev/>.


## Credits

[Yushi Han](https://github.com/yushiHn/), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

[Jacques Serizai](https://github.com/js2264), Spatial Regulation of Genomes team, Institut Pasteur, Paris, France

[Gael A. Millot](https://gitlab.pasteur.fr/gmillot), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France


## Acknowledgements

The developers & maintainers of [R](https://www.r-project.org/) as well as packages used in the *cuteDev* functions.

