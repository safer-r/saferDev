
# saferDev <a href="https://safer-r.github.io/saferDev">[<img src="man/figures/new_saferDev.png" align="right" height="140" />](https://safer-r.github.io/saferDev/)</a>

<br />

<!-- badges: start -->

[![CRAN status](https://img.shields.io/cran/v/saferDev.svg)](https://cran.r-project.org/package=saferDev)
[![R Version](https://img.shields.io/badge/R-%3E%3D%204.6.0-blue?logo=R)](https://cran.r-project.org/)
[![Downloads](https://cranlogs.r-pkg.org/badges/saferDev)](https://www.rdocumentation.org/trends)<br />
[![Rworkflows](https://github.com/safer-r/saferDev/actions/workflows/rworkflows.yml/badge.svg)](https://github.com/safer-r/saferDev/actions/workflows/rworkflows.yml)
[![Codecov](https://codecov.io/github/safer-r/saferDev/coverage.svg?branch=master)](https://app.codecov.io/github/safer-r/saferDev?branch=master)
[![safer-R Status](https://img.shields.io/badge/Safer--R%20status-backbone%20v19.3-brightgreen)](https://github.com/safer-r/.github/blob/main/profile/backbone.R)<br />
[![](https://img.shields.io/badge/license-GPL3.0-lightgrey.svg)](https://opensource.org/license/gpl-3.0)
<!-- badges: end -->

<br />

## Table of content

   - [Description](#description)
   - [Content](#content)
   - [Versions](#versions)
   - [Installation](#installation)
   - [Docker](#docker)
   - [Licence](#licence)
   - [Citations](#citations)
   - [Credits](#credits)
   - [acknowledgments](#acknowledgments)

<br />


## Description

Set of functions for the development of R functions and pipelines, written according to the [safer-R project](https://github.com/safer-r) specifications.

<br />

## Content
<br />

Read this [webpage](https://safer-r.github.io/saferDev) for more details.

<br />

| Function | Description |
| :--- | :--- |
| **all_args_here()** | Verify that all the functions used inside a function are written with all their arguments. For instance: `base::paste0(letters[1:2], collapse = NULL, recycle0 = FALSE)` and not `base::paste0(letters[1:2])`. |
| **arg_check()** | Check expected values of arguments of functions: class, type, mode, length, restricted values panel, kind of numeric values in addition to the distinction between 'integer' and 'double' (proportion only? Inf values authorized? negative values authorized? Integers of type 'double'?). |
| **colons_check()** | Verify that all the functions used inside a function are all referenced by their package attribution. For instance: `base::mean()` and not `mean()`, or `saferDev:::.base_op_check()` and not `.base_op_check()`. |
| **env_check()** | Verify that object names in the environment defined by the `pos` argument are identical or not to object names in the above environments (following R Scope). This can be used to verify that names used for objects inside a function or in the working environment do not override names of objects already present in the above R environments, following the R scope. |
| **get_message()** | Return the error, warning or simple (non error non warning) message if ever exist of an instruction written between quotes. |
| **is_function_here()** | Check if required functions are present in installed packages. This controls for instance modifications of function names depending on package versions. |
| **is_package_here()** | Check if required packages are installed locally. |
| **report()** | Print a character string or a data object into a same log file. |

<br />

## Versions

The different *saferDev* releases are tagged [here](https://github.com/safer-r/saferDev/tags).

<br />

## Installation

```r
install.packages("saferDev")
```

Older versions can be installed like this:

```r
v <- "v1.0" # desired tag version
remotes::install_github(paste0("https://github.com/safer-r/saferDev", "/tree/", v))
```

## Docker image

SaferDev alone in R 4.6.0: [gmillot/r_v4.6.0_safer_v1.0:gitlab_v11.7](https://hub.docker.com/repository/docker/gmillot/r_v4.6.0_safer_v1.0/general)


SaferDev and 295 packages in R 4.6.0: [gmillot/r_v4.6.0_extended_v1.0:gitlab_v11.7](https://hub.docker.com/repository/docker/gmillot/r_v4.6.0_extended_v1.0/general)

<br />

## Licence

This package can be redistributed and/or modified under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
Distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchantability or fitness for a particular purpose.
See the GNU General Public License for more details at https://www.gnu.org/licenses or in the Licence.txt attached file.

<br />

## Citation

If you are using functions of *saferDev*, please cite: 

> Wang H, Han Y, Legras M, Serizay J, Millot GA (2026). _The R saferDev package_.
> <https://github.com/safer-r/saferDev/>.

<br />

## Credits

[Haiding Wang](https://github.com/Tintin2710), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

[Yushi Han](https://github.com/yushiHn), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

[Mia Legras](https://github.com/mialgrs), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

[Jacques Serizay](https://github.com/js2264), Spatial Regulation of Genomes team, Institut Pasteur, Paris, France

[Gael A. Millot](https://github.com/gael-millot), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

<br />

## acknowledgments

The developers & maintainers of the mentioned softwares and packages, including:

- [R](https://www.r-project.org/)
- [Git](https://git-scm.com/)
- [GitHub](https://github.com/)
- [rworkflows](https://github.com/neurogenomics/rworkflows)
- [tidyverse](https://ggplot2.tidyverse.org/)



