# is_python_package_here

Check if the specified python packages are installed locally.

## Usage

``` r
is_python_package_here(
  req.package,
  python.exec.path = NULL,
  python.lib.path = NULL,
  lib.path = NULL
)
```

## Arguments

- req.package:

  Character vector of package names to import.

- python.exec.path:

  Single optional character vector specifying the absolute pathways of
  the executable python file to use (associated to the packages to use).
  If NULL, the reticulate::import_from_path() function used in
  is_python_package_here() seeks for an available version of python.exe,
  and then uses python_config(python_version, required_module,
  python_versions). But might not be the correct one for the
  python.lib.path parameter specified. Thus, it is recommanded to do not
  leave NULL, notably when using computing clusters.

- python.lib.path:

  Optional character vector specifying the absolute pathways of the
  directories containing some of the listed packages in the req.package
  argument, if not in the default directories.

- lib.path:

  Absolute path of the reticulate packages, if not in the default
  folders.

## Value

An error message if at least one of the checked packages is missing in
python.lib.path, nothing otherwise.

## Details

WARNINGS

for python 3.7. Previous versions return an error "Error in
sys\$stdout\$flush() : attempt to apply non-function"

## Examples

``` r
# example of error message

# is_python_package_here(req.package = "nopackage")


# commented because this example returns an error if the python package is not installed on the computer
# (require the installation of the python serpentine package 
# from https://github.com/koszullab/serpentine

# is_python_package_here(req.package = "serpentine")


# another example of error message

# is_python_package_here(req.package = "serpentine", python.lib.path = "blablabla")
```
