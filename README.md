
# roxer

## Motivation

Set of functions that I have found useful when writing package
documentation, which supplement `roxygen` documentation (hence `roxer`).
Over time, I’ve added other groups of functions that I have
usedrepetitively while at DIDE and had no where better to put them.

-----

Broadly, functions are classified by the file they are within:

1.  `cluster.R` - tools for submitting to DIDE cluser using `didehpc`.
2.  `compendia.R` - functions for setting up minimal research compendia,
    i.e. not full R packages. For full R packages for compendia see
    [`rrrpkg`](https://github.com/ropensci/rrrpkg).
3.  `plot.R` - minor plotting help for `patchwork` and ggplot color
    generation.
4.  `system.R` - interacting with `system` calls for changing text
    throughout R projects, or opening files remotely.
5.  `utils.R` - aiding generating `roxygen` documentation.
