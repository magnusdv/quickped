
<!-- README.md is generated from README.Rmd. Please edit that file -->

# QuickPed

<!-- badges: start -->

<!-- badges: end -->

To use QuickPed, visit <https://magnusdv.shinyapps.io/quickped>.

## What is QuickPed?

QuickPed is a web application for creating and plotting pedigrees on the
fly. Once the pedigree is created, you may save the plot as an image, or
store the pedigree data in a *ped formatted* text file. Such files are
commonly required as input to software for pedigree analysis.

QuickPed is powered by the
[pedtools](https://CRAN.R-project.org/package=pedtools) package, which
imports [kinship2](https://CRAN.R-project.org/package=kinship2) for
plotting.

## What is *ped format*?

For a simple illustration, consider this pedigree:

<img src="README_files/figure-gfm/trio-ped-1.png" width="25%" style="display: block; margin: auto;" />

A text file describing this pedigree may contain the following.

``` 
 id fid mid sex aff
  1   0   0   1   1
  2   0   0   2   1
  3   1   2   2   2
```

The columns are:

  - `id`: Individual ID
  - `fid`: Father’s ID (or 0 if not included in the pedigree)
  - `mid`: Mother’s ID (or 0 if not inlcuded in the pedigree)
  - `sex`: Sex (1 = male; 2 = female; 0 = unknown)
  - `aff`: Affection status (1 = unaffected; 2 = affected; 0 = unknown)

The ped format is not completely standardised, and different software
may use slightly different versions. For example, a first column with
*Family ID* is sometimes required, while the `aff` column may not be
needed in non-medical applications. These and other details may be
specified when using QuickPed.
