## HuBMAPR

'HuBMAP' data portal (<https://portal.hubmapconsortium.org/>) provides
an open, global bio-molecular atlas of the human body at the cellular
level. `HuBMAPR` package provides an alternative interface to access the
data exploration and file retrieval via R.

#### Installation

`HuBMAPR` is a R package available in *Bioconductor* version 3.19 and
later. You can install `HuBMAPR` by using the following commands in R
session from *Bioconductor*:

``` r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("HuBMAPR")

## Check Bioconductor installation
BiocManager::valid()
```

Additionally, you can install development version from
[GitHub](https://christinehou11.github.io/HuBMAPR):

``` r
BiocManager::install("christinehou11/HuBMAPR")
```

#### Use

View the article [Explore Human BioMelecular Atlas Program Data
Portal](https://christinehou11.github.io/HuBMAPR/articles/hubmapr_vignettes.html).

### Preprint

Pre-print: https://www.biorxiv.org/content/10.1101/2024.09.26.615227v1
