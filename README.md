## HuBMAPR

'HuBMAP' data portal (<https://portal.hubmapconsortium.org/>) provides
an open, global bio-molecular atlas of the human body at the cellular
level. `HuBMAPR` package provides an alternative interface to access the
data exploration and file retrieval via R.
[![](https://img.shields.io/badge/Bioc3.21-HuBMAPR-blue.svg)](https://bioconductor.org/packages/HuBMAPR)

'HuBMAP' data portal (<https://portal.hubmapconsortium.org/>) provides an open,
global bio-molecular atlas of the human body at the cellular level. `HuBMAPR`
package provides an alternative interface to explore the data via R.

The HuBMAP Consortium offers several
[APIs](https://docs.hubmapconsortium.org/apis.html). 
To achieve the main objectives, `HuBMAPR` package specifically integrates three
APIs:  

- [Search API](https://smart-api.info/ui/7aaf02b838022d564da776b03f357158): The
**Search API** is primarily searching relevant data information and is
referenced to the 
[Elasticsearch API](https://www.elastic.co/guide/en/elasticsearch/). 

- [Entity API](https://smart-api.info/ui/0065e419668f3336a40d1f5ab89c6ba3): The
**Entity API** is specifically utilized in the `bulk_data_transfer()`
function for Globus URL retrieval

- [Ontology API](https://smart-api.info/ui/d10ff85265d8b749fbe3ad7b51d0bf0a): 
The **Ontology API** is applied in the `organ()` function to provide additional
information about the abbreviation and corresponding full name of each organ.

Each API serves a distinct purpose with unique query capabilities, tailored to
meet various needs. Utilizing the `httr2` and `rjsoncons` packages, `HuBMAPR`
effectively manages, modifies, and executes multiple requests via these APIs,
presenting responses in formats such as tibble or character. These outputs are
further modified for clarity in the final results from the `HuBMAPR` functions,
and these functions help reflect the data information of HuBMAP Data Portal as 
much as possible.

HuBMAP Data incorporates three different 
[identifiers](https://docs.hubmapconsortium.org/apis): 

- HuBMAP ID, e.g. HBM399.VCTL.353

- Universally Unique Identifier (UUID), e.g. 7036a70229eff1a51af965454dddbe7d

- Digital Object Identifiers (DOI), e.g. 10.35079/HBM399.VCTL.353.

The `HuBMAPR`
package utilizes the UUID - a 32-digit hexadecimal number - and the more
human-readable HuBMAP ID as two common identifiers in the retrieved results.
Considering precision and compatibility with software implementation and data
storage, UUID serves as the primary identifier to retrieve data across various
functions, with the UUID mapping uniquely to its corresponding HuBMAP ID. 

The
systematic nomenclature is adopted for functions in the package by appending
the entity category prefix to the concise description of the specific
functionality. Most of the functions are grouped by entity categories, thereby
simplifying the process of selecting the appropriate functions to retrieve the
desired information associated with the given UUID from the specific entity
category. The structure of these functions is heavily consistent across all
entity categories with some exceptions for collection and publication. 

### Installation

`HuBMAPR` is a R package available in *Bioconductor* version $\geq$ 3.20 and
R version $\geq$ 4.4.0. You can install `HuBMAPR` by using the following 
commands in R session from *Bioconductor*:

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
BiocManager::install_github("christinehou11/HuBMAPR")
```

### Use

$\textbf{Entity Category:}$

- Dataset

- Sample

- Donor

- Collection

- Publication

$\textbf{Available records for [Entity Category]:}$

- datasets()

- samples()

- donors()

- collections()

- publications()

$\textbf{The default columns from [Entity Category]()]:}$

- datasets_default_columns(as = c(“tibble”, “character”))

- samples _default_columns(as = c(“tibble”, “character”)

- donors _default_columns(as = c(“tibble”, “character”)

- collections _default_columns(as = c(“tibble”, “character”)

- publications _default_columns(as = c(“tibble”, “character”)

$\textbf{Single Record Information for [Entity Category] record:}$

- Dataset

  - dataset_detail(dataset_uuid)

  - dataset_derived(dataset_uuid)

  - dataset_metadata(dataset_uuid)

  - dataset_contributors(dataset_uuid)
  
- Sample

  - sample_detail(sample_uuid)
  
  - sample_derived(sample_uuid, entity_type = c("Dataset", "Sample"))
  
  - sample_metadata(sample_uuid)

- Donor

  - donor_detail(donor_uuid)

  - donor _derived(donor_uuid, entity_type = c("Dataset", "Sample"))

  - donor _metadata(donor_uuid)

- Collection

  - collection_detail(collection_uuid)

  - collection_data(collection_uuid)

  - collection_contributors(collection_uuid)

  - collection_contacts(collection_uuid)

  - collection_information(collection_uuid)

- Publication

  - publication_detail(publication_uuid)

  - publication _data(publication_uuid)

  - publication_authors(publication_uuid)

  - publication_information(publication_uuid)

$\textbf{Provenance of a dataset/sample/donor}$:

- uuid_provenance(dataset/sample/donor uuid)

$\textbf{Additional information about organ abbreviation and its full name:}$

- organ()

$\textbf{Retrieve data files from one dataset single record:}$

- bulk_data_transfer(dataset_uuid)

View the article [Explore Human BioMelecular Atlas Program Data
Portal](https://christinehou11.github.io/HuBMAPR/articles/hubmapr_vignettes.html)
to read detailed examples.


### Preprint

Pre-print: https://www.biorxiv.org/content/10.1101/2024.09.26.615227v1
