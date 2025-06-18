## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----'install bioc', eval=FALSE-----------------------------------------------
# if (!requireNamespace("BiocManager")) {
#     install.packages("BiocManager")
# }
# BiocManager::install("HuBMAPR")

## ----'github', eval = FALSE---------------------------------------------------
# remotes::install_github("christinehou11/HuBMAPR")

## ----'library', message=FALSE, warning=FALSE----------------------------------
library("dplyr")
library("tidyr")
library("ggplot2")
library("HuBMAPR")
library("pryr")

## ----'datasets'---------------------------------------------------------------
system.time({
    datasets_df <- datasets()
})
object_size(datasets_df)

datasets_df

## ----'plot', echo=FALSE, warning=FALSE, message=FALSE-------------------------
datasets_sub <- datasets_df |>
    select(organ, dataset_type) |>
    group_by(organ) |>
    mutate(count = n()) |>
    filter(!is.na(organ))

colorblind_palette <- c(
    "10X Multiome" = "#E69F00",
    "2D Imaging Mass Cytometry" = "#56B4E9",
    "3D Imaging Mass Cytometry" = "#009E73",
    "ATACseq" = "#F0E442",
    "Auto-fluorescence" = "#8d46e3",
    "Cell DIVE" = "#D55E00",
    "CODEX" = "#ed027c",
    "DESI" = "#abf227",
    "Histology" = "#6e5740",
    "LC-MS" = "#05ddfa",
    "Light Sheet" = "#23e84a",
    "MALDI" = "#f54242",
    "MIBI" = "#022ded",
    "MUSIC" = "#4e6e4e",
    "Publication" = "#CC79A7",
    "RNAseq" = "#999999",
    "seqFish" = "#daf56c",
    "Slide-seq" = "#d427f2",
    "Visium (no probes)" = "#02eda3",
    "WGS" = "#0d0505")

plot1 <- ggplot(datasets_sub,
                aes(y = reorder(organ, count), fill = dataset_type)) + 
    geom_histogram(stat = "count") + 
    scale_fill_manual(values = colorblind_palette) +
    labs(x = NULL, y = NULL, fill = "Assay Type") + 
    theme_minimal() +
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        legend.position = "bottom",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7), 
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)) +
    guides(fill = guide_legend(nrow = 4))

plot1

## ----'cols'-------------------------------------------------------------------
# as = "tibble" (default)
datasets_col_tbl <- datasets_default_columns(as = "tibble")
datasets_col_tbl

# as = "character"
datasets_col_char <- datasets_default_columns(as = "character")
datasets_col_char

## ----'summary cols'-----------------------------------------------------------
tbl <- bind_cols(
    dataset = datasets_default_columns(as = "character"),
    sample = c(samples_default_columns(as = "character"), rep(NA, 7)),
    donor = c(donors_default_columns(as = "character"), rep(NA, 6)),
    collection = c(collections_default_columns(as = "character"),
                    rep(NA, 10)),
    publication = c(publications_default_columns(as = "character"),
                    rep(NA, 7))
)

tbl

## ----'organs'-----------------------------------------------------------------
organs <- organ()
organs

## ----'datasets filter'--------------------------------------------------------
# Example from datasets()
datasets_df |>
    filter(organ == 'Small Intestine') |>
    count()

## ----'derived using left_join'------------------------------------------------
donors_df <- donors()
donor_sub <- donors_df |>
    filter(Sex == "Female",
            Age <= 76 & Age >= 55,
            Race == "White",
            `Body Mass Index` <= 25,
            last_modified_timestamp >= "2020-01-08" &
            last_modified_timestamp <= "2020-06-30") |>
    head(1)

# Datasets
donor_sub_dataset <- donor_sub |>
    left_join(datasets_df |>
                select(-c(group_name, last_modified_timestamp)) |>
                rename("dataset_uuid" = "uuid",
                        "dataset_hubmap_id" = "hubmap_id"),
                by = c("hubmap_id" = "donor_hubmap_id"))

donor_sub_dataset

# Samples
samples_df <- samples()
donor_sub_sample <- donor_sub |>
    left_join(samples_df |>
                select(-c(group_name, last_modified_timestamp)) |>
                rename("sample_uuid" = "uuid",
                        "sample_hubmap_id" = "hubmap_id"),
                by = c("hubmap_id" = "donor_hubmap_id"))

donor_sub_sample

## ----'*_detail()'-------------------------------------------------------------
dataset_uuid <- datasets_df |>
    filter(dataset_type == "Auto-fluorescence",
            organ == "Kidney (Right)") |>
    head(1) |>
    pull(uuid)

# Full Information
dataset_detail(dataset_uuid) |> glimpse()

# Specific Information
dataset_detail(uuid = dataset_uuid) |>
    select(title)

## ----'metadata'---------------------------------------------------------------
dataset_metadata("993bb1d6fa02e2755fd69613bb9d6e08")

sample_metadata("8ecdbdc3e2d04898e2563d666658b6a9")

donor_metadata("b2c75c96558c18c9e13ba31629f541b6")

## ----'dataset derived'--------------------------------------------------------
# no derived/support dataset
dataset_uuid_1 <- "3acdb3ed962b2087fbe325514b098101"

dataset_derived(uuid = dataset_uuid_1)

# has derived/support dataset
dataset_uuid_2 <- "baf976734dd652208d13134bc5c4594b"

dataset_derived(uuid = dataset_uuid_2) |> glimpse()

## ----'derived using sample_derived'-------------------------------------------
sample_uuid <- samples_df |>
    filter(last_modified_timestamp >= "2023-01-01" &
            last_modified_timestamp <= "2023-10-01",
            organ == "Kidney (Left)") |>
    head(1) |>
    pull(uuid)

sample_uuid

# Derived Datasets
sample_derived(uuid = sample_uuid, entity_type = "Dataset")

# Derived Samples
sample_derived(uuid = sample_uuid, entity_type = "Sample")

## ----'provenance'-------------------------------------------------------------
# dataset provenance
dataset_uuid <- "3e4c568d9ce8df9d73b8cddcf8d0fec3"
uuid_provenance(dataset_uuid)

# sample provenance
sample_uuid <- "35e16f13caab262f446836f63cf4ad42"
uuid_provenance(sample_uuid)

# donor provenance
donor_uuid <- "0abacde2443881351ff6e9930a706c83"
uuid_provenance(donor_uuid)

## ----'collection datasets'----------------------------------------------------
collections_df <- collections()
collection_uuid <- collections_df |>
    filter(last_modified_timestamp <= "2023-01-01") |>
    head(1) |>
    pull(uuid)

collection_data(collection_uuid)

## ----'publication data'-------------------------------------------------------
publications_df <- publications()
publication_uuid <- publications_df |>
    filter(publication_venue == "Nature") |>
    head(1) |>
    pull(uuid)

publication_data(publication_uuid, entity_type = "Dataset")

publication_data(publication_uuid, entity_type = "Sample")

## ----'information'------------------------------------------------------------
collection_information(uuid = collection_uuid)

publication_information(uuid = publication_uuid)

## ----'author'-----------------------------------------------------------------
# Dataset
dataset_contributors(uuid = dataset_uuid)

# Collection
collection_contacts(uuid = collection_uuid)

collection_contributors(uuid = collection_uuid)

# Publication
publication_authors(uuid = publication_uuid)

## ----'bulk data transfer successful', eval=FALSE------------------------------
# uuid_globus <- "d1dcab2df80590d8cd8770948abaf976"
# 
# bulk_data_transfer(uuid_globus)

## ----'bulk data transfer urls', eval=FALSE------------------------------------
# uuid_dbGAP_SRA <- "d926c41ac08f3c2ba5e61eec83e90b0c"
# 
# bulk_data_transfer(uuid_dbGAP_SRA)

## ----comment=NA, echo=FALSE---------------------------------------------------
result1 <- paste("Pruning cache",
    "Error in bulk_data_transfer(uuid_dbGAP_SRA) :",
    "This dataset contains protected-access human sequence data.",
    "If you are not a Consortium member,",
    "you must access these data through dbGaP if available.",
    "dbGaP authentication is required for downloading.",
    "View documentation on how to attain dbGaP access.",
    "Additional Help: 'https://hubmapconsortium.org/contact-form/'",
    "Navigate to the 'Bioproject' or 'Sequencing Read Archive' links.",
    "dbGaP URL: 
https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id=phs002267",
    "Select the 'Run' link on the page to download the dataset.",
    "Additional documentation: https://www.ncbi.nlm.nih.gov/sra/docs/.",
    "SRA URL: https://www.ncbi.nlm.nih.gov/sra/SRX13283313.)",
    sep = "\n")

cat(result1)

## ----'bulk data transfer not avail', eval=FALSE-------------------------------
# uuid_not_avail <- "0eb5e457b4855ce28531bc97147196b6"
# 
# bulk_data_transfer(uuid_not_avail)

## ----comment=NA, echo=FALSE---------------------------------------------------
result2 <- paste("Pruning cache",
    "Error in bulk_data_transfer(uuid_not_avail) :",
    "This dataset contains protected-access human sequence data.",
    "Data isn't yet available through dbGaP,",
    "but will be available soon.",
    "Please contact us via 'https://hubmapconsortium.org/contact-form/'",
    "with any questions regarding this data.",
    sep = "\n")

cat(result2)

## ----'sessionInfo', echo=FALSE----------------------------------------------------------------------------------------
## Session info
options(width = 120)
sessionInfo()

