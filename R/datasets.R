#' @rdname datasets
#' 
#' @name datasets
#'
#' @title HuBMAP Datasets
#' 
#' @description `datasets` returns the details available datasets, ordered by
#' last modified dates
#'     
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
#' @examples
#' datasets()
datasets <-
    function() {
    
    tbl <-  .query_entity("Dataset")
    
    .dataset_edit(tbl)
    
    }


#' @rdname datasets
#'
#' @name datasets_default_columns
#' 
#' @description `*_columns()` returns a tibble or named
#'     character vector describing the content of the tibble returned
#'     by `samples()`, `datasets()`,  `donors()`, `collections()`, 
#'     or `publications()`.
#'     
#' @param as character(1) return format. One of `"tibble"` (default),
#'      or `"character"`.
#'
#' @return `*_columns()` returns a named list `name`
#'     containing the column name used in the tibble returned by
#'     `samples()`, `datasets()`,  `donors()`, 
#'     `collections()`,  or `publications()`. 
#'     When `as = "tibble"`,the return value is a tibble 
#'     with paths as elements and abbreviations as names.
#'
#' @examples
#' datasets_default_columns()
#'
#' @export
datasets_default_columns <-
    function(as = c( "tibble", "character")) {
    
    .default_columns("Dataset", as)
    
    }

#' @rdname datasets
#'
#' @name dataset_detail
#'
#' @description `dataset_detail()` takes a unique dataset_id and 
#' returns details about one specified dataset as a tibble
#' 
#' @param uuid character(1) corresponding to the HuBMAP Dataset UUID
#'     string. This is expected to be a 32-digit hex number.
#'
#' @export
#' 
#' @examples
#' uuid <- "7754aa5ebde628b5e92705e33e74a4ef"
#' dataset_detail(uuid)
dataset_detail <-
    function (uuid) {
    
    stopifnot( .is_uuid(uuid))
    
    .query_match(uuid, option = "hits.hits[]._source")
    
    }

#' @rdname datasets
#'
#' @name dataset_derived
#'
#' @description `dataset_derived()` takes a unique sample_id and 
#'     returns the derived (support) dataset details. Support datasets normally
#'     belong to Image Pyramid, with image files available to download via
#'     Globus Collection. See details to download in `files_globus_url()`.
#' 
#' @param uuid character(1) corresponding to the HuBMAP Sample UUID
#'     string. This is expected to be a 32-digit hex number.
#' 
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#' 
#' @examples
#' # no derived dataset
#' uuid <- "3acdb3ed962b2087fbe325514b098101"
#' dataset_derived(uuid)
#' 
#' # with derived dataset
#' uuid <- "2c77b1cdf33dbed3dbfb74e4b578300e"
#' dataset_derived(uuid)
dataset_derived <-
    function(uuid) {
    
    stopifnot(.is_uuid(uuid))
    
    option <- .list_to_option(
        path = "hits.hits[]._source.descendants[]",
        fields = c("uuid","hubmap_id", "data_types", 
                    "dataset_type", "status", "last_modified_timestamp"))
    
    tbl <- .query_match(uuid, option = option) |>
            .unnest_mutate_relocate()
    
    if (tbl$uuid == "") { tbl <- NULL }
    
    tbl
    
    }

#' @importFrom dplyr left_join rename select
.dataset_edit <-
    function (tbl) {
    
    tbl |>
        .unnest_mutate_relocate() |>
        left_join(organ(), by = c("origin_samples.organ" = "abbreviation")) |>
        select(-"origin_samples.organ") |>
        rename("organ" = "name",
                "analyte_class" = "metadata.metadata.analyte_class",
                "sample_category" = "source_samples.sample_category") |>
        .dataset_processing_category()
        
    }