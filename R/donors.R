#' @rdname donors
#' 
#' @name donors
#' 
#' @title HuBMAP Donors
#'
#' @description `donors()` returns details about available samples, ordered by
#' last modified dates
#'
#' @param size integer(1) number of maximum results to return; 
#' The default (10000) is meant to be large enough to return all results.
#'     
#' @param from integer(1) number of number of results to skip, defaulting to 0.
#' 
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
#' @examples
#' donors(size = 4)
donors <-
    function(size = 10000L, from = 0L)
    
    {
    
    remaining <- size
    query_size <- min(10000L, remaining) # max = 10000 at a time
    
    tbl <- .query_entity("Donor", query_size, from)
    
    .donor_edit(tbl)

    }


#' @rdname donors
#'
#' @name donors_default_columns
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
#' donors_default_columns()
#'
#' @export
donors_default_columns <-
    function(as = c("tibble", "character"))
    {
    
    as <- match.arg(as)
    
    .default_columns("Donor", as)
    
    }


#' @rdname donors
#'
#' @name donor_detail
#'
#' @description `donor_detail()` takes a unique donor_id and 
#' returns details about one specified sample as a tibble
#' 
#' @param uuid character(1) corresponding to the HuBMAP Donor UUID
#'     string. This is expected to be a 32-digit hex number.
#' 
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#' 
#' @examples
#' uuid <- "1dcde05aea3509b2cf89a41ceb3d700f"
#' donor_detail(uuid)
donor_detail <-
    function (uuid)
    {
    
    stopifnot(.is_uuid(uuid))
    
    .query_match(uuid, option = "hits.hits[]._source")
    
    }

#' @rdname donors
#'
#' @name donor_derived
#'
#' @importFrom dplyr select filter left_join any_of
#' 
#' @description `donor_derived()` takes a unique donor_id and 
#' returns the derived dataset or/and sample details.
#' 
#' @param uuid character(1) corresponding to the HuBMAP Donor UUID
#'     string. This is expected to be a 32-digit hex number.
#'     
#' @param entity_type character(1) selected derived entity type. 
#' One of `"Sample"` or `"Dataset"`.
#' 
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#' 
#' @examples
#' uuid <- "1dcde05aea3509b2cf89a41ceb3d700f"
#' donor_derived(uuid, "Sample")
#' 
donor_derived <-
    function(uuid, entity_type = c("Dataset", "Sample")) {
    
    stopifnot(.is_uuid(uuid))
    
    entity <- match.arg(entity_type)
    
    tbl <- .query_match(uuid, option = "hits.hits[]._source.descendants[]") |>
            filter(entity_type == entity) |>
            select(any_of(.FIELDS[[entity]]))
    
    if (identical(entity, "Sample")) {
        if (nrow(tbl) > 0L) {
        
        uuids <- tbl$uuid
        organ_info <- rep("", length(uuids))
        
        for (i in seq_along(uuids)) {
        
            organ_info[i] <- .query_match(uuids[i], 
                            "hits.hits[]._source.origin_samples[]") |>
                            left_join(organ(), 
                                by = c("organ" = "abbreviation")) |>
                            select("name")
        }
        
        tbl$organ <- organ_info
        tbl <- .unnest_mutate_relocate(tbl)
        
    }
    else {
    
        tbl <- tbl  
        
    }
    }
    else {
    
        tbl <- tbl |> 
                .unnest_mutate_relocate() |>
                .dataset_processing_category()
    
    }
    
    tbl
    
    }

## helper function
#' @importFrom dplyr coalesce mutate select rename_with rename
#' @importFrom tidyr unnest_longer everything
#' 
.donor_edit <-
    function(tbl) {
    
    if (ncol(tbl) == 8) {
        tbl  <- tbl |>
        rename_with(.rename_columns, .cols = everything())
    
    } else {
        tbl <- tbl |> mutate(
            data_value = coalesce(
                tbl$metadata.organ_donor_data.data_value,
                tbl$metadata.living_donor_data.data_value),
            preferred_term = coalesce(
                tbl$metadata.organ_donor_data.preferred_term, 
                tbl$metadata.living_donor_data.preferred_term),
            grouping_concept_preferred_term = coalesce(
                tbl$metadata.organ_donor_data.grouping_concept_preferred_term,
                tbl$metadata.living_donor_data.grouping_concept_preferred_term),
            data_type = coalesce(
                tbl$metadata.living_donor_data.data_type,
                tbl$metadata.organ_donor_data.data_type)
        )
    }
    
    tbl |>
        select("hubmap_id", "uuid", "group_name", "last_modified_timestamp",
                "data_value", "preferred_term", 
                "grouping_concept_preferred_term", "data_type") |>
        unnest_longer(c("data_value", "preferred_term", 
                "grouping_concept_preferred_term", "data_type")) |>
        .donor_matadata_modify() |>
        .unnest_mutate_relocate()
    
    }
