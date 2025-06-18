#' @rdname donors
#'
#' @name donors
#'
#' @title HuBMAP Donors
#'
#' @description `donors()` returns details about available samples, ordered by
#' last modified dates
#'
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
#' @examples
#' donors()
donors <-
  function() {
    
    tbl <- .query_entity("Donor")
    
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
    
    stopifnot(.is_uuid(uuid), .uuid_category(uuid) == "Donor")
    
    .query_match(uuid, option = "hits.hits[]._source")
    
  }

#' @rdname donors
#'
#' @name donor_derived
#'
#' @importFrom dplyr select filter mutate any_of rename distinct
#' @importFrom tidyr unnest everything
#' @importFrom purrr map_chr map_int
#' @importFrom rlang .data
#'
#' @description `donor_derived()` takes a unique donor_id and
#' returns the derived dataset or/and sample details.
#'
#' @param uuid character(1) corresponding to the HuBMAP Donor UUID
#'     string. This is expected to be a 32-digit hex number.
#'
#' @param entity_type character(1) selected derived entity type.
#' One of `"Sample"` or `"Dataset"` (default).
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
    
    stopifnot(.is_uuid(uuid), .uuid_category(uuid) == "Donor")
    
    entity <- match.arg(entity_type)
    
    tbl <- .query_match(uuid, option = "hits.hits[]._source.descendants[]") |>
      unnest(everything())
    
    if (identical(entity, "Sample")) {
      tbl <- tbl |>
        filter(is.na(.data$dataset_type)) |>
        select("uuid")
      
      if (nrow(tbl) > 0L) {
        
        tbl <- tbl |>
            mutate(organ = map_chr(uuid, ~.organ_sample_uuid(.x)),
                    derived_dataset_count = map_int(uuid, ~{
                    nrow(sample_derived(.x, "Dataset"))}))|>
            distinct(uuid, .keep_all = TRUE)
        }
        else { tbl <- NULL }
    }
    else {
        tbl <- tbl |>
            filter(!is.na(.data$dataset_type))
      
      tbl <- tbl |>
          mutate(derived_dataset_count = map_int(uuid, ~{
              nrow(.query_match(.x,
                  option = "hits.hits[]._source.descendants[]"))}))
    }
    
    tbl
    
    }

#' @rdname donors
#'
#' @name donor_metadata
#'
#' @description `donor_metadata()` takes a unique donor_id and
#' returns the metadata of the donor.
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
#' uuid <- "d37df2cad4e80dc368763caefccf7140"
#' donor_metadata(uuid)
#'
donor_metadata <-
  function(uuid) {
    
    stopifnot(.is_uuid(uuid), .uuid_category(uuid) == "Donor")
    
    .donor_metadata(uuid)
    
  }

## helper function
#' @importFrom dplyr coalesce mutate select rename_with
#' @importFrom tidyr unnest_longer everything
#' @importFrom rlang .data
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
        .unnest_mutate_relocate() |>
        mutate(Age = as.numeric(.data$Age),
            `Body Mass Index` = as.numeric(.data$`Body Mass Index`))

    }
