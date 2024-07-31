#' @rdname publications
#' 
#' @name publications
#' 
#' @title HuBMAP Samples
#'
#' @description `publications()` returns details about available samples, 
#' ordered by last modified dates. `publication_status == TRUE` represents peer 
#' reviewed publications; `publication_status == FALSE` represents preprint
#' publications.
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
#' publications(size = 3, from = 2)
publications <-
  function(size = 10000L,
           from = 0L)
  {
    
    fields = publications_default_columns(as = "character")
    
    remaining <- size
    query_size <- min(10000L, remaining) # max = 10000 at a time
    
    tbl <-.query_entity("Publication", query_size, from)
    
    .unnest_mutate_relocate(tbl)
    
  }


#' @rdname publications
#'
#' @name publications_default_columns
#' 
#' @description `*_columns()` returns a tibble or named
#'     character vector describing the content of the tibble returned
#'     by `samples()`, `datasets()`, `donors()`, `collections()`, 
#'     or `publications()`.
#'     
#' @param as character(1) return format. One of `"tibble"` (default),
#'      or `"character"`.
#'
#' @return `*_columns()` returns a named list `name`
#'     containing the column name used in the tibble returned by
#'     `samples()`, `datasets()`, `donors()`, `collections()`, or 
#'     `publications()`. When `as = "tibble"`, the return value is a tibble 
#'     with paths as elements and abbreviations as names.
#'
#' @examples
#' publications_default_columns()
#'
#' @export
publications_default_columns <-
  function(as = c("tibble", "character"))
  {
    .default_columns("Publication", as)
  }


#' @rdname publications
#'
#' @name publication_data
#' 
#' @importFrom dplyr select mutate
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @importFrom rlang .data
#'
#' @description `publication_data()` takes a unique publication_id and 
#' returns details about one specified publication.
#' 
#' @param uuid character(1) corresponding to the HuBMAP Publication UUID
#'     string. This is expected to be a 32-digit hex number.
#'     
#' @param entity character(1) selected derived entity type. 
#' One of `"Sample"`, `"Donor"` or `"Dataset"`.
#' 
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#' @examples
#' uuid <- "3c7273660cdf9ab91a7901533b2cd9a5"
#' publication_data(uuid)
#'
#' @export
#' 
publication_data <-
  function(uuid, 
           entity = c("Dataset", "Sample", "Donor")) {
    
    stopifnot(
      .is_uuid(uuid)
    )
    
    entity = match.arg(entity)
    columns <- switch(
      entity,
      Dataset = c("uuid", "hubmap_id", "data_types", "dataset_type",
                  "title", "status", "last_modified_timestamp"),
      Sample =  c("uuid", "hubmap_id", "sample_category", "last_modified_timestamp"),
      Donor = c("uuid", "hubmap_id", "metadata", "last_modified_timestamp")
    )
    
    entity_ids <- 
      .query_match(uuid, "hits.hits[]._source.ancestors[]") |>
      filter(.data$entity_type == entity) |>
      select(columns)

    entity_ids <- switch(
      entity,
      
      Dataset = entity_ids |> 
        mutate(organ = .title_to_organ(.data$title)) |> 
        select(-"title"),
      
      Sample = entity_ids,
      
      Donor = entity_ids |>
        mutate(metadata = map(.data$metadata, .unify_metadata)) |>
        unnest("metadata")  |>
        .donor_matadata_modify()
    )
    
    .unnest_mutate_relocate(entity_ids)
 
  }

#' @rdname publications
#'
#' @name publications_information
#' 
#' @importFrom dplyr select mutate left_join
#' @importFrom tidyr unnest_longer
#'
#' @description `publications_information()` takes a unique publication_id and 
#' returns details about one specified publication.
#' 
#' @param uuid character(1) corresponding to the HuBMAP Publication UUID
#'     string. This is expected to be a 32-digit hex number.
#' 
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#' @export
#' 
#' @examples
#' uuid <- "3c7273660cdf9ab91a7901533b2cd9a5"
#' publications_information(uuid)
publications_information <-
  function(uuid) 
  {
    stopifnot(
      .is_uuid(uuid)
    )
    
    option <- .list_to_option(path = "hits.hits[]._source",
                              fields = c("uuid", "hubmap_id", "title", 
                                         "description", "publication_venue", 
                                         "publication_url", "publication_date",
                                         "last_modified_timestamp"))
    
    # corresponding_authors
    contacts <- .query_match(uuid, "hits.hits[]._source.contacts[]")
    
    # organ
    origin_samples <- .query_match(uuid, "hits.hits[]._source.origin_samples[]") |>
      left_join(.organ(), by = c("organ" = "abbreviation"))
    organs <- unique(origin_samples$"name")
    
    # data_types
    ancestors <- .query_match(uuid, "hits.hits[]._source.ancestors[]") |>
      unnest_longer("data_types")
    data_types <- unique(ancestors$"data_types")
    
    tbl <- .query_match(uuid, option) |>
      .unnest_mutate_relocate() |>
      mutate(corresponding_authors = paste(paste(contacts$"name", contacts$"orcid_id"), 
                                            collapse = "\n - "),
             organ = paste(organs, collapse = "\n - "),
             data_type = paste(data_types, collapse = "\n - "))
    
    cat(
      "Title\n ",
      tbl$title, "\n",
      "Abstract\n ",
      tbl$description, "\n",
      "Manuscript\n - ",
      paste(tbl$publication_venue, tbl$publication_url, sep = ": "), "\n",
      "Corresponding Authors\n - ",
      tbl$corresponding_authors, "\n",
      "Data Types\n - ",
      tbl$data_type, "\n",
      "Organs\n - ",
      tbl$organ, "\n",
      "Publication Date\n - ",
      tbl$publication_date, "\n",
      "Last Modified\n - ",
      as.character(tbl$last_modified_timestamp), "\n",
      sep = ""
    )
    
  }

#' @rdname publications
#'
#' @name publication_authors
#'
#' @description `publication_authors()` takes a unique publication_id and 
#' returns authors information of one specified collection as a tibble
#' 
#' @param uuid character(1) corresponding to the HuBMAP Publication UUID
#'     string. This is expected to be a 32-digit hex number.
#' 
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#' 
#' @examples
#' uuid <- "3c7273660cdf9ab91a7901533b2cd9a5"
#' publication_authors(uuid)
publication_authors <-
  function(uuid) {
    stopifnot(
      .is_uuid(uuid)
    )
    
    option <- .list_to_option(path = "hits.hits[]._source.contributors[]",
                              fields = c("name", "affiliation", "orcid_id"))
    
    .query_match(uuid, option = "hits.hits[]._source.contributors[]") |>
      tidyr::unnest(tidyr::everything())
  }