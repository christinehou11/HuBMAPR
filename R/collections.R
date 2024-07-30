#' @rdname collections
#' 
#' @name collections
#' 
#' @title HuBMAP Collections
#'
#' @description `collections()` returns details about available collections, 
#' ordered by last modified dates
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
#' collections(size = 4, from = 2)
collections <-
  function(size = 10000L,
           from = 0L)
  {
    
    fields = collections_default_columns(as = "character")
    
    remaining <- size
    query_size <- min(10000L, remaining) # max = 10000 at a time
    
    tbl <- .query_entity("Collection", query_size, from) 
    
    .unnest_mutate_relocate(tbl)

  }


#' @rdname collections
#'
#' @name collections_default_columns
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
#'     `samples()`, `datasets()`,  `donors()`, `collections()`, 
#'     or `publications()`. When `as = "tibble"`, the return value is a tibble 
#'     with paths as elements and abbreviations as names.
#'
#' @examples
#' collections_default_columns()
#'
#' @export
collections_default_columns <-
  function(as = c("tibble", "character"))
  {
    .default_columns("Collection", as)
  }


#' @rdname collections
#'
#' @name collection_information
#'
#' @description `collection_information()` takes a unique collection_id and 
#' returns details about one specified collection.
#' 
#' @param uuid character(1) corresponding to the HuBMAP Collection UUID
#'     string. This is expected to be a 32-digit hex number.
#' 
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#' 
#' @examples
#' uuid <- "6a6efd0c1a2681dc7d2faab8e4ab0bca"
#' collection_information(uuid)
collection_information <-
  function(uuid) 
  {
    stopifnot(
      .is_uuid(uuid)
    )
    
    option <- .list_to_option(path = "hits.hits[]._source",
                    fields = c("uuid","hubmap_id","title", "description", 
                               "doi_url", "registered_doi",
                               "created_timestamp", "last_modified_timestamp"))
    
    tbl <- .query_match(uuid, option) |>
      .unnest_mutate_relocate()
    
    cat(
      "Title\n ",
      tbl$title, "\n",
      "Description\n ",
      tbl$description, "\n",
      "DOI\n - ",
      tbl$registered_doi, "\n",
      "URL\n - ",
      tbl$doi_url, "\n",
      "Creation Date\n - ",
      as.character(tbl$created_timestamp), "\n",
      "Last Modified\n - ",
      as.character(tbl$last_modified_timestamp), "\n",
      sep = ""
    )
    
  }


#' @rdname collections
#'
#' @name collection_contacts
#'
#' @description `collection_contacts()` takes a unique collection_id and 
#' returns contacts information of one specified collection as a tibble
#' 
#' @param uuid character(1) corresponding to the HuBMAP Collection UUID
#'     string. This is expected to be a 32-digit hex number.
#' 
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#' 
#' @examples
#' uuid <- "381f65e58d5e2c1d16a9cef2cc203aab"
#' collection_contacts(uuid)
collection_contacts <-
  function(uuid) {
    
    stopifnot(
      .is_uuid(uuid)
    )
    
    option <- .list_to_option(path = "hits.hits[]._source.contacts[]",
                              fields = c("name", "affiliation", "orcid_id"))
    
    .query_match(uuid, option) |>
      tidyr::unnest(tidyr::everything())
    
  }

#' @rdname collections
#'
#' @name collection_datasets
#'
#' @description `collection_datasets()` takes a unique collection_id and 
#' returns contributors information of one specified collection as a tibble
#' 
#' @param uuid character(1) corresponding to the HuBMAP Collection UUID
#'     string. This is expected to be a 32-digit hex number.
#' 
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#' 
#' @examples
#' uuid <- "381f65e58d5e2c1d16a9cef2cc203aab"
#' collection_datasets(uuid)
collection_datasets <-
  function(uuid) {
    stopifnot(
      .is_uuid(uuid)
    )
    
    option <- .list_to_option(path = "hits.hits[]._source.datasets[]",
                              fields = c("uuid", "hubmap_id", "data_types", 
                                         "dataset_type", 
                                         "last_modified_timestamp","title",
                                         "created_by_user_displayname", "status"))
    tbl <- .query_match(uuid, option)
    tbl$organ <- .title_to_organ(tbl$title)
      
    tbl |>
      .unnest_mutate_relocate() |>
      dplyr::select(-"title")
  }

#' @rdname collections
#'
#' @name collection_contributors
#'
#' @description `collection_contributors()` takes a unique collection_id and 
#' returns contributors information of one specified collection as a tibble
#' 
#' @param uuid character(1) corresponding to the HuBMAP Collection UUID
#'     string. This is expected to be a 32-digit hex number.
#' 
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#' 
#' @examples
#' uuid <- "590b0485a196956284b8f3344276bc50"
#' collection_contributors(uuid)
collection_contributors <-
  function(uuid) {
    stopifnot(
      .is_uuid(uuid)
    )
    
    option <- .list_to_option(path = "hits.hits[]._source.creators[]",
                              fields = c("name", "affiliation", "orcid_id"))
    
    .query_match(uuid, option) |>
      tidyr::unnest(tidyr::everything())

  }