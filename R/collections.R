#' @rdname collections
#' 
#' @name collections
#' 
#' @importFrom tidyselect all_of ends_with
#' 
#' @title HuBMAP Collections
#'
#' @description `collections()` returns details about available collections
#'     
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
#' @examples
#' collections()
collections <-
  function()
  {
    
    fields = collections_default_columns(as = "character")
    fields_query = paste(fields, collapse = '", "')
    
    query_string <- paste0('{
      "size": 3000,
      "query": {
         "match": { "entity_type.keyword": "Collection" }
      },
      "fields": ["', fields_query, '"],
      "_source": false
    }')
    
    .query(query_string, option = "hits.hits[].fields") |>
      unnest_longer(all_of(fields)) |>
      mutate(across(ends_with("timestamp"), .timestamp_to_date))

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
  function(as = c("character", "tibble"))
  {
    .default_columns("collection", as)
  }


#' @rdname collections
#'
#' @name collections_detail
#'
#' @description `collections_detail()` takes a unique collection_id and 
#' returns details about one specified collection as a tibble
#' 
#' @description See `collection_information()` and `collection_title()` to
#'     easily summarize a collection from its project id.
#' 
#' @param uuid character(1) corresponding to the HuBMAP UUID
#'     string. This is expected to be a 32-digit hex number.
#' 
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#' 
#' @examples
#' uuid <-
#'     collections() |>
#'     dplyr::slice(1) |>
#'     dplyr::pull("uuid")
#'     
#' collections_detail(uuid)
collections_detail <-
  function (uuid)
  {
    stopifnot(
      is.character(uuid), length(uuid) == 1L, nchar(uuid) == 32L
    )
    
    query_string <- paste0('{
    "query": {
       "match": { "uuid": "',uuid,'" }
    }
    }')
    
    .query(query_string, "hits.hits[]._source")
  }


#' @rdname collections
#'
#' @name collection_contributors
#' 
#' @importFrom dplyr select
#'
#' @description `collection_contributors()` takes a unique collection_id and 
#' returns contributors information of one specified collection as a tibble
#' 
#' @param uuid character(1) corresponding to the HuBMAP UUID
#'     string. This is expected to be a 32-digit hex number.
#' 
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#' 
#' @examples
#' uuid <-
#'     collections() |>
#'     dplyr::slice(1) |>
#'     dplyr::pull("uuid")
#'     
#' collection_contributors(uuid)
collection_contributors <-
  function(uuid) {
    stopifnot(
      is.character(uuid), length(uuid) == 1L, nchar(uuid) == 32L
    )
    
    query_string <- paste0('{
    "query": {
       "match": { "uuid": "',uuid,'" }
    }
    }')
    
    .query(query_string, "hits.hits[]._source.creators[]") |>
      select("first_name", "middle_name_or_initial", "last_name", 
             "affiliation", "orcid_id")
  }

#' @rdname collections
#'
#' @name collection_datasets
#' 
#' @importFrom dplyr select mutate
#' @importFrom tidyselect ends_with
#'
#' @description `collection_datasets()` takes a unique collection_id and 
#' returns contributors information of one specified collection as a tibble
#' 
#' @param uuid character(1) corresponding to the HuBMAP UUID
#'     string. This is expected to be a 32-digit hex number.
#' 
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#' 
#' @examples
#' uuid <-
#'     collections() |>
#'     dplyr::slice(1) |>
#'     dplyr::pull("uuid")
#'     
#' collection_datasets(uuid)
collection_datasets <-
  function(uuid) {
    stopifnot(
      is.character(uuid), length(uuid) == 1L, nchar(uuid) == 32L
    )
    
    query_string <- paste0('{
    "query": {
       "match": { "uuid": "',uuid,'" }
    }
    }')
    
    .query(query_string, "hits.hits[]._source.datasets[]") |>
      select("uuid", "hubmap_id", "dataset_type", "last_modified_timestamp",
        contact = "created_by_user_displayname", "status") |>
      mutate(across(ends_with("timestamp"), .timestamp_to_date))
  }

