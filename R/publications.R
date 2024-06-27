#' @rdname publications
#' 
#' @name publications
#' 
#' @importFrom tidyselect all_of ends_with
#' 
#' @title HuBMAP Samples
#'
#' @description `publications()` returns details about available samples
#'     
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
#' @examples
#' publications()
publications <-
  function()
  {
    
    fields = publications_default_columns(as = "character")
    fields_query = paste(fields, collapse = '", "')
    
    query_string <- paste0('{
      "size": 3000,
      "query": {
         "match": { "entity_type.keyword": "Publication" }
      },
      "fields": ["', fields_query, '"],
      "_source": false
    }')
    
    .query(query_string, option = "hits.hits[].fields") |>
      unnest_longer(all_of(fields)) |>
      mutate(across(ends_with("timestamp"), .timestamp_to_date))
    
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
  function(as = c("character", "tibble"))
  {
    .default_columns("publication", as)
  }


#' @rdname publications
#'
#' @name publications_detail
#'
#' @description `publications_detail()` takes a unique publication_id and 
#' returns details about one specified publication as a tibble
#' 
#' @param uuid character(1) corresponding to the HuBMAP UUID
#'     string. This is expected to be a 32-digit hex number.
#' 
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#' @export
#' 
#' @examples
#' uuid <-
#'     publications() |>
#'     dplyr::slice(1) |>
#'     dplyr::pull("uuid")
#'     
#' publications_detail(uuid)
publications_detail <-
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

#' @rdname publications
#'
#' @name publication_contributors
#' 
#' @importFrom dplyr select
#'
#' @description `publication_contributors()` takes a unique collection_id and 
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
#'     publications() |>
#'     dplyr::slice(1) |>
#'     dplyr::pull("uuid")
#'     
#' publication_contributors(uuid)
publication_contributors <-
  function(uuid) {
    stopifnot(
      is.character(uuid), length(uuid) == 1L, nchar(uuid) == 32L
    )
    
    query_string <- paste0('{
    "query": {
       "match": { "uuid": "',uuid,'" }
    }
    }')
    
    .query(query_string, "hits.hits[]._source.contributors[]") |>
      select("first_name", "middle_name_or_initial", "last_name", 
             "affiliation", "orcid_id")
  }
