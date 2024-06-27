#' @rdname samples
#' 
#' @name samples
#' 
#' @importFrom tidyselect all_of ends_with
#' 
#' @title HuBMAP Samples
#'
#' @description `samples()` returns details about available samples
#' 
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
#' @examples
#' samples()
#' samples() |>
#'     dplyr::glimpse()
#' samples() |>
#'     dplyr::count("sample_category", sort = TRUE)
samples <-
  function()
  {
    
    fields = samples_default_columns(as = "character")
    fields_query = paste(fields, collapse = '", "')
    
    query_string <- paste0('{
      "size": 3000,
      "query": {
         "match": { "entity_type.keyword": "Sample" }
      },
      "fields": ["', fields_query, '"],
      "_source": false
    }')
    
    .query(query_string, option = "hits.hits[].fields") |>
      unnest_longer(all_of(fields)) |>
      mutate(across(ends_with("timestamp"), .timestamp_to_date))
    
  }


#' @rdname samples
#'
#' @name samples_default_columns
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
#' samples_default_columns()
#'
#' @export
samples_default_columns <-
  function(as = c("character", "tibble"))
  {
    .default_columns("sample", as)
  }


#' @rdname samples
#'
#' @name samples_detail
#'
#' @description `samples_detail()` takes a unique sample_id and 
#' returns details about one specified sample as a tibble
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
#'     samples() |>
#'     dplyr::slice(1) |>
#'     dplyr::pull("uuid")
#'     
#' samples_detail(uuid)
samples_detail <-
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