#' @rdname datasets
#' 
#' @name datasets
#' 
#' @importFrom tidyr unnest_longer
#' @importFrom dplyr mutate across
#' @importFrom tidyselect all_of ends_with
#'
#' @title HuBMAP Datasets
#' 
#' @description `datasets` returns the details available datasets
#'
#'     
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
#' @examples
#' datasets()
#' datasets() |>
#'     dplyr::glimpse()
#' datasets() |>
#'     dplyr::count(dataset_type, sort = TRUE)
datasets <-
  function()
  {
    
    fields = datasets_default_columns(as = "character")
    fields_query = paste(fields, collapse = '", "')
    
    query_string <- paste0('{
      "size": 3000,
      "query": {
         "match": { "entity_type.keyword": "Dataset" }
      },
      "fields": ["', fields_query, '"],
      "_source": false
    }')
    
    .query(query_string, option = "hits.hits[].fields") |>
      unnest_longer(all_of(fields)) |>
      mutate(across(ends_with("timestamp"), .timestamp_to_date))
    
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
#' @return `*_columns()` returns a named list with column `name`
#'     containing the column name used in the tibble returned by
#'     `samples()`, `datasets()`,  `donors()`, `collections()`, 
#'     or `publications()`. When `as = "tibble"`, the return value is a tibble 
#'     with paths as elements and abbreviations as names.
#'
#' @examples
#' datasets_default_columns()
#'
#' @export
datasets_default_columns <-
  function(as = c("character", "tibble"))
  {
    .default_columns("dataset", as)
  }

#' @rdname datasets
#'
#' @name datasets_detail
#'
#' @description `datasets_detail()` takes a unique dataset_id and 
#' returns details about one specified dataset as a tibble
#' 
#' @param uuid character(1) corresponding to the HuBMAP UUID
#'     string. This is expected to be a 32-digit hex number.
#'
#' @export
#' 
#' @examples
#' uuid <-
#'     datasets() |>
#'     dplyr::slice(1) |>
#'     dplyr::pull("uuid")
#'     
#' datasets_detail(uuid)
datasets_detail <-
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
