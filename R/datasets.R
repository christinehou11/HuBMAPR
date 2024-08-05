#' @rdname datasets
#' 
#' @name datasets
#'
#' @title HuBMAP Datasets
#' 
#' @description `datasets` returns the details available datasets, ordered by
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
#' datasets(size = 100, from = 200)
datasets <-
    function(size = 10000L, from = 0L)
    {
    
    stopifnot( .is_size(size))
    
    remaining <- size
    query_size <- min(10000L, remaining) # max = 10000 at a time
    
    tbl <-  .query_entity("Dataset", query_size, from)
    
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
    function(as = c( "tibble", "character"))
    {
    
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
    function (uuid)
    {
    
    stopifnot( .is_uuid(uuid))
    
    .query_match(uuid, option = "hits.hits[]._source")
    
    }


#' @importFrom dplyr left_join rename select
.dataset_edit <-
    function (tbl) {
    
    tbl |>
        .unnest_mutate_relocate() |>
        left_join(organ(), by = c("origin_samples.organ" = "abbreviation")) |>
        select(-"origin_samples.organ") |>
        rename("organ" = "name")
    
    }