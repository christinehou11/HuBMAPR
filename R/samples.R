#' @rdname samples
#' 
#' @name samples
#' 
#' @title HuBMAP Samples
#'
#' @description `samples()` returns details about available samples, ordered by
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
#' samples(size = 20, from = 5)
samples <-
    function(size = 10000L, from = 0L) {
    
    remaining <- size
    query_size <- min(10000L, remaining) # max = 10000 at a time
    
    tbl <- .query_entity("Sample", query_size, from)
    
    .sample_edit(tbl)
    
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
#'     `samples()`, `datasets()`,  `donors()`, 
#'     `collections()`,  or `publications()`. 
#'     When `as = "tibble"`,the return value is a tibble 
#'     with paths as elements and abbreviations as names.
#'
#' @examples
#' samples_default_columns()
#'
#' @export
samples_default_columns <-
    function(as = c("tibble", "character"))
    {
    
    .default_columns("Sample", as)
    
    }


#' @rdname samples
#'
#' @name sample_detail
#'
#' @description `sample_detail()` takes a unique sample_id and 
#' returns details about one specified sample as a tibble
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
#' uuid <- "d3525d35f6d5ee3dc3186613b0ab1762"
#' sample_detail(uuid)
sample_detail <-
    function (uuid) {
    
    stopifnot(.is_uuid(uuid))
    
    .query_match(uuid, option = "hits.hits[]._source")
    
    }

#' @rdname samples
#'
#' @name sample_derived
#' 
#' @importFrom dplyr select filter left_join any_of
#'
#' @description `sample_derived()` takes a unique sample_id and 
#' returns the derived dataset or/and sample details.
#' 
#' @param uuid character(1) corresponding to the HuBMAP Sample UUID
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
#' uuid <- "3e7dc14313262af577f686dcb09f5119"
#' sample_derived(uuid, "Sample")
sample_derived <-
    function(uuid, entity_type = c("Dataset", "Sample")) {
    
    stopifnot(.is_uuid(uuid))
    
    entity <- match.arg(entity_type)
    
    tbl <- .query_match(uuid, option = "hits.hits[]._source.descendants[]") |>
            filter(entity_type == entity) |>
            select(any_of(.default_columns(entity, "character"))) |>
            .unnest_mutate_relocate()
    
    if (identical(entity, "Sample")  && nrow(tbl) > 0L) {
    
        uuids <- tbl$uuid
        organ_info <- rep("", length(uuids))
        
        for (i in seq_along(uuids)) {
        
            organ_info[i] <- .query_match(uuids[i], 
                                "hits.hits[]._source.origin_samples[]") |>
                                left_join(.organ(), 
                                            by = c("organ" = "abbreviation")) |>
                                select("name")
        }
        
        tbl$organ <- organ_info
        
    }
    
    tbl
    
    }


#' @importFrom dplyr left_join rename select 
.sample_edit <-
    function (tbl) {
    
    tbl |>
        .unnest_mutate_relocate() |>
        left_join(.organ(), by = c("origin_samples.organ" = "abbreviation")) |>
        select(-"origin_samples.organ") |>
        rename("organ" = "name")
    
    }
