## helper function
## internal only

SEARCH <- "https://search.api.hubmapconsortium.org/v3/search"
.FIELDS <- list(
  dataset = c("hubmap_id", "uuid", "title", "group_name", "dataset_type", "status", "last_modified_timestamp"),
  sample = c("sample_category", "group_name", "hubmap_id", "uuid", "last_modified_timestamp"),
  donor = "",
  collection = c("title", "hubmap_id", "uuid", "description","doi_url","last_modified_timestamp"),
  publication = c("dataset_type", "hubmap_id", "uuid","last_modified_timestamp", "publication_date","publication_status","publication_url","publication_venue","title","uuid")
)

#' @importFrom httr2 request req_headers req_body_raw req_perform resp_body_string
#' @importFrom rjsoncons j_pivot
.query <-
  function(query_string,
           option) {
    
    stopifnot(
      is.character(query_string)
    )
    
    resp <-
      request(SEARCH) |>
      req_headers(Accept = "application/json") |>
      req_body_raw(query_string) |>
      req_perform() |>
      resp_body_string() 
    
    resp |>
      j_pivot(option, 
              as = "tibble")
  }


#'@importFrom tibble tibble
.default_columns <-
  function(view = c("dataset", "sample", "donor", "collection", "publication"), 
           as = c("character", "tibble")) 
    {
    view <- match.arg(view)
    as <- match.arg(as)
    
    option <- .FIELDS[[view]]
    if (identical(as, "tibble")) {
      tibble(option)
    } 
    else {
      option
    }
  }

