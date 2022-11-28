DATASET <- paste0(BASE_URL, "/browse/dataset")

#' @rdname dataset
#'
#' @title Retrieve information about a single dataset
#'
#' @examples
#' uuid <-
#'     datasets() |>
#'     dplyr::slice(1) |>
#'     dplyr::pull("uuid")
#'
#' dataset(uuid)
dataset <-
    function(uuid)
{
    uri <- paste0(DATASET, "/", uuid, ".json")
    response <- httr::GET(uri)
    httr::stop_for_status(response)
    json <- httr::content(response, as = "text", encoding = "UTF-8")
}
