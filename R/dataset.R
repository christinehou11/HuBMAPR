DATASET <- paste0(BASE_URL, "/browse/dataset")

#' @rdname dataset
#'
#' @title Retrieve information about a single dataset
#'
#' @param uuid Character string, corresponding to the HuBMAP UUID string. This is
#' expected to be a 32-digit hex number.
#'
#' @details Additional details are provided on the HuBMAP consortium webpage,
#' https://software.docs.hubmapconsortium.org/apis
#'
#' @export
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
    stopifnot(nchar(uuid) == 32L)
    uri <- paste0(DATASET, "/", uuid, ".json")
    response <- httr::GET(uri)
    httr::stop_for_status(response)
    json <- httr::content(response, as = "text", encoding = "UTF-8")
    return(json)
}
