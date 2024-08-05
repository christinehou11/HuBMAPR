# available APIs
ONTOLOGY <- "https://ontology.api.hubmapconsortium.org"

#' @rdname organs
#'
#' @title HuBMAP Organs
#'
#' @importFrom httr2 request req_url_path_append req_url_query req_perform 
#'              resp_body_string
#' @importFrom rjsoncons j_pivot
#' @importFrom tidyr pivot_longer everything
#' 
#' @description `organ` retrieves available organ categories and abbreviations
#' 
#' @return `organ` returns the tibble with all available organs' abbreviations 
#'        and corresponding full name.
#' 
#' @export
#' 
#' @examples
#' organs <- organ() 
#' organs |>
#'     dplyr::select("name") |>
#'     dplyr::distinct()
organ <-
    function() {
    
    request(ONTOLOGY) |>
        req_url_path_append("organs/by-code") |>
        req_url_query(application_context = "HUBMAP") |>
        req_perform() |>
        resp_body_string() |>
        j_pivot(as = "tibble") |>
        pivot_longer(cols = everything(), 
                    names_to = "abbreviation", 
                    values_to = "name")
    
    }