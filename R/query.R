## helper function
## internal functions only

# available APIs
SEARCH <- "https://search.api.hubmapconsortium.org/v3/search"

.FIELDS <- list(
    Dataset = c("uuid", "hubmap_id", "group_name", "dataset_type", "data_types",
                "origin_samples.organ", "status", "last_modified_timestamp",
                "donor.hubmap_id", "metadata.metadata.analyte_class",
                "source_samples.sample_category",
                "created_by_user_displayname", "creation_action"),
    Sample = c("uuid", "hubmap_id", "group_name", "sample_category",
                "origin_samples.organ", "last_modified_timestamp",
                "donor.hubmap_id"),
    Donor = c("uuid", "hubmap_id", "group_name",
                "metadata.organ_donor_data.data_value",
                "metadata.living_donor_data.data_value",
                "metadata.organ_donor_data.preferred_term",
                "metadata.living_donor_data.preferred_term",
                "metadata.organ_donor_data.grouping_concept_preferred_term",
                "metadata.living_donor_data.grouping_concept_preferred_term",
                "metadata.living_donor_data.data_type",
                "metadata.organ_donor_data.data_type",
                "last_modified_timestamp"),
    Collection = c("uuid", "hubmap_id", "title", "last_modified_timestamp"),
    Publication = c("uuid", "hubmap_id", "title", "publication_venue",
                "last_modified_timestamp", "publication_date",
                "publication_status")
    )


#' @importFrom rjsoncons j_query j_patch_apply
#' @importFrom httr2 resp_body_string
.rnext_req_fun <- function(resp, req) {

    from <- j_query(req$body$data, "from", as = "R")
    body <- resp_body_string(resp)
    n_results <- j_query(body, "length(hits.hits)", as = "R")
    n_total <- j_query(body, "hits.total.value", as = "R")
    from <- from + n_results
    if (from >= n_total)
        return(NULL)

    patch <- json_template("query_patch", from = from)
    req$body$data <- j_patch_apply(req$body$data, patch)
    req

}

#' @importFrom httr2 resp_body_string
#' @importFrom rjsoncons j_pivot
.query_response_as_tibble <-
    function(resp) {

    resp |>
        resp_body_string() |>
        j_pivot("hits.hits[].fields", as = "tibble")

    }

#' @importFrom httr2 request req_headers req_body_raw req_cache
#'              req_perform_iterative
#' @importFrom dplyr bind_rows
.query_entity <-
    function(entity = c("Dataset","Sample","Donor","Publication","Collection")){

    entity <- match.arg(entity)
    fields <- paste0('"', paste(.FIELDS[[entity]], collapse = '", "'), '"')
    query_string <- json_template("query_entity",
                                    size = 10000,
                                    from = 0,
                                    entity = entity,
                                    fields = fields)

    req <- request(SEARCH) |>
        req_cache(tempdir(), debug = TRUE) |>
        req_headers(Accept = "application/json") |>
        req_body_raw(query_string)

    resps <- req_perform_iterative(req, .rnext_req_fun, progress = FALSE)
    data <- lapply(resps, .query_response_as_tibble)

    bind_rows(data)

    }

#' @importFrom httr2 request req_headers req_body_raw req_perform
#'              resp_body_string req_cache
#' @importFrom rjsoncons j_pivot
.query_match <-
    function(uuid, option) {

    query_string <- json_template("query_match", uuid = uuid)

    resp <- request(SEARCH) |>
        req_cache(tempdir(), debug = TRUE) |>
        req_headers(Accept = "application/json") |>
        req_body_raw(query_string) |>
        req_perform() |>
        resp_body_string()

    resp |>
        j_pivot(option, as = "tibble")

    }

#'@importFrom tibble tibble
.default_columns <-
    function(view = c("Dataset", "Sample", "Donor",
                        "Collection", "Publication"),
            as = c("tibble", "character")) {

    view <- match.arg(view)
    as <- match.arg(as)

    option <- .FIELDS[[view]]

    option <- switch(
        view,

        Dataset = c("uuid", "hubmap_id", "group_name",
                    "data_types", "dataset_type", "organ", "analyte_class",
                    "dataset_processing_category", "sample_category",
                    "registered_by", "status", "pipeline",
                    "last_modified_timestamp", "donor.hubmap_id"),

        Sample = c("uuid", "hubmap_id", "group_name", "sample_category",
                    "organ", "last_modified_timestamp", "donor.hubmap_id"),

        Donor = c("hubmap_id", "uuid", "group_name", "Sex", "Age",
                    "Body Mass Index", "Race", "last_modified_timestamp"),

        Collection = option,

        Publication = option
    )

    if (identical(as, "character")) {
        option
    }
    else {
        tibble("columns" = option)
    }
    }
