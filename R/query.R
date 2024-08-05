## helper function
## internal only

# available APIs
SEARCH <- "https://search.api.hubmapconsortium.org/v3/search"

.FIELDS <- list(
    Dataset = c("uuid", "hubmap_id", "group_name", "dataset_type", "data_types",
                "origin_samples.organ", "status", "last_modified_timestamp", 
                "donor.hubmap_id"),
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


#' @importFrom httr2 request req_headers req_body_raw req_perform 
#'              resp_body_string req_cache
.query_resp <-
    function(query_string) {
    
    stopifnot(is.character(query_string))
    
    request(SEARCH) |>
        req_cache(tempdir(), debug = TRUE) |> 
        req_headers(Accept = "application/json") |>
        req_body_raw(query_string) |>
        req_perform() |>
        resp_body_string() 
    
    }


#' @importFrom rjsoncons j_pivot
.query_entity <-
    function(entity = c("Dataset","Sample","Donor","Publication","Collection"),
            size = 10000L,
            from = 0L) {
    
    entity <- match.arg(entity)
    fields <- paste0('"', paste(.FIELDS[[entity]], collapse = '", "'), '"')
    query_string <- json_template("query_entity",
                                    size = size,
                                    from = from,
                                    entity = entity,
                                    fields = fields)
    
    resp <- .query_resp(query_string)
    
    resp |>
        j_pivot("hits.hits[].fields", as = "tibble")
    
    }

#' @importFrom httr2 request req_headers req_body_raw req_perform 
#'              resp_body_string req_cache
#' @importFrom rjsoncons j_pivot
.query_match <- 
    function(uuid, option) {
    
    query_string <- json_template("query_match", uuid = uuid)
    
    resp <- .query_resp(query_string)
    
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
    
    if (view %in% c("Sample", "Dataset")) {
        option <- .FIELDS[[view]]
        option <- gsub("origin_samples.organ", "organ", option)
    } else if (view == "Donor") {
        option <- c("hubmap_id", "uuid", "group_name",
                    "last_modified_timestamp", "Sex", "Age", 
                    "Body Mass Index", "Race")
    } else {
        option <- .FIELDS[[view]]
    }
    
    if (identical(as, "character")) {
        option
    } 
    else {
        tibble("columns" = option)
    }
    }

