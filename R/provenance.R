#' @rdname provenance
#'
#' @title HuBMAP Provenance
#'
#' @name uuid_provenance
#'
#' @importFrom dplyr select pull
#' @importFrom tidyr unnest everything
#' @importFrom rlang .data
#'
#' @param uuid character(1) corresponding to the HuBMAP Dataset or Sample UUID
#'     string. This is expected to be a 32-digit hex number.
#'
#' @return `uuid_provenance()` takes a Dataset or Sample UUID,
#' and returns the provenance details as a list of characters, from the most
#' recent ancestor to the furthest ancestor. The entity type of ancestors
#' include Dataset, Sample, and Donor. Note that there is no ancestor for Donor
#' entity such that the returned result will be an empty list.
#'
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
#' @examples
#' uuid <- "acf816bed56dd6ba55da82960d937ec4"
#' uuid_provenance(uuid)
uuid_provenance <-
    function(uuid) {

    provenance <- list()

    while (TRUE) {

    ancestors <- .query_match(uuid,
                    option = "hits.hits[]._source") |>
        select("immediate_ancestor_ids") |> 
        unnest(everything())

    if(nrow(ancestors) == 0L) { break }
    else {

        ancestors <- ancestors |>
            mutate(entity_type = .uuid_category(.data$immediate_ancestor_ids))
        
        uuid <- ancestors$immediate_ancestor_ids

        provenance <- append(provenance,
                            paste(unlist(ancestors), collapse = ", "))
    }
    }

    provenance

    }
