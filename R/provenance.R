#' @rdname provenance
#'
#' @title HuBMAP Provenance
#'
#' @name uuid_provenance
#'
#' @importFrom dplyr select
#' @importFrom tidyr unnest everything
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
                    option = "hits.hits[]._source.immediate_ancestors[]")

    if(nrow(ancestors) == 0L) { break }
    else {

        ancestors <- ancestors |>
                    select("uuid", "hubmap_id", "entity_type") |>
                    unnest(everything())

        uuid <- ancestors$uuid

        provenance <- append(provenance,
                            paste(unlist(ancestors), collapse = ", "))
    }
    }

    provenance

    }
