#' @rdname collections
#'
#' @name collections
#'
#' @title HuBMAP Collections
#'
#' @description `collections()` returns details about available collections,
#' ordered by last modified dates
#'
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
#' @examples
#' collections()
collections <-
    function() {

    tbl <- .query_entity("Collection")

    .unnest_mutate_relocate(tbl)

    }


#' @rdname collections
#'
#' @name collections_default_columns
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
#' collections_default_columns()
#'
#' @export
collections_default_columns <-
    function(as = c("tibble", "character"))
    {
        .default_columns("Collection", as)
    }

#' @rdname collections
#'
#' @name collection_contacts
#'
#' @importFrom tidyr unnest everything
#'
#' @description `collection_contacts()` takes a unique collection_id and
#' returns contacts information of one specified collection as a tibble
#'
#' @param uuid character(1) corresponding to the HuBMAP Collection UUID
#'     string. This is expected to be a 32-digit hex number.
#'
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
#' @examples
#' uuid <- "381f65e58d5e2c1d16a9cef2cc203aab"
#' collection_contacts(uuid)
collection_contacts <-
    function(uuid) {

    stopifnot(.is_uuid(uuid))

    option <- .list_to_option(
        path = "hits.hits[]._source.contacts[]",
        fields = c("name", "affiliation", "orcid_id"))

    .query_match(uuid, option) |> unnest(everything())

    }

#' @rdname collections
#'
#' @name collection_data
#'
#' @importFrom tidyr unnest everything
#' @importFrom dplyr select
#'
#' @description `collection_data()` takes a unique collection_id and
#' returns related datasets of one specified collection as a tibble
#'
#' @param uuid character(1) corresponding to the HuBMAP Collection UUID
#'     string. This is expected to be a 32-digit hex number.
#'
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
#' @examples
#' uuid <- "381f65e58d5e2c1d16a9cef2cc203aab"
#' collection_data(uuid)
collection_data <-
    function(uuid) {

    stopifnot( .is_uuid(uuid))

    option <- .list_to_option(path = "hits.hits[]._source.datasets[]",
                                fields = c("uuid", "hubmap_id", "data_types",
                                            "dataset_type", "title",
                                            "last_modified_timestamp",
                                            "status"))

    tbl <- .query_match(uuid, option) |> unnest(everything())
    tbl$organ <- .title_to_organ(tbl$title)

    .unnest_mutate_relocate(tbl) |> select(-"title")

    }

#' @rdname collections
#'
#' @name collection_contributors
#'
#' @importFrom tidyr unnest everything
#'
#' @description `collection_contributors()` takes a unique collection_id and
#' returns contributors information of one specified collection as a tibble
#'
#' @param uuid character(1) corresponding to the HuBMAP Collection UUID
#'     string. This is expected to be a 32-digit hex number.
#'
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
#' @examples
#' uuid <- "590b0485a196956284b8f3344276bc50"
#' collection_contributors(uuid)
collection_contributors <-
    function(uuid) {

    stopifnot(.is_uuid(uuid))

    option <- .list_to_option(path = "hits.hits[]._source.creators[]",
                                fields = c("name", "affiliation", "orcid_id"))

    .query_match(uuid, option) |> unnest(everything())

    }
