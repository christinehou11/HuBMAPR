#' @rdname collection_information
#'
#' @title Collection Information Summaries from Collection UUID
#'
#' @description `collection_information()` queries the HuBMAP collection for
#'  title, description, DOI, URL, creation date, and last modified time date.
#'
#' @param uuid character(1) corresponding to the HuBMAP Collection UUID
#'     string. This is expected to be a 32-digit hex number.
#'
#' @return `collection_information()` returns a tibble,
#'     and columns containing information about the collection. The
#'     tibble is of class `collection_information` and is printed in character.
#'
#' @examples
#' uuid <- "90399057f6ff0ff18d4c46a378f0b069"
#' collection_information(uuid)
#'
#' @export
collection_information <-
    function(uuid)
    {

    stopifnot(
    .is_uuid(uuid)
    )

    option <- .list_to_option(
        path = "hits.hits[]._source",
        fields = c("uuid", "hubmap_id", "title", "description",
                    "doi_url", "registered_doi")
    )

    tbl <- .query_match(uuid, option) |>
                .unnest_mutate_relocate()

    class(tbl) <- c("collection_information", class(tbl))

    tbl

    }

#' @rdname collection_information
#'
#' @description `print.collection_information()` organizes the returned tibble
#'     from `collection_information()` in a more legible format.
#'
#' @param x an object of class `collection_information`, the result of a
#'     call to `collection_information()`.
#'
#' @param ... additional arguments, required to conform with the
#'     `print` generic but not used.
#'
#' @return `print.collection_information()` is invoked automatically when
#'     the result of `collection_information()` is displayed for it's
#'     side effect of displaying the object.
#'
#' @export
print.collection_information <-
    function(x, ...)
    {

    cat(
        "Title\n ",
        x$title, "\n",
        "Description\n ",
        x$description, "\n",
        "DOI\n - ",
        x$doi_url, "\n",
        "URL\n - ",
        x$registered_doi, "\n"
    )

    }
