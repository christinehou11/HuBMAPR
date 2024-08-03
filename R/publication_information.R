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
#'     and columns containing information about the project. The
#'     tibble is of class `collection_information` and is printed in character.
#'
#' @examples
#' uuid <- "90399057f6ff0ff18d4c46a378f0b069"
#' collection_information(uuid)
#'
#' @export
collection_information <- 
    function(uuid) {
    
    stopifnot(.is_uuid(uuid))
    
    option <- .list_to_option(
        path = "hits.hits[]._source",
        fields = c("uuid", "hubmap_id", "title", "description", 
                    "doi_url", "registered_doi"))
    
    tbl <- .query_match(uuid, option) |>
        .unnest_mutate_relocate()
    
    class(tbl) <- c("collection_information", class(tbl))
    
    tbl
    
    }


#' @rdname publication_information
#'
#' @title Publication Information Summaries from Publication UUID
#' 
#' @importFrom dplyr select mutate left_join
#' @importFrom tidyr unnest_longer
#'
#' @description `publication_information()` queries the HuBMAP publication for
#'  title, abstract, manuscript,corresponding authors, dataset types, and organ.
#'
#' @param uuid character(1) corresponding to the HuBMAP Publication UUID
#'     string. This is expected to be a 32-digit hex number.
#'
#' @return `publication_information()` returns a tibble,
#'     and columns containing information about the publication. The
#'     tibble is of class `publication_information` and is printed in character.
#'
#' @examples
#' uuid <- "e8338966c69e759157d6c6ad24847989"
#' publication_information(uuid)
#'
#' @export
publication_information <-
    function(uuid) {
    
    stopifnot(.is_uuid(uuid))
    
    option <- .list_to_option(path = "hits.hits[]._source",
                                fields = c("uuid", "hubmap_id", "title", 
                                            "description", "publication_venue", 
                                            "publication_url"))
    
    # corresponding_authors
    contacts <- .query_match(uuid, "hits.hits[]._source.contacts[]")
    
    # organ
    origin_samples <- .query_match(uuid, 
                                    "hits.hits[]._source.origin_samples[]") |>
                        left_join(.organ(), by = c("organ" = "abbreviation"))
    organs <- unique(origin_samples$"name")
    
    # data_types
    ancestors <- .query_match(uuid, "hits.hits[]._source.ancestors[]") |>
        unnest_longer("data_types")
    data_types <- unique(ancestors$"data_types")
    
    tbl <- .query_match(uuid, option) |>
        .unnest_mutate_relocate() |>
        mutate(corresponding_authors = paste(paste(contacts$"name", 
                                                contacts$"orcid_id"), 
                                                collapse = "\n - "),
                organ = paste(organs, collapse = "\n - "),
                data_type = paste(data_types, collapse = "\n - "))
    
    class(tbl) <- c("publication_information", class(tbl))
    
    tbl
    
    }

#' @rdname publication_information
#'
#' @description `publication_information()` organizes the returned tibble 
#'     from `publication_information()` in a more legible format.
#'
#' @param x an object of class `publication_information`, the result of a
#'     call to `publication_information()`.
#'
#' @param ... additional arguments, required to conform with the
#'     `print` generic but not used.
#'
#' @return `print.publication_information()` is invoked automatically when
#'     the result of `publication_information()` is displayed for it's
#'     side effect of displaying the object.
#'
#' @export
print.publication_information <- 
    function(x, ...) {
    
    cat(
        "Title\n ",
        x$title, "\n",
        "Abstract\n ",
        x$description, "\n",
        "Manuscript\n - ",
        paste(x$publication_venue, x$publication_url, sep = ": "), "\n",
        "Corresponding Authors\n - ",
        x$corresponding_authors, "\n",
        "Data Types\n - ",
        x$data_type, "\n",
        "Organs\n - ",
        x$organ, "\n",
        sep = ""
    )
    
    }
