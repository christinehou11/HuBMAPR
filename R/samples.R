#' @rdname samples
#'
#' @name samples
#'
#' @title HuBMAP Samples
#'
#' @description `samples()` returns details about available samples, ordered by
#' last modified dates
#'
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
#' @examples
#' samples()
samples <-
    function() {

    tbl <- .query_entity("Sample")

    .sample_edit(tbl)

    }


#' @rdname samples
#'
#' @name samples_default_columns
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
#' samples_default_columns()
#'
#' @export
samples_default_columns <-
    function(as = c("tibble", "character")) {

    .default_columns("Sample", as)

    }


#' @rdname samples
#'
#' @name sample_detail
#'
#' @description `sample_detail()` takes a unique sample_id and
#' returns details about one specified sample as a tibble
#'
#' @param uuid character(1) corresponding to the HuBMAP Sample UUID
#'     string. This is expected to be a 32-digit hex number.
#'
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
#' @examples
#' uuid <- "d3525d35f6d5ee3dc3186613b0ab1762"
#' sample_detail(uuid)
sample_detail <-
    function (uuid) {

    stopifnot(.is_uuid(uuid))

    .query_match(uuid, option = "hits.hits[]._source")

    }

#' @rdname samples
#'
#' @name sample_derived
#'
#' @importFrom dplyr select mutate filter any_of
#' @importFrom purrr map_int map_chr
#'
#' @description `sample_derived()` takes a unique sample_id and
#' returns the derived dataset or/and sample details.
#'
#' @param uuid character(1) corresponding to the HuBMAP Sample UUID
#'     string. This is expected to be a 32-digit hex number.
#'
#' @param entity_type character(1) selected derived entity type.
#' One of `"Sample"` or `"Dataset"` (default).
#'
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
#' @examples
#' uuid <- "3e7dc14313262af577f686dcb09f5119"
#' sample_derived(uuid, "Sample")
sample_derived <-
    function(uuid, entity_type = c("Dataset", "Sample")) {

    stopifnot(.is_uuid(uuid))

    entity <- match.arg(entity_type)

    tbl <- .query_match(uuid, option = "hits.hits[]._source.descendants[]") |>
            filter(.data$entity_type == entity)

    if (identical(entity, "Sample")) {
        if (nrow(tbl) > 0L) {

        tbl <- tbl |>
            select("uuid", "hubmap_id", "sample_category",
                    "last_modified_timestamp") |>
            .unnest_mutate_relocate() |>
            mutate(organ = map_chr(uuid, ~.organ_sample_uuid(.x)))

        }
        else { tbl <- NULL }

    }
    else {

        tbl <- tbl |>
            select(any_of(c("uuid", "hubmap_id", "data_types", "dataset_type",
                        "status", "last_modified_timestamp"))) |>
            .unnest_mutate_relocate() |>
            mutate(derived_dataset_count = map_int(uuid, ~{
                            nrow(.query_match(.x,
                            option = "hits.hits[]._source.descendants[]"))}))

    }

    tbl

    }

#' @rdname samples
#'
#' @name sample_metadata
#'
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
#'
#' @description `sample_metadata()` takes a unique donor_id and
#' returns the metadata of the sample.
#'
#' @param uuid character(1) corresponding to the HuBMAP Donor UUID
#'     string. This is expected to be a 32-digit hex number.
#'
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
#' @examples
#' uuid <- "8d7ce3aab7013e416263d23b0c048900"
#' sample_metadata(uuid)
#'
sample_metadata <-
    function(uuid) {
    
    stopifnot(.is_uuid(uuid))
    
    donor_uuid <- .query_match(uuid,
                    option = "hits.hits[]._source.ancestors[]") |>
                    filter(.data$entity_type == "Donor") |>
                    pull("uuid")
    
    .donor_metadata(donor_uuid) |>
        mutate(Key = paste0("donor.", .data$Key))
    
    }

#' @importFrom dplyr left_join rename select
.sample_edit <-
    function (tbl) {

    tbl |>
        .unnest_mutate_relocate() |>
        left_join(organ(), by = c("origin_samples.organ" = "abbreviation")) |>
        select(-"origin_samples.organ") |>
        rename("organ" = "name")

    }
