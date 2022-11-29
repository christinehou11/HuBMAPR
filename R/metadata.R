#' @importFrom tools R_user_dir
.hubmapr_cache_path <-
    function(path = R_user_dir("HuBMAPR", "cache"))
{
    if (!dir.exists(path))
        dir.create(path, recursive = TRUE)
    path
}

#' @importFrom httr GET progress write_disk status_code stop_for_status
.hubmapr_cache_get <-
    function(
        uri, file = basename(uri), progress = FALSE, overwrite = FALSE,
        cache_path = .hubmapr_cache_path())
{
    path <- file.path(cache_path, file)
    if (overwrite || !file.exists(path)) {
        ## download to path0 and then copy / unlink to path to avoid
        ## overwriting file with failed attempt. Don't use
        ## file.rename() since this will fail when tempfile() and
        ## cache are on separate file systems.
        path0 <- tempfile()
        response <- GET(
            uri,
            if (progress) progress(),
            write_disk(path0, overwrite = overwrite)
        )
        if (status_code(response) >= 400L) {
            unlink(path0)
            stop_for_status(response)
        }
        success <- file.copy(path0, path, overwrite = TRUE)
        if (!success) {
            stop(
                "failed to copy uri from local path to cache.\n",
                "  uri: '", uri, "\n",
                "  local path: ", path0, "\n",
                "  cache path: ", path
            )
        }
        unlink(path0)
    }
    path
}

BASE_URL <- "https://portal.hubmapconsortium.org"
DATASETS <- paste0(BASE_URL, "/metadata/v0/datasets.tsv")
SAMPLES <- paste0(BASE_URL, "/metadata/v0/samples.tsv")
DONORS <- paste0(BASE_URL, "/metadata/v0/donors.tsv")

#' @importFrom readr read_tsv
#'
#' @importFrom dplyr summarize select across
.metadata <-
    function(uri, columns)
{
    local_file <- .hubmapr_cache_get(uri)
    col_names <- strsplit(readLines(local_file, 1L), "\t")[[1]]
    tsv <- read_tsv(
        local_file,
        col_names = col_names, skip = 2L,
        show_col_types = FALSE
    )

    if (identical(columns, "some")) {
        fields <-
            tsv |>
            summarize(across(.fns = ~ sum(is.na(.x)))) |>
            select(where( ~ .x <= 1)) |>
            colnames()

        tsv <-
            tsv |>
            select(fields)
    } else if (!identical(columns, "all")) {
        ## 'portal'
        tsv <-
            tsv |>
            select(columns)
    }

    tsv
}

#' @rdname metadata
#'
#' @title "HuBMAP dataset, sample, and donor metadata"
#'
#' @export
#'
#' @examples
#' datasets()
#' datasets() |>
#'     dplyr::glimpse()
#' # datasets() |>
#' #      dplyr::count(origin_sample.mapped_organ, sort = TRUE)
datasets <-
    function(columns = c("portal", "some", "all"))
{
    columns <- match.arg(columns)
    if (identical(columns, "portal")) {
        columns <- c(
            "uuid", "hubmap_id",
            group = "group_name",
            `data types` = "assay_type",
            organ = "origin_sample.mapped_organ",
            "status",
            `last modified` = "last_modified_timestamp",
            ## left-hand selector
            consortium = "mapped_consortium",
            ## required for joins
            "donor.hubmap_id"
        )
    }

    .metadata(DATASETS, columns)
}

#' @rdname metadata
#'
#' @export
#'
#' @examples
#' samples()
#' samples() |>
#'     dplyr::glimpse()
#' samples() |>
#'     dplyr::count("mapped_specimen_type", sort = TRUE)
samples <-
    function(columns = c("portal", "some", "all"))
{
    columns <- match.arg(columns)
    if (identical(columns, "portal")) {
        columns <- c(
            "uuid",
            `hubmap id` = "hubmap_id",
            group = "group_name",
            `specimen type` = "mapped_specimen_type",
            organ = "origin_sample.mapped_organ",
            `last modified` = "last_modified_timestamp",
            ## required for joins
            "donor.hubmap_id"
        )
    }

    .metadata(SAMPLES, columns)
}


#' @rdname metadata
#'
#' @export
#'
#' @examples
#' donors()
#' donors() |>
#'     dplyr::glimpse()
#'
#' donors() |>
#'     dplyr::count(race, sort = TRUE)
#'
#' ## number of non-missing values
#' donors() |>
#'     dplyr::summarize(dplyr::across(.fns = ~sum(!is.na(.x)))) |>
#'     dplyr::glimpse()
#'
#' ## mostly complete data
#' donors() |>
#'     dplyr::select(where(~ sum(!is.na(.x)) > 100L))
donors <-
    function(columns = c("portal", "all"))
{
    columns <- match.arg(columns)
    if (identical(columns, "portal")) {
        columns <- c(
            "uuid",
            `hubmap id` = "hubmap_id",
            group = "group_name",
            age = "age_value",
            bmi = "body_mass_index_value",
            "sex",
            "race",
            `last modified` = "last_modified_timestamp"
        )
    }

    .metadata(DONORS, columns)
}
