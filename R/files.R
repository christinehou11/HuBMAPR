ENTITY <- "https://entity.api.hubmapconsortium.org"

#' @rdname files
#'
#' @name bulk_data_transfer
#'
#' @title HuBMAP File Querying
#'
#' @importFrom httr2 resp_body_string req_cache req_url_path_append
#' @importFrom rlang has_name
#' @importFrom utils browseURL
#'
#' @description `bulk_data_transfer()` takes a unique dataset or publication
#'    UUID to be used to query the HuBMAP API for information about available
#'    files.
#'
#' @param uuid character(1) corresponding to the HuBMAP Dataset UUID
#'     string. This is expected to be a 32-digit hex number. One unique Dataset
#'     UUID corresponds to one specific URL on Globus. Files corresponding to
#'     unique Dataset UUID are saved on Globus HuBMAP Public Collection webpage.
#'
#' @return `bulk_data_transfer()` display information about how to bulk
#' download all files related to the dataset or publication.
#'
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
#' @examples
#' # Globus available
#' uuid_globus <- "2d3dbd5e5a3d9d5cfc7a46f06815a4eb"
#' uuid_sra_dbGaP <- "5ba6a5b81e95c93c26a33980f6e957d7"
#' uuid_not_available <- "0eb5e457b4855ce28531bc97147196b6"
#'
#' # bulk_data_transfer(*_globus)
#'
bulk_data_transfer <-
    function(uuid) {

    stopifnot(.is_uuid(uuid))

    resp <- .globus_url(uuid)

    if (resp$"status_code" >= 400L) { # not accessible via Globus
        uuid_detail <- dataset_detail(uuid)

        if (!has_name(uuid_detail,"dbgap_study_url")) {# no accessible dataset
        stop(
        "This dataset contains protected-access human sequence data.\n",
        "Data isn't yet available through dbGaP,\n",
        "but will be available soon.\n",
        "Please contact us via 'https://hubmapconsortium.org/contact-form/'\n",
        " with any questions regarding this data."
        )
        }
        else { # accessible dataset via dbGaP or SRA
            if (has_name(uuid_detail, "dbgap_sra_experiment_url")) {
            # accessible via SRA
            sra_url <- uuid_detail$"dbgap_sra_experiment_url"
            }
        # accessible via dbGaP
        dbGaP_url <- uuid_detail$"dbgap_study_url"
        stop(
        "This dataset contains protected-access human sequence data.\n",
        "If you are not a Consortium member,\n",
        "you must access these data through dbGaP if available.\n",
        "dbGaP authentication is required for downloading.\n",
        "View documentation on how to attain dbGaP access.\n",
        "Additional Help: 'https://hubmapconsortium.org/contact-form/' \n",
        "Navigate to the 'Bioproject' or 'Sequencing Read Archive' links.\n",
        "dbGaP URL: ", dbGaP_url, "\n\n",
        "Select the 'Run' link on the page to download the dataset.\n",
        "Additional documentation: https://www.ncbi.nlm.nih.gov/sra/docs/.\n",
        "SRA URL: ", sra_url, "."
        )
        }
    }
    # accessible via Globus
    globus_url <- resp |> resp_body_string()
    browseURL(globus_url)
    }

#' @importFrom httr2 request req_error req_perform req_cache req_url_path_append
.globus_url <-
    function(uuid) {
        request(ENTITY) |>
            req_cache(tempdir(), debug = TRUE) |>
            req_url_path_append(paste0("entities/",uuid,"/globus-url")) |>
            req_error(is_error = \(resp) FALSE) |>
            req_perform()
    }
