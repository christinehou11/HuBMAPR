ENTITY <- "https://entity.api.hubmapconsortium.org"

#' @rdname files
#' 
#' @name files_globus_url
#' 
#' @title HuBMAP File Querying
#' 
#' @importFrom httr2 request req_error req_perform resp_body_string req_cache req_url_path_append
#' @importFrom rlang has_name
#' @importFrom utils browseURL
#'
#' @description `files_globus_url()` takes a unique dataset UUID or publication UUID
#' to be used to query theHuBMAP API for information about available files. 
#'
#' @param uuid character(1) corresponding to the HuBMAP Dataset UUID
#'     string. This is expected to be a 32-digit hex number. One unique Dataset
#'     UUID corresponds to one specific URL on Globus. Files corresponding to
#'     unique Dataset UUID are saved on Globus HuBMAP Public Collection webpage.
#'     
#' @details Additional details are provided on the HuBMAP consortium
#'     webpage, https://software.docs.hubmapconsortium.org/apis
#'
#' @export
#'
files_globus_url <-
  function(uuid) {
    
    stopifnot(
      .is_uuid(uuid)
    )
    
    resp <- request(ENTITY) |>
      req_cache(tempdir(), debug = TRUE) |> 
      req_url_path_append(paste0("entities/",uuid,"/globus-url")) |> 
      req_error(is_error = \(resp) FALSE) |>
      req_perform()
    
    # not accessible via Globus
    if (resp$"status_code" >= 400L) {
      
      uuid_detail <- dataset_detail(uuid)
      
      # no accessible dataset
      if (!has_name(uuid_detail,"dbgap_study_url")) {
        
        stop(
          "This dataset contains protected-access human sequence data.\n",
        "Data is not yet available through dbGaP, but will be available soon.\n", 
        "Please contact us via 'https://hubmapconsortium.org/contact-form/' \n",
        "with any questions regarding this data."
        )
        
      }
      # accessible dataset via dbGaP or SRA
      else {
        
        # accessible via SRA
        if (has_name(uuid_detail, "dbgap_sra_experiment_url")) {
          sra_url <- uuid_detail$"dbgap_sra_experiment_url"
        }
        
        # accessible via dbGaP
        dbGaP_url <- uuid_detail$"dbgap_study_url"
        
        stop(
          "This dataset contains protected-access human sequence data.\n",
          "If you are not a Consortium member, you must access these data through dbGaP if available.\n",
          "dbGaP authentication is required for downloading through these links.\n",
          "View documentation on how to attain dbGaP access.\n", 
          "For additional help, contact us via 'https://hubmapconsortium.org/contact-form/' \n",
          "with the dataset ID and information about the files you are trying to access.\n\n",
          "Navigate to the 'Bioproject' or 'Sequencing Read Archive' links to access the datasets.\n",
          "dbGaP URL: ", dbGaP_url, "\n\n",
          "Select the 'Run' link on the page to download the dataset information.\n",
          "    Here is additional documentation: https://www.ncbi.nlm.nih.gov/sra/docs/.\n",
          "SRA URL: ", sra_url, "."
        )
      }
    }
    
    
    # accessible via Globus
    globus_url <- resp |> resp_body_string()
    
    browseURL(globus_url)
    
  }