.timestamp_to_date <-
    function(x) {
    as.Date(as.POSIXct(as.numeric(x)/1000, origin = "1970-01-01"))
    }

.is_scalar <-
    function(x) {
    length(x) == 1L && !is.na(x)
    }

.is_scalar_character <-
    function(x) {
    .is_scalar(x) && is.character(x) && nzchar(x)
    }

.is_scalar_logical <-
    function(x) {
    .is_scalar(x) && is.logical(x)
    }

.is_uuid <-
    function(x) {
    .is_scalar_character(x) && identical(nchar(x), 32L)
    }

.is_size <-
    function(x) {
    .is_scalar(x) && x > 0L && x <= 10000L
    }

.to_numeric <-
    function(x) {
    as.numeric(ifelse(is.na(x), "",x))
    }

.concat_values <- 
    function(x) {
    paste(stats::na.omit(unique(x)), collapse = "/")
    }

#' @importFrom dplyr if_else
.update_preferred_term <- 
    function(data_value, preferred_term) {
    if_else(data_value %in% c("", NA), preferred_term, data_value)
    }

#' @importFrom stringr str_detect
.keep_numeric_values <- 
    function(x) {
    
        if (str_detect(x, "^[0-9.]+$")) {
            return(x)
        } else {
            return("")
        }
    
    }

.title_to_organ <-
    function(title) {
    
    organ_pattern <- "(?<=data from the )[a-zA-Z\\s\\(\\)]+(?= of a)"
    organ <- regmatches(title, regexpr(organ_pattern, title, perl = TRUE))
    organ <- gsub("(^|\\s)([a-z])", "\\1\\U\\2", organ, perl = TRUE)
    
    # Ensure the location letter
    organ <- gsub("\\(left\\)", "(Left)", organ, perl = TRUE)
    organ <- gsub("\\(right\\)", "(Right)", organ, perl = TRUE)
    
    organ
    
    }

.list_to_option <-
    function(path, fields) {
    
    columns <- stats::setNames(paste0(path, ".", fields), fields)
    
    paste0("[{", 
            paste0(names(columns), ": ", columns, collapse = ", "),
            "}]")
    
    }

# unify the nested list structure by ignoring outer list name
.unify_metadata <- 
    function(list) {
    dplyr::bind_rows(list)
    }

.rename_columns <- 
    function(cols) {
    
    suffixes <- c(".data_value", "_donor_data.preferred_term", 
                    "_donor_data.grouping_concept_preferred_term", ".data_type")
    new_names <- c("data_value", "preferred_term", 
                    "grouping_concept_preferred_term", "data_type")
    
    vapply(cols, function(col) {
        for (i in seq_along(suffixes)) {
            if (grepl(suffixes[i], col)) {
                return(new_names[i])
            }
        }
        col
    }, FUN.VALUE = character(1))
    
    }

#' @importFrom dplyr mutate select case_when summarise group_by
#' @importFrom tidyr pivot_wider everything any_of
#' @importFrom rlang .data
.donor_matadata_modify <- 
    function(tbl) {
    
    tbl |>
        mutate(data_value = ifelse(.data$data_type == "Numeric",
                                    .data$data_value, ""),
                data_value = vapply(.data$data_value, .to_numeric, numeric(1)),
                preferred_term = ifelse(is.na(.data$data_value), 
                                    .data$preferred_term, 
                                    .data$data_value))  |>
        pivot_wider(
            names_from = "grouping_concept_preferred_term", 
            values_from = "preferred_term",
            values_fn = list("preferred_term" = function(x) 
                                        paste(unique(x), collapse = "/"))) |>
        group_by(.data$hubmap_id) |>
        select(any_of(c(.default_columns("Donor", "character"), 
                        "Body mass index"))) |>
        summarise(across(everything(), .concat_values), .groups = 'drop') |>
        mutate(Age = as.numeric(.data$Age),
                `Body Mass Index` = as.numeric(.data$`Body Mass Index`),
                `Body mass index` = as.numeric(.data$`Body mass index`),
                `Body Mass Index` = case_when(
                    !is.na(.data$`Body Mass Index`) ~ .data$`Body Mass Index`, 
                    is.na(.data$`Body Mass Index`) & 
                    is.na(.data$`Body mass index`) ~ NA_real_, 
                    TRUE ~ .data$`Body mass index`)) |>
                select(-"Body mass index")
    
    }

#' @importFrom dplyr mutate relocate across ends_with
#' @importFrom tidyr unnest everything
#' @importFrom purrr map_chr map
.unnest_mutate_relocate <-
    function(tbl) {
    
    tbl |> 
        mutate(across(ends_with("timestamp"), .timestamp_to_date),
                across(everything(), ~ 
                map_chr(.x, ~ paste(.x, collapse = ", ")))) |>
        unnest(everything()) |>
        relocate("uuid","hubmap_id",everything())
    
    }

#' @importFrom dplyr mutate case_when rename
#' @importFrom rlang .data
.dataset_processing_category <-
    function(tbl) {
    
    tbl |> 
    rename("registered_by" = "created_by_user_displayname",
            "dataset_processing_category" = "creation_action") |>
    mutate(dataset_processing_category = case_when(
        .data$dataset_processing_category == "Create Dataset Activity" ~ 
            "Raw",
        .data$dataset_processing_category == "Central Process" ~ 
            "HuBMAP Process",
        .data$dataset_processing_category == "Lab Process" ~ 
            "Lab Process",
        TRUE ~ dataset_processing_category))
    
    }

## .onLoad

.onLoad <-
    function(libname, pkgname) {
    
        SEARCH <- Sys.getenv("BIOCONDUCTOR_HUBMAP_SEARCH_URL", SEARCH)
        ONTOLOGY <- Sys.getenv("BIOCONDUCTOR_HUBMAP_ONTOLOGY_URL", ONTOLOGY)
        ENTITY <- Sys.getenv("BIOCONDUCTOR_HUBMAP_ENTITY_URL", ENTITY)
    
    }