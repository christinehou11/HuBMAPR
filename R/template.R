## `json_template()` separates JSON query from R code. JSON query
## are in inst/json.
## @param name the name of the template file, without the '.sql'
##     extension, e.g., `'range_join'`.
##
## @param ... name-value paired used for template substitution
##     following the 'mustache' scheme as implemented by the {whisker}
##     package, e.g., `db_file_name = 'db file name'`.
##
#' @importFrom whisker whisker.render
json_template <-
    function(name, ...) {
    
    language_template("json", name, ...)
    
    }

language_template <- function(language, name, ...) {
    
    file <- paste0(name, ".", language)
    path <- system.file(language, file, package = "HuBMAPR")
    lines <- readLines(path, warn = FALSE)
    template <- paste(lines, collapse = "\n")
    whisker.render(template, list(...))
    
    }
