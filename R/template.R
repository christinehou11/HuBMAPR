## `json_template()` separates JSON query from R code. JSON query
## are in inst/json.
## @param name the name of the template file, without the '.json'
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

## Internal function to retrieve and substitute templates for specific file 
## types (e.g., JSON). This function is primarily designed to work with 
## template files stored in the package's `inst/` directory. It retrieves the 
## file, substitutes placeholder values using the {whisker} package,
## and returns the rendered output as a character string.
##
## @param language A string specifying the file type or language of the 
##     template, e.g., "json".
##
## @param name A string specifying the name of the template file without its 
##     extension. For example, if the file is "range_join.json", provide 
##     `name = "range_join"`.
##
## @param ... Additional arguments passed as name-value pairs for substitution 
##     within the template following the "mustache" template scheme as 
##     implemented in the {whisker} package.
##
language_template <- function(language, name, ...) {
  
  file <- paste0(name, ".", language)
  path <- system.file(language, file, package = "HuBMAPR")
  lines <- readLines(path, warn = FALSE)
  template <- paste(lines, collapse = "\n")
  whisker.render(template, list(...))
  
}
