## helper function
## internal only

.BASE_URL <- "https://portal.hubmapconsortium.org"

#' @importFrom tools R_user_dir
.hubmapr_cache_path <-
  function(path = R_user_dir("HuBMAPR", "cache"))
  {
    if (!dir.exists(path))
      dir.create(path, recursive = TRUE)
    path
  }

#' @importFrom httr GET progress write_disk status_code
#'     stop_for_status
.hubmapr_cache_get <-
  function(
    uri, file = basename(uri), progress = FALSE, overwrite = TRUE,
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

#' @importFrom httr GET stop_for_status content
.hubmapr_GET <- function(path) {
  response <- GET(path)
  stop_for_status(response)
  content(response, as = "text", encoding = "UTF-8")
}

.hubmapr_path <- function(path) {
  stopifnot(
    is.character(path),
    length(path) == 1L,
    startsWith(path, "/")
  )
  paste0(.BASE_URL, path)
}