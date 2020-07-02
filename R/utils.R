#' Write the data to a csv file
#'
#' Write the data to a csv file.
#'
#' @param data The data.
#' @param output_dir The directory to write the file to.
output_submission_csv <- function(data, output_dir) {
  filename <- glue::glue("{output_dir}submissions.csv")
  utils::write.csv(data, filename, row.names = FALSE)
}

#' Log into Synapse
#'
#' Log into Synapse.
#'
#' @param username Your username
#' @param password Your password
#' @param python_path Path to python installation. If left blank, uses system python
#' @export
#' @return Synapse login object from
log_into_synapse <- function(username = NULL, password = NULL,
                             python_path = NULL) {
  if (!is.null(python_path)) {
    reticulate::use_python(python_path)
  } # else use system install
  synapse <- reticulate::import("synapseclient")
  syn <- synapse$login(username, password)
  return(syn)
}
