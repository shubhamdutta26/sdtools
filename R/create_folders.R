#' Create Standard Project Folders
#'
#' This function creates a standard set of project folders at the specified path.
#' The folders created are: 'data', 'scripts', 'output', 'docs', 'figures',
#' 'raw_data', and 'processed_data'.
#'
#' @param base_path A character string specifying the base path where folders should be created.
#'  Defaults to the current working directory.
#' @param overwrite Logical value indicating whether to overwrite existing folders.
#'  Defaults to FALSE.
#' @param recursive Logical value indicating whether to create parent directories if they don't exist.
#'  Defaults to FALSE.
#'
#' @return A named logical vector indicating success (TRUE) or failure (FALSE) for each folder creation attempt
#' @examples
#' \dontrun{
#' # Create standard numbered folders in current directory
#' create_project_structure()
#'
#' # Create folders in specific path with options
#' create_project_structure(
#'   base_path = "my_project",
#'   overwrite = TRUE,
#'   recursive = TRUE
#' )
#' }
#'
#' @export
#' @importFrom utils str
create_folders <- function(
    base_path = getwd(),
    overwrite = FALSE,
    recursive = FALSE) {

  # Define standard folder structure
  folder_names <- c(
    "01-sequences",
    "02-raw-data",
    "03-data",
    "04-scripts",
    "05-reports",
    "06-manuscripts",
    "07-presentations",
    "08-images",
    "09-protocols",
    "10-others"
  )

  # Input validation
  if (!is.character(base_path) || length(base_path) != 1) {
    stop("`base_path` must be a single character string.")
  }

  # Initialize results vector
  results <- logical(length(folder_names))
  names(results) <- folder_names

  # Create each folder
  for (i in seq_along(folder_names)) {
    folder_path <- file.path(base_path, folder_names[i])

    # Check if folder exists
    if (dir.exists(folder_path)) {
      if (overwrite) {
        # Try to remove existing directory
        unlink(folder_path, recursive = TRUE)
      } else {
        warning(sprintf("Folder '%s' already exists and overwrite=FALSE", folder_names[i]))
        results[i] <- FALSE
        next
      }
    }

    # Try to create the directory
    try_result <- try({
      dir.create(folder_path, recursive = recursive)
    }, silent = TRUE)

    # Check if creation was successful
    if (!inherits(try_result, "try-error") && dir.exists(folder_path)) {
      results[i] <- TRUE
      message(sprintf("Successfully created folder: %s", folder_names[i]))
    } else {
      results[i] <- FALSE
      warning(sprintf("Failed to create folder: %s", folder_names[i]))
    }
  }

  # Return results
  return(invisible(results))
}
