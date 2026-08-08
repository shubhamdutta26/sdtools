#' Create a Standard R/Quarto Research Project Structure
#'
#' Creates a lightweight directory and file structure for research projects
#' using R, RStudio, and Quarto.
#'
#' The function is intended to be run after creating a new RStudio project.
#' It creates directories for raw and processed data, R scripts, Quarto reports,
#' generated output, and reference materials.
#'
#' Existing files are never overwritten.
#'
#' A conservative `.gitignore` is also created. Raw data, processed data,
#' output files, `.Rproj` files, and `renv.lock` are not ignored by default.
#'
#' @param path Character. Path to the project root. Defaults to the current
#'   working directory.
#' @param keep_empty_dirs Logical. If `TRUE`, adds `.gitkeep` files to empty
#'   directories so that they can be tracked by Git. Defaults to `TRUE`.
#'
#' @return Invisibly returns the normalized path to the project directory.
#'
#' @examples
#' \dontrun{
#' # Run from the root of a newly created RStudio project
#' setup_project()
#'
#' # Set up another project explicitly
#' setup_project("~/Projects/my-project")
#'
#' # Do not create .gitkeep files
#' setup_project(keep_empty_dirs = FALSE)
#' }
#'
#' @export
setup_project <- function(path = ".", keep_empty_dirs = TRUE) {

  # ---- Validate project directory ----------------------------------------

  if (!dir.exists(path)) {
    stop("Project directory does not exist: ", path)
  }

  project_dir <- normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )

  project_name <- basename(project_dir)

  message("Setting up project: ", project_name)
  message("Location: ", project_dir)
  message("")

  # Check for an RStudio project file
  rproj_files <- list.files(
    project_dir,
    pattern = "\\.Rproj$",
    full.names = TRUE
  )

  if (length(rproj_files) == 0) {
    warning(
      "No .Rproj file found in the project directory. ",
      "The structure will still be created."
    )
  }

  # ---- Helper function ---------------------------------------------------

  write_if_missing <- function(relative_path, contents = "") {

    file_path <- file.path(project_dir, relative_path)

    if (!file.exists(file_path)) {

      writeLines(
        text = contents,
        con = file_path
      )

      message("Created: ", relative_path)

    } else {

      message("Skipped: ", relative_path, " (already exists)")

    }

    invisible(file_path)
  }

  # ---- Create directories ------------------------------------------------

  directories <- c(
    "sequences",
    "data/raw",
    "data/processed",
    "R",
    "report",
    "output/figures",
    "output/tables",
    "references"
  )

  for (directory in directories) {

    dir_path <- file.path(project_dir, directory)

    if (!dir.exists(dir_path)) {

      dir.create(
        dir_path,
        recursive = TRUE,
        showWarnings = FALSE
      )

      message("Created directory: ", directory)

    }
  }

  # ---- Preserve empty directories in Git --------------------------------

  if (isTRUE(keep_empty_dirs)) {

    for (directory in directories) {

      gitkeep <- file.path(
        project_dir,
        directory,
        ".gitkeep"
      )

      if (!file.exists(gitkeep)) {
        file.create(gitkeep)
      }
    }
  }

  # ---- README ------------------------------------------------------------

  write_if_missing(
    "README.md",
    c(
      paste0("# ", project_name),
      "",
      "## Overview",
      "",
      "Brief description of the project.",
      "",
      "## Project structure",
      "",
      "- `sequences/` — DNA/RNA sequences",
      "- `data/raw/` — original, unmodified data",
      "- `data/processed/` — cleaned and processed data",
      "- `R/` — R scripts and reusable functions",
      "- `report/` — Quarto reports and presentations",
      "- `output/figures/` — generated figures",
      "- `output/tables/` — generated tables",
      "- `references/` — protocols, papers, manuals, and supporting material"
    )
  )

  # ---- .gitignore --------------------------------------------------------

  write_if_missing(
    ".gitignore",
    c(
      "# RStudio",
      ".Rproj.user/",
      "",
      "# R session files",
      ".Rhistory",
      ".RData",
      ".Ruserdata",
      "",
      "# Quarto",
      ".quarto/",
      "*_cache/",
      "*_files/",
      "",
      "# macOS",
      ".DS_Store",
      "",
      "# Windows",
      "Thumbs.db"
    )
  )

  # ---- Quarto configuration ---------------------------------------------

  write_if_missing(
    "_quarto.yml",
    c(
      "project:",
      "  type: default",
      "  output-dir: output",
      "  execute-dir: project",
      "",
      "execute:",
      "  warning: false",
      "  message: false"
    )
  )

  # ---- R scripts ---------------------------------------------------------

  write_if_missing(
    "R/01-import.R",
    c(
      "# Import data ---------------------------------------------------------",
      "",
      "# Load and inspect raw data here.",
      "",
      "# Example:",
      "# library(readr)",
      "#",
      "# data <- read_csv(\"data/raw/data.csv\")"
    )
  )

  write_if_missing(
    "R/02-analysis.R",
    c(
      "# Analysis ------------------------------------------------------------",
      "",
      "# Data cleaning, transformation, statistics,",
      "# and other analyses go here."
    )
  )

  write_if_missing(
    "R/03-plots.R",
    c(
      "# Plots ---------------------------------------------------------------",
      "",
      "# Create publication and presentation figures here.",
      "",
      "# Example:",
      "# library(ggplot2)"
    )
  )

  write_if_missing(
    "R/functions.R",
    c(
      "# Project functions ---------------------------------------------------",
      "",
      "# Store reusable functions for this project here."
    )
  )

  # ---- Quarto report -----------------------------------------------------

  write_if_missing(
    "report/report.qmd",
    c(
      "---",
      paste0('title: "', project_name, '"'),
      'subtitle: "Analysis Report"',
      "format:",
      "  html:",
      "    toc: true",
      "    embed-resources: true",
      "---",
      "",
      "```{r}",
      "#| label: setup",
      "#| include: false",
      "",
      'source("R/functions.R")',
      "```",
      "",
      "# Overview",
      "",
      "Brief description of the project and experimental question.",
      "",
      "# Methods",
      "",
      "",
      "# Results",
      "",
      "",
      "# Conclusions",
      ""
    )
  )

  # ---- Quarto presentation ----------------------------------------------

  write_if_missing(
    "report/presentation.qmd",
    c(
      "---",
      paste0('title: "', project_name, '"'),
      "format:",
      "  revealjs:",
      "    slide-number: true",
      "---",
      "",
      "## Overview",
      "",
      "",
      "## Experimental Design",
      "",
      "",
      "## Results",
      "",
      "",
      "## Conclusions",
      ""
    )
  )

  # ---- Finished ----------------------------------------------------------

  message("")
  message("Project setup complete.")
  message("")
  message("Project structure:")
  message("")
  message("  sequences/")
  message("  data/")
  message("    raw/")
  message("    processed/")
  message("  R/")
  message("  report/")
  message("  output/")
  message("    figures/")
  message("    tables/")
  message("  references/")
  message("")

  invisible(project_dir)
}
