#' Batch-compute kinematics for all included tracking files
#'
#' Reads the metadata Excel table, keeps individuals marked for
#' inclusion, matches included rows to \code{.trk} files by individual
#' ID, runs \code{compute_kinematics()} for each matched file, and
#' collects the successfully processed results in a list.
#'
#' This wrapper is intended for project-level batch processing: it
#' combines metadata import, tracking-file discovery, per-file gait
#' analysis, and PDF figure generation as implemented in
#' \code{compute_kinematics()}.
#'
#' Files without a matching metadata row are silently skipped. Files
#' that cause an error inside \code{compute_kinematics()} are reported
#' with a warning and their entries in the returned list remain
#' \code{NULL}.
#'
#' @param meta_dir Character scalar, directory containing the metadata
#'   Excel file.
#' @param meta_file Character scalar, name of the Excel metadata file
#'   (e.g. \code{"Tabelle_Gesamt_23-4-21.xlsx"}).
#' @param trk_dir Character scalar, directory containing the
#'   \code{.trk} tracking files to process.
#' @param plotsize Numeric, half-size of the plotting window passed to
#'   \code{compute_kinematics()} (controls x/y limits of the
#'   generated PDFs).
#' @param scale_plot Either the string \code{"bodylength"} (default) or
#'   a numeric normalizing factor passed to \code{compute_kinematics()}
#'   to set the coordinate scale.
#' @param out_fig_dir Character scalar, directory where PDF outputs
#'   produced by \code{compute_kinematics()} are written.
#'
#' @return A list of length equal to the number of \code{.trk} files
#'   found in \code{trk_dir}. Elements contain the modified metadata
#'   rows returned by \code{compute_kinematics()} for successfully
#'   processed files; entries corresponding to unmatched or failed files
#'   remain \code{NULL}.
#'
#' @examples
#' \dontrun{
#' results <- batch_compute_kinematics(
#'   meta_dir    = "~/data",
#'   meta_file   = "Tabelle_Gesamt_23-4-21.xlsx",
#'   trk_dir     = "~/data/trk",
#'   plotsize    = 1.5,
#'   scale_plot  = "bodylength",
#'   out_fig_dir = "~/tmp/"
#' )
#' }
#'
#' @export
batch_compute_kinematics <- function(
  meta_dir,
  meta_file,
  trk_dir,
  plotsize    = 1.5,
  scale_plot  = "bodylength",
  out_fig_dir = "~/tmp/gothouant/"
) {
  meta_raw <- readxl::read_excel(
    file.path(meta_dir, meta_file),
    col_names = TRUE,
    skip = 1
  )

  meta_table <- dplyr::filter(meta_raw, Include == 1)

  trk_files <- list.files(
    path = trk_dir,
    pattern = "\\.trk$",
    full.names = TRUE
  )

  id_str <- paste0(
    sprintf("%02d", meta_table$`Individuum-Nr.`),
    "_",
    meta_table$Messnummer
  )

  results <- vector("list", length(trk_files))

  for (i in seq_along(trk_files)) {
    fn  <- trk_files[i]
    idx <- match(substring(basename(fn), 2, 7), id_str)

    if (!is.na(idx)) {
      meta_row <- as.data.frame(meta_table[idx, , drop = FALSE])
      message("Processing ", basename(fn))

      res_i <- try(
        compute_kinematics(
          filename   = fn,
          meta       = meta_row,
          plotsize   = plotsize,
          scale_plot = scale_plot,
          outdir     = out_fig_dir
        ),
        silent = FALSE
      )

      if (!inherits(res_i, "try-error")) {
        results[[i]] <- res_i
      } else {
        warning("Failed to process file: ", basename(fn))
      }
    }
  }

  results
}
