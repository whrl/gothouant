#' Extract per-tarsus trajectory
#'
#' Computes the u/v trajectory segment for a given tarsus from frame
#' D1 to D2 or D3, depending on availability, and returns it as a list.
#'
#' @param meta_row A single-row data.frame or list containing the
#'   trajectory index columns for the specified tarsus (e.g. \code{"L1D1"},
#'   \code{"L1D2"}, \code{"L1D3"}).
#' @param Tarsus A data.frame or list with numeric vectors \code{u} and
#'   \code{v} giving tarsus coordinates over time.
#' @param tarsus Character scalar naming the tarsus prefix used to look
#'   up indices in \code{meta_row} (e.g. \code{"L1"}, \code{"R2"}).
#' @param startframe Integer scalar offset added to all indices before
#'   subsetting, default is 0.
#'
#' @return A list with components \code{u} and \code{v}, the extracted
#'   numeric vectors for the selected tarsus segment.
#'
#' @examples
#' ## Minimal example (toy data):
#' meta_row <- data.frame(L1D1 = 1L, L1D2 = 10L, L1D3 = NA_integer_)
#' Tarsus  <- data.frame(u = 1:20, v = 21:40)
#' per_tarsus_traj(meta_row, Tarsus, tarsus = "L1",
#'                  startframe = 0L)
#' @export
per_tarsus_traj <- function(meta_row, Tarsus, tarsus, startframe = 0L) {

  idx <- function(suffix) unlist(meta_row[paste0(tarsus, suffix)]) + startframe

  D1 <- idx("D1")
  D2 <- idx("D2")
  D3 <- idx("D3")

  end_idx <- if (is.na(D3)) D2 else D3

  u <- Tarsus$u[D1:end_idx]
  v <- Tarsus$v[D1:end_idx]

  list(u = u,
       v = v)
}
