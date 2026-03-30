#' Compute per-leg stride measures
#'
#' Calculates stride lengths, durations, and speeds for a single leg
#' across two consecutive touchdown-to-touchdown strides based on
#' touchdown indices stored in \code{meta_row}. Results are returned as a
#' tibble with one row per stride.
#'
#' @param meta_row A single-row data.frame or list containing touchdown
#'   and liftoff indices for the specified leg (e.g. \code{"L1O1"},
#'   \code{"L1D1"}, \code{"L1O2"}, \code{"L1D2"}, \code{"L1O3"},
#'   \code{"L1D3"}).
#' @param Center A data.frame or similar with numeric vectors
#'   \code{x}, \code{y} giving center-of-mass coordinates over time.
#' @param Leg A data.frame or similar with numeric vectors \code{x},
#'   \code{y} giving leg tip coordinates over time.
#' @param leg Character scalar naming the leg prefix used to look up
#'   indices in \code{meta_row} (e.g. \code{"L1"}, \code{"R2"}).
#' @param fps Numeric scalar, acquisition frame rate in frames per
#'   second used to convert frame counts to durations.
#' @param startframe Integer scalar offset added to all indices before
#'   subsetting, default is 0.
#'
#' @return A tibble with one row per stride and the columns:
#'   \itemize{
#'     \item \code{leg}: Leg identifier.
#'     \item \code{stride}: Stride label (\code{"l1"}, \code{"l2"}).
#'     \item \code{D_start}: Starting touchdown index.
#'     \item \code{D_end}: Ending touchdown index.
#'     \item \code{midpoint_stride_length}: midpoint-based stride length.
#'     \item \code{leg_stride_length}: Leg-tip stride length.
#'     \item \code{stride_duration}: Touchdown-to-touchdown stride
#'       duration in seconds.
#'     \item \code{midpoint_stride_speed}: midpoint-based stride speed.
#'     \item \code{leg_stride_speed}: Leg-tip stride speed.
#'   }
#'
#' @examples
#' meta_row <- data.frame(L1O1 = 1L, L1D1 = 5L,
#'                        L1O2 = 6L, L1D2 = 10L,
#'                        L1O3 = 11L, L1D3 = 15L)
#' Center  <- data.frame(x = seq(0, 1, length.out = 20),
#'                       y = seq(0, 1, length.out = 20))
#' Leg     <- data.frame(x = seq(0, 2, length.out = 20),
#'                       y = seq(0, 2, length.out = 20))
#' per_leg_stride_measures(meta_row, Center, Leg, leg = "L1",
#'                         fps = 200, startframe = 0L)
#'
#' @export
per_leg_stride_measures <- function(meta_row, Center, Leg, leg, fps, startframe = 0L) {
  idx <- function(suffix) unlist(meta_row[paste0(leg, suffix)]) + startframe

  D1 <- idx("D1")
  D2 <- idx("D2")
  D3 <- idx("D3")

  tibble::tibble(
    leg = leg,
    stride = c("l1", "l2"),
    D_start = c(D1, D2),
    D_end = c(D2, D3),
    midpoint_stride_length = c(
      sqrt((Center$x[D2] - Center$x[D1])^2 + (Center$y[D2] - Center$y[D1])^2),
      sqrt((Center$x[D3] - Center$x[D2])^2 + (Center$y[D3] - Center$y[D2])^2)
    ),
    leg_stride_length = c(
      sqrt((Leg$x[D2] - Leg$x[D1])^2 + (Leg$y[D2] - Leg$y[D1])^2),
      sqrt((Leg$x[D3] - Leg$x[D2])^2 + (Leg$y[D3] - Leg$y[D2])^2)
    ),
    stride_duration = c(
      (D2 - D1) / fps,
      (D3 - D2) / fps
    )
  ) |>
    dplyr::mutate(
      midpoint_stride_speed = midpoint_stride_length / stride_duration,
      leg_stride_speed = leg_stride_length / stride_duration
    )
}
