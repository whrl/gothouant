#' Rotate and shift trajectories into another frame
#'
#' Translates a trajectory so that a reference point lies at the origin,
#' rotates it by \code{phi}, and optionally mirrors the right side so
#' that left and right legs become comparable. Coordinates are normalized
#' (e.g. by body length) so that \code{u} and \code{v} can be interpreted
#' in relative units.
#'
#' @param phi Numeric, rotation angle in radians (0 = original x-axis).
#'   Typically derived from the body axis or net direction of progression.
#' @param rp Data frame or list with numeric components \code{x} and
#'   \code{y} used as reference point (e.g. body center).
#' @param p Data frame with numeric columns \code{x} and \code{y} to
#'   transform.
#' @param side Character, \code{"L"} or \code{"R"}; right side is mirrored.
#' @param norm_factor Numeric, normalizing factor for \code{u} and
#'   \code{v} (e.g. body length).
#'
#' @return A copy of \code{p} with added columns:
#' \describe{
#'   \item{u}{Lateral coordinate (left-positive) in normalized units.}
#'   \item{v}{Fore-aft coordinate in normalized units.}
#' }
#'
#' @export
rot_shift <- function(phi, rp, p, side, norm_factor) {

  y <- p$y - rp$y
  x <- p$x - rp$x

  s <- if (identical(side, "R")) -1 else 1

  p$u <- (y * sin(phi) + x * cos(phi)) * s / norm_factor
  p$v <- (y * cos(phi) - x * sin(phi)) / norm_factor

  p[p == 0] <- NA

  p
}
