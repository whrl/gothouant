#' Compute per-leg temporal gait measures
#'
#' Calculates swing time, contact time, touchdown-to-touchdown cycle
#' time, and duty factor for a single leg based on liftoff and touchdown
#' indices stored in \code{meta_row}. Durations are returned in seconds,
#' and duty factor is returned as the percentage of cycle time spent in
#' contact with the substrate.
#'
#' In this implementation, contact time is defined as
#' touchdown-to-next-liftoff duration, swing time as
#' liftoff-to-next-touchdown duration, and cycle time as
#' touchdown-to-next-touchdown duration.
#'
#' @param meta_row A single-row data.frame or list containing liftoff
#'   and touchdown indices for the specified leg (e.g. \code{"L1O1"},
#'   \code{"L1D1"}, \code{"L1O2"}, \code{"L1D2"}, \code{"L1O3"},
#'   \code{"L1D3"}).
#' @param leg Character scalar naming the leg prefix used to look up
#'   indices in \code{meta_row} (e.g. \code{"L1"}, \code{"R2"}).
#' @param fps Numeric scalar, acquisition frame rate in frames per
#'   second used to convert frame counts to durations.
#'
#' @return A tibble with temporal gait measures:
#'   \itemize{
#'     \item \code{swing_time}: Named numeric vector of swing durations
#'       (in seconds) for consecutive swings (\code{s1}, \code{s2}),
#'       computed as liftoff-to-next-touchdown.
#'     \item \code{contact_time}: Named numeric vector of contact
#'       durations (in seconds) for consecutive stance phases
#'       (\code{c1}, \code{c2}), computed as
#'       touchdown-to-next-liftoff.
#'     \item \code{cycle_time}: Named numeric vector of
#'       touchdown-to-touchdown cycle durations (in seconds) for
#'       consecutive stride cycles (\code{c1}, \code{c2}).
#'     \item \code{duty_factor}: Named numeric vector of duty factors
#'       (in percent) for the same stride cycles, calculated as
#'       \code{contact_time / cycle_time * 100}.
#'   }
#'
#' @examples
#' meta_row <- data.frame(
#'   L1O1 = 1L,  L1D1 = 5L,
#'   L1O2 = 8L,  L1D2 = 12L,
#'   L1O3 = 15L, L1D3 = 20L
#' )
#'
#' per_leg_times(meta_row, leg = "L1", fps = 200)
#'
#' @export
per_leg_times <- function(meta_row, leg, fps) {
  idx <- function(suffix) unlist(meta_row[paste0(leg, suffix)])

  O1 <- idx("O1"); D1 <- idx("D1")
  O2 <- idx("O2"); D2 <- idx("D2")
  O3 <- idx("O3"); D3 <- idx("D3")

  contact_time <- c(
    c1 = (O2 - D1) / fps,
    c2 = (O3 - D2) / fps
  )

  swing_time <- c(
    s1 = (D2 - O2) / fps,
    s2 = (D3 - O3) / fps
  )

  cycle_time <- c(
    c1 = (D2 - D1) / fps,
    c2 = (D3 - D2) / fps
  )

  duty_factor <- contact_time / cycle_time * 100

  tibble::tibble(
    cycle       = 1:2,
    swing_time  = swing_time,
    contact_time = contact_time,
    cycle_time  = cycle_time,
    duty_factor = duty_factor
  )
  
}

