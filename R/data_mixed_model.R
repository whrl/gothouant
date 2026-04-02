#' Example stride-level dataset for mixed-effects modelling
#'
#' A tidy data set containing stride-level kinematic and morphological
#' variables prepared for linear mixed-effects modelling of walking speed
#' in insects. Each row corresponds to one stride cycle of a specific leg
#' for a given individual and trial, and includes both raw and standardized
#' covariates used in the manuscript examples.
#'
#' @format A tibble with 150 rows and 22 variables:
#' \describe{
#'   \item{Individuum}{Numeric individual identifier (ID) for each animal.}
#'   \item{Messnummer}{Character trial identifier (original recording ID).}
#'   \item{mass_mg}{Body mass in milligrams.}
#'   \item{ant_group}{Factor indicating ant group (\code{"ant"} or \code{"non-ant"}).}
#'   \item{Species}{Species name (character).}
#'   \item{Genus}{Genus of the specimen (character).}
#'   \item{cycle}{Stride index (1, 2, ...) for that leg within the analysed sequence.}
#'   \item{leg_num}{Integer leg index within the animal (e.g. 1–3 for front to hind leg on one side).}
#'   \item{body_length}{Body length of the individual in millimetres.}
#'   \item{swing_time}{Swing duration of the leg in seconds.}
#'   \item{contact_time}{Stance/contact duration of the leg in seconds.}
#'   \item{cycle_time}{Stride cycle duration in seconds, \code{swing_time + contact_time}.}
#'   \item{duty_factor}{Duty factor in percent, \code{(contact_time / cycle_time) * 100}.}
#'   \item{leg_stride_length}{Horizontal distance (mm) travelled by the leg tarsus over the stride.}
#'   \item{midpoint_stride_speed}{Stride-averaged forward speed of the body midpoint (mm/s).}
#'   \item{contact_inv}{Inverse of contact_time, used as a modelling alternative to raw contact time.}
#'   \item{swing_inv}{Inverse of swing_time, used as a modelling alternative to raw swing time.}
#'   \item{leg_stride_length_s}{Standardized leg stride length, \code{scale(leg_stride_length)}.}
#'   \item{contact_time_s}{Standardized contact time, \code{scale(contact_time)}.}
#'   \item{swing_time_s}{Standardized swing time, \code{scale(swing_time)}.}
#'   \item{body_length_s}{Standardized body length, \code{scale(body_length)}.}
#'   \item{midpoint_stride_speed_s}{Standardized midpoint stride speed, \code{scale(midpoint_stride_speed)}.}
#' }
#'
#' @details
#' This data set is derived from the stride- and leg-level kinematic
#' summaries included in the package and is intended for demonstrating
#' linear mixed-effects models of stride-wise speed (e.g. with
#' \pkg{lme4}). It combines raw stride timing, stride lengths, and
#' morphology with pre-computed standardized versions of the main
#' covariates and outcome, allowing users to reproduce the modelling
#' examples in the package documentation and vignettes.
#'
#' @source Internal example data for the \pkg{gothouant} package.
#'
#' @examples
#' dplyr::glimpse(data_mixed_model)
"data_mixed_model"
