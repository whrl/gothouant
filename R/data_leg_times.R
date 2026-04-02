#' Leg-level stride timing data for walking hymenoptera
#'
#' A tidy data set containing leg-level stride timing measures and
#' basic morphology for individual hymenoptera recorded in
#' locomotion trials. Each row corresponds to one stride cycle of a
#' specific leg for a given individual and trial.
#'
#' @format A tibble with N rows and 14 variables:
#' \describe{
#'   \item{Individuum}{Numeric individual identifier (ID) for each animal.}
#'   \item{leg_num}{Integer leg index within the animal (e.g. 1–3 for front to hind leg).}
#'   \item{cycle}{Stride index (1, 2, ...) for that leg within the analysed sequence.}
#'   \item{swing_time}{Swing duration of the leg in seconds, i.e. time with the foot off the ground.}
#'   \item{contact_time}{Stance/contact duration of the leg in seconds, i.e. time with the foot on the ground.}
#'   \item{cycle_time}{Stride cycle duration in seconds, defined as \code{swing_time + contact_time}.}
#'   \item{duty_factor}{Duty factor in percent, defined as \code{(contact_time / cycle_time) * 100}.}
#'   \item{body_length}{Body length of the individual in millimetres.}
#'   \item{Messnummer}{Character trial identifier (original recording ID).}
#'   \item{mass_mg}{Body mass in milligrams.}
#'   \item{Genus}{Genus of the specimen (character).}
#'   \item{Species}{Species name (character).}
#'   \item{fps}{Frame rate of the recording in frames per second (character or numeric, depending on source data).}
#'   \item{ant_group}{Factor indicating whether the species is an ant (\code{"ant"}) or non-ant (\code{"non-ant"}).}
#' }
#'
#' @details
#' The data originate from video recordings of walking insects.
#' Touchdown and liftoff events of each leg were used to compute swing,
#' contact and cycle times, as well as duty factor, for consecutive stride
#' cycles per leg and individual.
#'
#' @source Internal example data for the \pkg{gothouant} package.
#'
#' @examples
#' dplyr::glimpse(data_leg_times)
"data_leg_times"
