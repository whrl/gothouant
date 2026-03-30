#' Build long-format kinematic summaries from batch results
#'
#' Takes the bound metadata/kinematics list \code{res} as returned by
#' \code{batch_compute_kinematics()} and constructs long-format tables
#' for stride kinematics, per-leg temporal variables, and per-tarsus
#' trajectory summaries.
#'
#' The function unpacks per-leg stride kinematics and temporal measures
#' from list-columns, derives a compact metadata table, and then produces
#' a unified long-format table that combines temporal, spatial, and
#' tarsus-position metrics across individuals and legs.
#'
#' @param res List of results as returned by
#'   \code{batch_compute_kinematics()}.
#'
#' @return A list with components:
#' \describe{
#'   \item{all_leg_measures}{Long-format tibble with one row per
#'     individual, leg, cycle/stride, and metric, combining temporal
#'     measures (swing, contact, cycle time, duty factor), stride
#'     measures (e.g. leg stride length), and tarsus trajectory
#'     summaries.}
#'   \item{leg_times}{Long-format tibble of per-leg temporal measures
#'     (swing time, contact time, cycle time, duty factor) with one row
#'     per individual, leg, and cycle.}
#'   \item{stride_kin}{Long-format tibble of stride-level spatial
#'     kinematics (e.g. COM and leg-based stride length, stride
#'     duration, stride speeds) with one row per individual, leg, and
#'     stride.}
#'   \item{tarsus_summary}{Long-format tibble of median lateral and
#'     minimum/maximum longitudinal tarsus positions, with one row per
#'     individual, tarsus, and metric.}
#' }
#'
#' @export
summarise_kinematics_long <- function(res) {

  res_df <-  dplyr::bind_rows(res)

  meta_tbl <- res_df |>
    dplyr::transmute(
      Individuum,
      body_length, 
      Messnummer = Messnummer,
      mass_mg = `Masse in mg`,
      Genus = Gattung,
      Species = `Artname lt.`,
      fps = Framerate)|>
    dplyr::distinct() |>
    dplyr::mutate(
      ant_group = dplyr::case_when(
        tolower(Genus) %in% c("lasius", "camponotus", "myrmica") ~ "ant",
        !is.na(Genus) ~ "non-ant",
        TRUE ~ "non-ant"
      ), 
      ant_group = factor(ant_group, levels = c("ant", "non-ant"))
    )
  
  # ------------------------------------------------------------
  # Stride kinematics (list-column stride_kinematics)
  # ------------------------------------------------------------

 
  
  # One long table for all legs and strides
  stride_kin_long <- res_df |>
    dplyr::select(Individuum, stride_kinematics) |>
    # T1, T2, T3, R1, ... -> list-column of tibbles, keep the key ("T1", "T2", ...)
    tidyr::unnest_longer(stride_kinematics, indices_to = "leg_key") |>
    # expand each tibble T1/T2/T3 into rows; columns are leg, stride, D_start, ...
    tidyr::unnest(stride_kinematics) |>
    # derive side (L/R) and numeric leg index (1,2,3) from `leg`
    dplyr::mutate(
      side = substr(leg, 1, 1),
      leg_num = as.integer(substr(leg, 2, nchar(leg)))
    ) |>
    # optional: order columns nicely
    dplyr::relocate(
      Individuum, side, leg_num, leg, stride,
      .before = dplyr::everything()
    ) |>
    dplyr::left_join( meta_tbl, by = "Individuum")
  

  # ------------------------------------------------------------
  # Per-leg timing variables (list-column leg_times)
  # ------------------------------------------------------------
 # One long table for all legs and strides
  leg_times_long <- res_df |>
    dplyr::select(Individuum, leg_times) |>
    # T1, T2, T3, R1, ... -> list-column of tibbles, keep the key ("T1", "T2", ...)
    tidyr::unnest_longer(leg_times, indices_to = "tarsus") |>
    tidyr::unnest(leg_times) |>
    # derive side (L/R) and numeric leg index (1,2,3) from `leg`
    dplyr::mutate(
      leg_num = as.integer(substr(tarsus, 2, nchar(tarsus)))
    ) |>
    # optional: order columns nicely
    dplyr::relocate(
      Individuum, leg_num, 
      .before = dplyr::everything()
    ) |>
    dplyr::select(-tarsus) |>
    dplyr::left_join( meta_tbl, by = "Individuum")
  
  # ------------------------------------------------------------
  # Tarsus trajectory summaries (list-column tarsus_traj)
  # ------------------------------------------------------------
  tarsi <- c("T1", "T2", "T3")

  med_u <- sapply(tarsi, function(tarsus) {
    vapply(
      res_df$tarsus_traj,
      function(x) median(x[[tarsus]]$u, na.rm = TRUE),
      numeric(1)
    )
  })
  colnames(med_u) <- paste0(tarsi, "_lat_median")

  min_v <- sapply(tarsi, function(tarsus) {
    vapply(
      res_df$tarsus_traj,
      function(x) min(x[[tarsus]]$v, na.rm = TRUE),
      numeric(1)
    )
  })
  colnames(min_v) <- paste0(tarsi, "_lon_min")

  max_v <- sapply(tarsi, function(tarsus) {
    vapply(
      res_df$tarsus_traj,
      function(x) max(x[[tarsus]]$v, na.rm = TRUE),
      numeric(1)
    )
  })
  colnames(max_v) <- paste0(tarsi, "_lon_max")

  summary_tarsus <- cbind(med_u, min_v, max_v)

  tarsus_summary_long <- summary_tarsus |>
    tibble::as_tibble() |>
    dplyr::mutate(Individuum = res_df$Individuum) |>
    tidyr::pivot_longer(
      cols = -Individuum,
      names_to = c("tarsus", "metric"),
      names_pattern = "^(T[123])_(.*)$",
      values_to = "value"
    ) |>
    mutate(leg_num = as.integer(gsub("T", "", tarsus))) |>
    dplyr::select(-tarsus) |> 
    dplyr::left_join( meta_tbl, by = "Individuum")

  #browser()
    # ---- unified long table --------------------------------------------------
  # temporal measures
  temporal_long <- leg_times_long |>
    dplyr::select(
      Individuum, Messnummer, ant_group, Genus, Species,
       leg_num, body_length, cycle, 
      swing_time, contact_time, cycle_time, duty_factor, mass_mg, fps
    ) |>
    tidyr::pivot_longer(
      cols = c(swing_time, contact_time, cycle_time, duty_factor),
      names_to = "metric",
      values_to = "value"
    )

  # selected stride measures (match by Individuum + leg + stride ~ cycle if needed)
  stride_long <- stride_kin_long |>
    dplyr::select(
      Individuum, Messnummer, ant_group, Genus, Species, leg_num, stride,
      leg_stride_length, midpoint_stride_speed, mass_mg, fps, body_length
    ) |>
    tidyr::pivot_longer(
      cols = c(leg_stride_length, midpoint_stride_speed),
      names_to = "metric",
      values_to = "value"
    ) |>
    # unify naming: treat stride as cycle index for merging / plotting
    dplyr::mutate(cycle = as.integer(gsub("l", "", stride))) |>
    dplyr::select(-stride)

  all_leg_measures <- dplyr::bind_rows(
    temporal_long,
    stride_long) 

  list(
    all_leg_measures = all_leg_measures, 
    leg_times = leg_times_long,
    stride_kin = stride_kin_long, 
    tarsus_summary = tarsus_summary_long
  )
}
