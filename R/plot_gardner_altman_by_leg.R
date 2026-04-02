#' Plot a Gardner-Altman style estimation plot by leg
#'
#' Creates a two-panel Gardner-Altman style estimation plot for one kinematic
#' metric. The top panel shows raw values by group for each leg, faceted by
#' \code{leg_num}. The bottom panel shows the bootstrapped distribution of the
#' mean difference between groups for each leg, defined as \code{ant - non-ant},
#' together with the bootstrap mean and percentile-based 95\% confidence interval.
#'
#' This function is intended for long-format kinematic summary data such as
#' \code{kin_long$tarsus_summary}, and is suitable for repeated use with metrics
#' such as \code{"lon_min"}, \code{"lon_max"}, and \code{"lat_median"}. For
#' \code{metric_of_interest == "lat_median"}, absolute values are used before
#' computing group differences.
#'
#' @param data A data frame containing at least the metric, grouping, leg, and
#'   value columns.
#' @param metric_of_interest Character scalar giving the metric to plot, e.g.
#'   \code{"lon_min"}, \code{"lon_max"}, or \code{"lat_median"}.
#' @param metric_col Character scalar naming the metric column.
#'   Default is \code{"metric"}.
#' @param value_col Character scalar naming the value column.
#'   Default is \code{"value"}.
#' @param group_col Character scalar naming the grouping column.
#'   Default is \code{"ant_group"}.
#' @param leg_col Character scalar naming the leg identifier column.
#'   Default is \code{"leg_num"}.
#' @param n_boot Integer; number of bootstrap replicates per leg.
#'   Default is \code{5000}.
#' @param conf Numeric confidence level for the bootstrap interval.
#'   Default is \code{0.95}.
#' @param raw_point_alpha Numeric alpha for raw jittered points.
#'   Default is \code{0.7}.
#' @param raw_jitter_width Numeric jitter width for raw points.
#'   Default is \code{0.1}.
#' @param violin_fill Fill colour for bootstrap violins.
#'   Default is \code{"grey90"}.
#' @param violin_colour Outline colour for bootstrap violins.
#'   Default is \code{"grey60"}.
#' @param line_zero_linetype Linetype for the horizontal reference line at zero.
#'   Default is \code{2}.
#' @param xlab Character scalar; x-axis label for the raw-data panel.
#'   Default is \code{"Group"}.
#' @param ylab Character scalar; y-axis label for the raw-data panel.
#'   Default is \code{metric_of_interest}.
#' @param title Character scalar; overall plot title. Can include \code{"\n"}
#'   for manual line breaks. Default is a generated title based on
#'   \code{metric_of_interest}.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{plot}{A patchwork object combining the raw-data and estimation panels.}
#'   \item{raw_data}{The filtered data used for plotting.}
#'   \item{boot_data}{A data frame containing bootstrap distributions and summary
#'   statistics for each leg.}
#' }
#'
#' @details
#' The bootstrap difference is computed as the mean in \code{"ant"} minus the
#' mean in \code{"non-ant"}. For this to work as intended, \code{group_col}
#' must contain at least these two levels. The y-axis limits of the lower
#' difference panel are centered on zero and scaled to match the total height of
#' the raw-data panel, allowing visual comparison of effect-size magnitude
#' across legs.
#'
#' @examples
#' \dontrun{
#' res_lon_min <- plot_gardner_altman_by_leg(
#'   data = kin_long$tarsus_summary,
#'   metric_of_interest = "lon_min"
#' )
#' res_lon_min$plot
#'
#' res_lon_max <- plot_gardner_altman_by_leg(
#'   data = kin_long$tarsus_summary,
#'   metric_of_interest = "lon_max"
#' )
#' res_lon_max$plot
#'
#' res_lat_median <- plot_gardner_altman_by_leg(
#'   data = kin_long$tarsus_summary,
#'   metric_of_interest = "lat_median"
#' )
#' res_lat_median$plot
#' }
#'
#' @export
plot_gardner_altman_by_leg <- function(data,
                                       metric_of_interest,
                                       metric_col = "metric",
                                       value_col = "value",
                                       group_col = "ant_group",
                                       leg_col = "leg_num",
                                       n_boot = 5000,
                                       conf = 0.95,
                                       ylim_raw = c(-0.8, 0.8),
                                       ylim_diff = c(-0.8, 0.8), 
                                       subtitle = NULL, 
                                       raw_point_alpha = 0.7,
                                       raw_jitter_width = 0.1,
                                       violin_fill = "grey90",
                                       violin_colour = "grey60",
                                       line_zero_linetype = 2,
                                       xlab = "Group",
                                       ylab = NULL,
                                       title = NULL) {

  # helper: bootstrap mean difference (ant - non-ant)
  boot_mean_diff <- function(df, value_col = "value", group_col = "ant_group",
                             n_boot = 2000) {

    if (!all(c("ant", "non-ant") %in% df[[group_col]])) {
      stop("group_col must contain levels 'ant' and 'non-ant'.")
    }

    idx_ant     <- which(df[[group_col]] == "ant")
    idx_non_ant <- which(df[[group_col]] == "non-ant")

    replicate(n_boot, {
      m_ant     <- mean(df[[value_col]][sample(idx_ant,     replace = TRUE)])
      m_non_ant <- mean(df[[value_col]][sample(idx_non_ant, replace = TRUE)])
      m_ant - m_non_ant
    })
  }

  alpha <- (1 - conf) / 2

  df_metric <- data |>
    dplyr::filter(.data[[metric_col]] == metric_of_interest) |>
    dplyr::mutate(
      !!group_col := droplevels(.data[[group_col]])
    )

  # abs() for lat_median
  if (metric_of_interest == "lat_median") {
    df_metric <- df_metric |>
      dplyr::mutate(
        !!value_col := abs(.data[[value_col]])
      )
  }

  # default labels
  if (is.null(ylab)) {
    ylab <- metric_of_interest
  }

  if (is.null(title)) {
    title <- paste0("Gardner-Altman style estimation plot")
  }

  boot_df <- df_metric |>
    dplyr::group_by(.data[[leg_col]]) |>
    dplyr::group_modify(~{
      b <- boot_mean_diff(
        df = .x,
        value_col = value_col,
        group_col = group_col,
        n_boot = n_boot
      )

      tibble::tibble(
        !!leg_col := unique(.x[[leg_col]]),
        diff = list(b),
        est = mean(b),
        lwr = stats::quantile(b, alpha),
        upr = stats::quantile(b, 1 - alpha)
      )
    }) |>
    tidyr::unnest_longer(diff, values_to = "boot_diff") |>
    dplyr::ungroup()

  p_raw <- ggplot2::ggplot(
    df_metric,
    ggplot2::aes(
      x = .data[[group_col]],
      y = .data[[value_col]],
      colour = .data[[group_col]]
    )
  ) +
    ggplot2::geom_jitter(width = raw_jitter_width, alpha = raw_point_alpha) +
    ggplot2::facet_wrap(stats::as.formula(paste("~", leg_col)), nrow = 1) +
    ggplot2::labs(
      title = paste0("Raw values (faceted by leg number)."),
      x = xlab,
      y = ylab
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(size = 9) # smaller panel title
    ) +
    ylim(ylim_raw[1], ylim_raw[2])

  y_range <- range(df_metric[[value_col]], na.rm = TRUE)
  height  <- diff(y_range)
  diff_lim <- c(-0.5 * height, 0.5 * height) 

  p_diff <- ggplot2::ggplot(
    boot_df,
    ggplot2::aes(
      x = factor(.data[[leg_col]]),
      y = boot_diff,
      group = .data[[leg_col]]
    )
  ) +
    ggplot2::geom_violin(fill = violin_fill, color = violin_colour) +
    ggplot2::geom_pointrange(
      ggplot2::aes(y = est, ymin = lwr, ymax = upr),
      color = "black"
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = line_zero_linetype) +
   # ggplot2::scale_y_continuous(limits = diff_lim) +
   # ggplot2::scale_y_continuous(limits = ylim_diff) +
    ggplot2::labs(
      title = "Bootstrapped mean differences (ant - non-ant)",
      x = "Leg number",
      y = "Difference"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 9) # smaller panel title
    ) +
    ylim(ylim_diff[1], ylim_diff[2])

  p_combined <- (p_raw / p_diff) +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle, 
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 10),  # smaller overall title
        plot.subtitle = ggplot2::element_text(size = 8)
      )
    )

  list(
    plot = p_combined,
    raw_data = df_metric,
    boot_data = boot_df
  )
}
