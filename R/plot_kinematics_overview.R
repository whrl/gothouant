#' Plot overview of kinematic measures by leg and group
#'
#' Creates a faceted jitter + boxplot overview of selected kinematic
#' measures across legs and groups. Each facet shows the distribution of
#' one metric across leg numbers, with points coloured by
#' \code{ant_group} and overlaid boxplots summarising the distribution.
#'
#' The function expects a long-format data set where each row
#' corresponds to one observation of a given metric for a specific leg
#' and individual.
#'
#' @param all_measures A data frame (or tibble) in long format with at
#'   least the columns \code{leg_num} (numeric or integer leg index),
#'   \code{value} (numeric kinematic measure), \code{metric} (factor or
#'   character identifying the measure), and \code{ant_group} (factor or
#'   character with levels such as \code{"ant"} and
#'   \code{"non-ant"}).
#'
#' @return A \code{ggplot} object showing per-leg distributions of each
#'   metric as jittered points and boxplots, faceted by
#'   \code{metric}.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' all_measures <- tibble::tibble(
#'   leg_num   = rep(1:6, each = 40),
#'   value     = rnorm(240, mean = 1, sd = 0.2),
#'   metric    = rep(c("stride_length", "stride_speed"), each = 120),
#'   ant_group = rep(c("ant", "non-ant"), times = 120)
#' )
#'
#' p <- plot_kinematics_overview(all_measures)
#' print(p)
#' }
#'
#' @export
plot_kinematics_overview <- function(all_measures) {
  ggplot2::ggplot(
    all_measures,
    ggplot2::aes(
      x      = as.factor(leg_num),
      y      = value,
      colour = ant_group
    )
  ) +
    ggplot2::geom_jitter(
      position = ggplot2::position_jitterdodge(
        jitter.width = 0.1,
        dodge.width  = 0.6
      ),
      alpha = 0.4, size = 1
    ) +
    ggplot2::geom_boxplot(
      width   = 0.5,
      alpha   = 0.3,
      outlier.shape = NA,
      position = ggplot2::position_dodge(width = 0.6)
    ) +
    ggplot2::facet_wrap(~ metric, scales = "free_y") +
    ggplot2::scale_colour_manual(
      values = c("ant" = "#01696f", "non-ant" = "#964219")
    ) +
    ggplot2::labs(
      x      = "Leg",
      y      = "Value",
      colour = "Group"
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )
}
