#' Plot mixed-model predictions with raw data and species means
#'
#' This function plots a continuous predictor against midpoint stride speed,
#' overlaying raw observations, individual/species means, unadjusted group-wise
#' linear fits, and conditional mixed-model predictions by ant group.
#'
#' Raw points show all observations. Filled points show individual means
#' (grouped by \code{id_var} and \code{species_var}). Dashed lines are ordinary
#' least-squares fits to the raw data within each ant group, summarizing the
#' unadjusted association between \code{xvar} and \code{yvar}. Solid coloured
#' lines and ribbons are population-level mixed-model predictions from
#' \code{ggeffects::ggpredict()}, representing the conditional effect of
#' \code{xvar} on \code{yvar} by ant group, adjusted for body length and other
#' kinematic covariates and averaging over random effects. Because these
#' conditional prediction curves are adjusted, they do not necessarily pass
#' through the raw point cloud or the individual means.
#'
#' @param model Fitted mixed-effects model object (compatible with
#'   \code{ggeffects::ggpredict()}).
#' @param data Data frame used to fit \code{model}.
#' @param xvar Character scalar; name of the focal predictor on the x-axis.
#' @param yvar Character scalar; name of the response variable
#'   (default: \code{"midpoint_stride_speed"}).
#' @param ylab Character scalar; y-axis label.
#' @param xlab Character scalar; x-axis label.
#' @param title Character scalar; plot title.
#' @param caption Character scalar; plot caption. If empty, a default caption
#'   describing all plot elements is used.
#' @param sizevar Character scalar; name of variable mapped to point size
#'   (default: \code{"mass_mg"}).
#' @param group_var Character scalar; grouping variable (default: \code{"ant_group"}).
#' @param id_var Character scalar; individual identifier (default: \code{"Individuum"}).
#' @param species_var Character scalar; species identifier (default: \code{"Species"}).
#' @param point_alpha Numeric; alpha for raw points.
#' @param point_size Numeric; base size for raw points (unused if size mapped).
#' @param mean_size Numeric; base size for mean points (unused if size mapped).
#' @param line_size Numeric; line width for prediction lines.
#' @param label_size Numeric; text size for species labels.
#' @param show_ci Logical; if \code{TRUE}, draw prediction ribbons.
#' @param show_species_labels Logical; if \code{TRUE}, label species means.
#' @param add_lit_line Logical; if \code{TRUE}, add a literature reference line.
#'
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
#' # plot_mixed_effect(m_speed, d_speed, xvar = "scale_swing_time",
#' #   xlab = "Swing time (scaled)",
#' #   ylab = expression("Midpoint stride speed ("*mm~s^{-1}*")"),
#' #   title = "Conditional effect of swing time on midpoint stride speed")
plot_mixed_effect <- function(model,
                                     data,
                                     xvar,
                                     yvar = "midpoint_stride_speed",
                                     ylab = "Midpoint stride speed",
                                     xlab = "",
                                     title = "",
                                     caption = "",
                                     sizevar = "mass_mg",
                                     group_var = "ant_group",
                                     id_var = "Individuum",
                                     species_var = "Species",
                                     point_alpha = 0.22,
                                     point_size = 1.5,
                                     mean_size = 2.8,
                                     line_size = 1.1,
                                     label_size = 3.2,
                                     size_label = "Body size in mm", 
                                     show_ci = TRUE,
                                     show_species_labels = TRUE,
                                     add_lit_line = FALSE) {

  if (identical(caption, "")) {
    caption <- paste(
      "Raw points and individual means show the observed data distribution for each ant group.",
      "Dashed lines are ordinary least-squares fits to the raw data within groups,",
      "summarizing the unadjusted association between the focal predictor and midpoint stride speed.",
      "Solid coloured lines and ribbons show mixed-model predicted midpoint stride speed",
      "as a function of the focal predictor by ant group, conditional on body length and the other",
      "kinematic covariates and with random effects averaged to the population level.",
      "Because these conditional prediction curves are adjusted, they do not necessarily pass",
      "through the raw point cloud or the individual means."
    )
  }

  pred <- ggpredict(model, terms = c(xvar, group_var)) |>
    as.data.frame()

  names(pred)[names(pred) == "x"] <- xvar
  names(pred)[names(pred) == "group"] <- group_var

  means_df <- data |>
    dplyr::mutate(ID = paste0(.data[[id_var]]," " , .data[[species_var]])) |> 
    dplyr::group_by(.data[[group_var]], ID) |>
    dplyr::summarise(
      x = mean(.data[[xvar]], na.rm = TRUE),
      y = mean(.data[[yvar]], na.rm = TRUE),
      size = mean(.data[[sizevar]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(
      !!xvar := x,
      !!yvar := y,
      species_label = .data[["ID"]]
    )

  p <- ggplot() +
    theme_classic() +
    labs(
      x = xlab,
      y = ylab,
      colour = "Group",
      fill = "Group",
      shape = "Group",
      title = title,
      caption = caption
    ) +
    geom_point(
      data = data,
      aes(x = .data[[xvar]],
          y = .data[[yvar]],
          shape = .data[[group_var]],
          colour = .data[[group_var]],
          fill = .data[[group_var]],
          size = .data[[sizevar]]),
      alpha = point_alpha
      # size = point_size
    ) +
    geom_smooth(
      data = data,
      aes(x = .data[[xvar]],
          y = .data[[yvar]],
          colour = .data[[group_var]]),
      method = "lm",
      se = FALSE,
      linetype = "dashed",
      linewidth = 0.6,
      inherit.aes = FALSE
    )

  if (show_ci) {
    p <- p +
      geom_ribbon(
        data = pred,
        aes(
          x = .data[[xvar]],
          ymin = conf.low,
          ymax = conf.high,
          fill = .data[[group_var]]
        ),
        alpha = 0.15,
        colour = NA,
        inherit.aes = FALSE
      )
  }

  p <- p +
    geom_line(
      data = pred,
      aes(
        x = .data[[xvar]],
        y = predicted,
        colour = .data[[group_var]]
      ),
      linewidth = line_size,
      inherit.aes = FALSE
    )

  if (add_lit_line) {
    p <- p +
      geom_abline(
        slope = 50, intercept = -236,
        linetype = "dashed",
        linewidth = 1,
        colour = "black"
      ) +
      annotate(
        "text",
        x = Inf, y = -Inf,
        hjust = 1.05, vjust = -0.6,
        label = "Literature: C. bombycina\nstride_length = 0.02*speed + 4.72",
        size = 3.3,
        colour = "black"
      )
  }

  p <- p +
    geom_point(
      data = means_df,
      aes(x = .data[[xvar]],
          y = .data[[yvar]],
          shape = .data[[group_var]],
          fill = .data[[group_var]],
          colour = .data[[group_var]],
          size = .data[["size"]]),
      stroke = 0.4
      # size = mean_size
    )

  if (show_species_labels) {
    p <- p +
      ggrepel::geom_label_repel(
        data = means_df,
        aes(
          x = .data[[xvar]],
          y = .data[[yvar]],
          label = species_label
        ),
        size = label_size,
        lineheight = 0.5,
        show.legend = FALSE,
        box.padding = 0.15,
        point.padding = 0.15,
        nudge_y = 0,
        nudge_x = 0,
        force = 5,
        force_pull = 0.1,
        segment.alpha = 0.5,
        segment.size = 0.5,
        max.overlaps = Inf,
        min.segment.length = 0,
        label.size = 0.1,
        label.padding = unit(0.15, "lines"),
        label.r       = unit(0.1, "lines")
      )
  }

  p <- p +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      plot.caption = element_text( size = 6)
    ) +
    scale_colour_manual(values = c("ant" = "#1f77b4",
                                   "non-ant" = "#ff7f0e")) +
    scale_fill_manual(values = c("ant" = "#1f77b4",
                                 "non-ant" = "#ff7f0e")) +
    scale_shape_manual(values = c("ant" = 22, "non-ant" = 21)) +
    scale_size_continuous(name = size_label)

  p
}
