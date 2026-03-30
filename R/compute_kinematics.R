
#' Compute gait kinematics 
#'
#' Reads tracked trajectories from a \code{.trk} file, computes
#' leg- and body-level kinematic summaries, and writes two PDF figures
#' for a single individual.
#'
#' All trajectories originate in the camera frame (as recorded in the
#' tracking file), are transformed into a body-anchored frame aligned
#' with the head–abdomen axis for posture-related measures, and into a
#' progression frame whose fore–aft axis is defined by the net
#' displacement between consecutive hind-leg touchdowns for tripod
#' geometry. 
#'
#' In the body frame, tarsus trajectories are expressed in normalized
#' coordinates (u/v, typically scaled by body length) to make tarsi
#' paths comparable across individuals.
#' In the progression frame, three tarsus tripods at specified frames
#' are used to construct "normalized tripod geometries" that summarize
#' spatial placement. 
#'
#' The first PDF shows progression-frame stepping triangles with a
#' body-length scalebar and qualitative head–tail orientation; the
#' second PDF shows body-frame tarsus trajectories of the focal side in
#' the same normalized units, again annotated with body-length
#' scalebars.
#'
#' Coordinate frames used:
#' \itemize{
#'   \item camera frame: original tracked coordinates from the \code{.trk} file;
#'   \item body frame: head–abdomen axis, used for 
#'         body-anchored tarsus trajectories;
#'   \item progression frame: net displacement between consecutive hind-leg
#'         touchdowns, used for normalized tripod progression geometry.
#' }
#'
#' @param filename Character scalar, path to the \code{.trk} file.
#' @param meta Single-row data.frame or tibble with metadata, stride
#'   event timings, tripod coding, and side information.
#' @param plotsize Numeric, half-size of the plotting window in
#'   normalized units, used for x/y limits of the PDFs.
#' @param scale_plot Either the character string \code{"bodylength"}
#'   (the default, using median body length as normalizing factor) or a
#'   numeric value giving the normalizing factor for all transformed
#'   coordinates.
#' @param outdir Character scalar, directory where the PDF files are
#'   written (created recursively if needed).
#'
#' @return A single-row data.frame like \code{meta} with additional
#'   columns:
#'   \itemize{
#'     \item \code{fps}, \code{startframe}, \code{length_unit},
#'           \code{body_length}.
#'     \item \code{midpoint_displacements}, \code{midpoint_durations},
#'           \code{midpoint_speed}: vectors describing body-midpoint
#'           displacements and speeds between mid-leg touchdowns.
#'     \item \code{stride_kinematics}: list-column with per-leg stride
#'           lengths and speeds from \code{per_leg_stride_measures()}.
#'     \item \code{leg_times}: list-column with per-leg swing, contact,
#'           and cycle times from \code{per_leg_times()}.
#'     \item \code{tarsus_traj}: list-column with per-tarsus body-frame
#'           trajectories (u/v) from \code{per_tarsus_traj()}.
#'   }
#'
#'   As a side effect, two PDF files are created in \code{outdir}: one
#'   showing progression-frame stepping triangles, and one showing
#'   body-frame tarsus trajectories.
#'
#' @seealso \code{\link{rot_shift}}, \code{\link{per_leg_stride_measures}},
#'   \code{\link{per_leg_times}}, \code{\link{per_tarsus_traj}}
#'
#' @export
compute_kinematics <- function(
                               filename,
                               meta,
                               plotsize   = 1.5,
                               scale_plot = "bodylength",
                               outdir     = "figures/progression_frame/footfalls/"
                               ) {
  side  <- meta[["Side"]]
  genus <- meta[["Gattung"]]
  mass  <- meta[["Masse in mg"]]
  title <- paste0(genus, " (", mass, " mg)\n", basename(filename))

  meta$trk_file <- filename

  fps <- as.numeric(gsub("[ a-z]*", "", meta[["Framerate"]]))
  meta$fps <- fps

  startframe <- meta[["Startframe"]]
  meta$startframe <- startframe

  # ------------------------------------------------------------
  # Camera frame: body axis and body size
  # ------------------------------------------------------------
  Kopf    <- get_TRK_points(filename, "Kopf")
  Abdomen <- get_TRK_points(filename, "Abdomen")

  Center <- Abdomen
  Center$x <- (Kopf$x + Abdomen$x) / 2
  Center$y <- (Kopf$y + Abdomen$y) / 2

  length_unit <- Kopf$length_unit[1]
  meta$length_unit <- length_unit

  body_length <- median(
    sqrt((Kopf$x - Abdomen$x)^2 + (Kopf$y - Abdomen$y)^2),
    na.rm = TRUE
  )
  meta$body_length <- body_length

  norm_factor <- if (identical(scale_plot, "bodylength")) body_length else scale_plot

  # Orientation (body frame)
  dx_body <- Kopf$x - Abdomen$x
  dy_body <- Kopf$y - Abdomen$y
  phi <- atan2(dy_body, dx_body) - pi / 2

  sf <- meta$Startframe
  ef <- meta$Endframe

  # ------------------------------------------------------------
  # Camera frame: body-midpoint displacements
  # ------------------------------------------------------------
  side_stride <- if (identical(side, "L")) "R" else "L"

  f1 <- unlist(meta[paste0(side_stride, "2D1")]) + startframe
  f2 <- unlist(meta[paste0(side_stride, "2D2")]) + startframe
  f3 <- unlist(meta[paste0(side_stride, "2D3")]) + startframe

  f4 <- unlist(meta[paste0(side,         "2D1")]) + startframe
  f5 <- unlist(meta[paste0(side,         "2D2")]) + startframe
  f6 <- unlist(meta[paste0(side,         "2D3")]) + startframe

  l1 <- sqrt((Center$x[f2] - Center$x[f1])^2 + (Center$y[f2] - Center$y[f1])^2)
  l2 <- sqrt((Center$x[f3] - Center$x[f2])^2 + (Center$y[f3] - Center$y[f2])^2)
  l3 <- sqrt((Center$x[f5] - Center$x[f4])^2 + (Center$y[f5] - Center$y[f4])^2)
  l4 <- sqrt((Center$x[f6] - Center$x[f5])^2 + (Center$y[f6] - Center$y[f5])^2)

  midpoint_displacements <- c(l1, l2, l3, l4)
  meta$midpoint_displacements <- list(midpoint_displacements)

  midpoint_durations <- c(f2 - f1, f3 - f2, f5 - f4, f6 - f5) / fps
  meta$midpoint_durations <- list(midpoint_durations)
  meta$midpoint_speed <- list(midpoint_displacements / midpoint_durations)

  # ------------------------------------------------------------
  # Camera frame: Per-leg stride metrics and timing
  # ------------------------------------------------------------
  Tarsus1_camera <- get_TRK_points(filename, paste0("Tarsus", side, "1"))
  Tarsus2_camera <- get_TRK_points(filename, paste0("Tarsus", side, "2"))
  Tarsus3_camera <- get_TRK_points(filename, paste0("Tarsus", side, "3"))

  Legs <- setNames(
    list(Tarsus1_camera, Tarsus2_camera, Tarsus3_camera),
    paste0(side, 1:3)
  )
  leg_names <- names(Legs)

  stride_kinematics <- lapply(leg_names, function(leg) {
    per_leg_stride_measures(
      meta_row   = meta,
      Center     = Center,
      Leg        = Legs[[leg]],
      leg        = leg,
      fps        = fps,
      startframe = startframe
    )
  })
  names(stride_kinematics) <- gsub("[LR]", "T", leg_names)
  meta$stride_kinematics <- list(stride_kinematics)

  leg_times <- lapply(leg_names, function(leg) {
    per_leg_times(
      meta_row = meta,
      leg      = leg,
      fps      = fps
    )
  })
  names(leg_times) <- gsub("[LR]", "T", leg_names)
  meta$leg_times <- list(leg_times)

  # ------------------------------------------------------------
  # Body frame:  tarsus trajectories
  # ------------------------------------------------------------
  Tarsus1_body <- rot_shift(phi, Center, Tarsus1_camera, side, norm_factor)
  Tarsus2_body <- rot_shift(phi, Center, Tarsus2_camera, side, norm_factor)
  Tarsus3_body <- rot_shift(phi, Center, Tarsus3_camera, side, norm_factor)
  Tarsi <- setNames(
    list(Tarsus1_body, Tarsus2_body, Tarsus3_body),
    paste0(side, 1:3)
  )

  tarsi_names <- names(Tarsi)
  tarsus_traj <- lapply(tarsi_names, function(tarsus) {
    per_tarsus_traj(
      meta_row   = meta,
      Tarsus     = Tarsi[[tarsus]],
      tarsus     = tarsus,
      startframe = startframe
    )
  })
  names(tarsus_traj) <- gsub("[LR]", "T", tarsi_names)
  meta$tarsus_traj <- list(tarsus_traj)

  # ------------------------------------------------------------
  # Progression frame: tripod geometry
  # ------------------------------------------------------------
  tas_code  <- gsub("[0-9]", "", meta[["Dreieck 1"]])
  fs1 <- as.numeric(gsub("[LR]", "", meta[["Dreieck 1"]]))
  fs2 <- as.numeric(meta[["Dreieck 2"]])
  fs3 <- as.numeric(meta[["Dreieck 3"]])

  tas_other <- if (identical(tas_code, "R")) "L" else "R"

  TarsusT11 <- get_TRK_points(filename, paste0("Tarsus", tas_other, "1"))
  TarsusT21 <- get_TRK_points(filename, paste0("Tarsus", tas_code,  "2"))
  TarsusT31 <- get_TRK_points(filename, paste0("Tarsus", tas_other, "3"))
  TarsusT12 <- get_TRK_points(filename, paste0("Tarsus", tas_code,  "1"))
  TarsusT22 <- get_TRK_points(filename, paste0("Tarsus", tas_other, "2"))
  TarsusT32 <- get_TRK_points(filename, paste0("Tarsus", tas_code,  "3"))

  dx_prog <- TarsusT31$x[fs3] - TarsusT31$x[fs1]
  dy_prog <- TarsusT31$y[fs3] - TarsusT31$y[fs1]
  phi2 <- atan2(dy_prog, dx_prog) - pi / 2

  Center2 <- Center
  Center2$x <- Center$x[round((fs1 + fs3) / 2)]
  Center2$y <- Center$y[round((fs1 + fs3) / 2)]

  side2 <- tas_other

  TarsusT11 <- rot_shift(phi2, Center2, TarsusT11, side2, norm_factor)
  TarsusT21 <- rot_shift(phi2, Center2, TarsusT21, side2, norm_factor)
  TarsusT31 <- rot_shift(phi2, Center2, TarsusT31, side2, norm_factor)
  TarsusT12 <- rot_shift(phi2, Center2, TarsusT12, side2, norm_factor)
  TarsusT22 <- rot_shift(phi2, Center2, TarsusT22, side2, norm_factor)
  TarsusT32 <- rot_shift(phi2, Center2, TarsusT32, side2, norm_factor)

  x11 <- TarsusT11$u[fs1]; y11 <- TarsusT11$v[fs1]
  x21 <- TarsusT21$u[fs1]; y21 <- TarsusT21$v[fs1]
  x31 <- TarsusT31$u[fs1]; y31 <- TarsusT31$v[fs1]

  x12 <- TarsusT12$u[fs2]; y12 <- TarsusT12$v[fs2]
  x22 <- TarsusT22$u[fs2]; y22 <- TarsusT22$v[fs2]
  x32 <- TarsusT32$u[fs2]; y32 <- TarsusT32$v[fs2]

  x13 <- TarsusT11$u[fs3]; y13 <- TarsusT11$v[fs3]
  x23 <- TarsusT21$u[fs3]; y23 <- TarsusT21$v[fs3]
  x33 <- TarsusT31$u[fs3]; y33 <- TarsusT31$v[fs3]

  # ------------------------------------------------------------
  # Plot: tripod geometry (progression-anchored frame)
  # ------------------------------------------------------------
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }

  pdf(
    file = file.path(outdir, paste0(basename(filename), ".pdf")),
    height = 5,
    width  = 5
  )
  plot(
    x = c(x11, x21, x31),
    y = c(y11, y21, y31),
    col  = "black",
    pch  = 20,
    main = title,
    xlab = "left-right",
    ylab = "direction of hind-leg tarsus displacement",
    ylim = c(-plotsize, plotsize),
    xlim = c(-plotsize, plotsize),
    xaxt = "n",
    yaxt = "n"
  )

  ## lines(x = c(0, 0), y = c(-0.5, 0.5), col = "grey", lwd = 6)

  lines(x = c(-0.5, 0.5), y = c(-plotsize * 0.9, -plotsize * 0.9), lwd = 2)
  text(x = 0, y = -plotsize * 0.97,
       labels = paste0("body length = ", round(body_length, 0), " mm"),
       cex = 0.9)

  lines(x = c(-plotsize * 0.9, -plotsize * 0.9), y = c(-0.5, 0.5), lwd = 2)
  text(x = -plotsize * 0.97, y = 0,
       labels = "body length", cex = 0.9, srt = 90)
  text(x = -plotsize * 0.9, y = -0.6, labels = "tail", cex = 0.9)
  text(x = -plotsize * 0.9, y = 0.6,  labels = "head", cex = 0.9)

  lines(x = c(x11, x21, x31, x11), y = c(y11, y21, y31, y11), col = "black")

  polygon(
    x = c(x12, x22, x32, x12),
    y = c(y12, y22, y32, y12),
    col    = rgb(1, 0, 0, 0.5),
    border = "red"
  )

  lines(x = c(x13, x23, x33, x13), y = c(y13, y23, y33, y13), col = "black")

  graphics.off()

  # ------------------------------------------------------------
  # Plot: tarsi trajectories (body-anchored frame)
  # ------------------------------------------------------------
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }

  pdf(
    file = file.path(outdir, paste0(basename(filename), "_tarsi_traj.pdf")),
    height = 5,
    width  = 5
  )

  plot(
    x = c(0, 0),
    y = c(-0.5, 0.5),
    col  = "black",
    pch  = 20,
    main = title,
    ylim = c(-plotsize, plotsize),
    xlim = c(-plotsize, plotsize),
    xlab = "left-right",
    ylab = "fore-aft",
    xaxt = "n",
    yaxt = "n"
  )

  lines(tarsus_traj$T1$u,  tarsus_traj$T1$v)
  lines(-tarsus_traj$T2$u, tarsus_traj$T2$v)
  lines(tarsus_traj$T3$u,  tarsus_traj$T3$v)

  lines(x = c(-0.5, 0.5), y = c(-plotsize * 0.9, -plotsize * 0.9), lwd = 2)
  text(x = 0, y = -plotsize * 0.97,
       labels = paste0("body length = ", round(body_length, 0), " mm"),
       cex = 0.9)

  lines(x = c(-plotsize * 0.9, -plotsize * 0.9), y = c(-0.5, 0.5), lwd = 2)
  text(x = -plotsize * 0.97, y = 0,
       labels = "body length", cex = 0.9, srt = 90)
  text(x = 0, y = -0.6, labels = "tail", cex = 0.9)
  text(x = 0, y = 0.6,  labels = "head", cex = 0.9)

  graphics.off()

  meta
}
