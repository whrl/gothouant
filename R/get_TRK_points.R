#' Import a point trajectory from a Tracker .trk file
#'
#' Reads a Tracker XML file and extracts the trajectory for a named point
#' (e.g. "Kopf", "Abdomen", "TarsusL1"), converts pixel coordinates to
#' physical units using the embedded scale, and returns a regular
#' frame-by-frame time series.
#'
#' @param filename Character string, path to the .trk file.
#' @param pointname Character string, name of the point in the Tracker file.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{x}{x-position in physical units.}
#'   \item{y}{y-position in physical units.}
#'   \item{frame}{frame index (1..video_framecount).}
#'   \item{px_per_<unit>}{pixels per physical unit (from Tracker).}
#'   \item{delta_t_in_s}{time step between frames in seconds.}
#'   \item{t_in_s}{cumulative time in seconds.}
#'   \item{length_unit}{character, unit name from the Tracker file.}
#'   \item{startframe}{numeric, start frame from the Tracker file.}
#' }
#'
#' @importFrom XML xmlParse getNodeSet xmlValue xmlGetAttr
#' @export
get_TRK_points <- function(filename, pointname) {

  doc <- xmlParse(filename, validate = FALSE)

  length_unit <- xmlValue(
    getNodeSet(doc, "/child::object//child::property[@name='length_unit']")
  )

  frame_count <- as.numeric(
    xmlValue(
      getNodeSet(doc, "/child::object//child::property[@name='video_framecount']")
    )
  )

  scale <- as.numeric(
    xmlValue(
      getNodeSet(doc, "/child::object//child::property[@name='xscale']")
    )
  )

  delta_t <- as.numeric(
    xmlValue(
      getNodeSet(doc, "/child::object//child::property[@name='delta_t']")
    )
  )

  startframe <- as.numeric(
    xmlValue(
      getNodeSet(doc, "/child::object//child::property[@name='startframe']")
    )
  )

  node_pattern <- paste0(
    "/child::object//child::property[@name='name' and text()='",
    pointname,
    "']"
  )

  nodeset <- getNodeSet(doc, node_pattern)
  parent_node <- xmlParent(nodeset[[1]])

  x <- as.numeric(
    xmlValue(
      getNodeSet(parent_node, "property//object//property[@name='x']")
    )
  )

  y <- as.numeric(
    xmlValue(
      getNodeSet(parent_node, "property//object//property[@name='y']")
    )
  )

  frame <- as.numeric(gsub(
    "\\]", "",
    gsub(
      "\\[", "",
      sapply(
        getNodeSet(parent_node, "property/property[@type='object']"),
        xmlGetAttr, "name"
      )
    )
  ))

  d_all <- data.frame(frame = seq_len(frame_count))
  d <- as.data.frame(cbind(x, y, scale, frame, delta_t / 1000))
  d$frame <- as.numeric(d$frame)

  d <- merge(d_all, d, by = "frame", all.x = TRUE)
  d <- d[, c(2, 3, 1, 4, 5)]
  d$frame <- as.numeric(d$frame)

  colnames(d)[4] <- paste0("px_per_", length_unit)
  d$x <- d$x / d[, 4]
  d$y <- d$y / d[, 4]

  colnames(d)[5] <- "delta_t_in_s"
  d$t_in_s <- cumsum(d[, "delta_t_in_s"])

  d$length_unit <- length_unit
  d$startframe <- startframe

  d
}
