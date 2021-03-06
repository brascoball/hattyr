#' Collect hexidecimal colors
#'
#' @param color The Red Hat color(s) to be converted. Either a single color
#'   name, or a vector of color names.
#' @param partial  A logical scalar. Do you want to match partial color names?
#' @param exclude A logical scalar. Do you want get all colors that don't match the color?
#'
#'
#' @examples
#' # Get one color
#' x <- redhat_colors("Purple")
#'
#' # Get a vector of colors
#' y <- redhat_colors(c("Storage 2", "Gray 3", "Red Hat Red", "Gray 10"))
#' z <- redhat_colors("Purple", partial = TRUE)
#' non_grays <- redhat_colors(c("Gray", "Black", "White"), partial = TRUE, exclude = TRUE)
#'
#' @export
redhat_colors = function(color, partial = FALSE, exclude = FALSE) {
  if (partial) {
    if (length(color) > 1) color = paste(color, collapse = "|")
    hex.numbers <- as.character(rh_colors[grep(color, rh_colors$name),]$color)
  } else {
    hex.numbers <- as.character(rh_colors[match(color, rh_colors$name),]$color)
  }
  if (exclude) {
    hex.numbers <- as.character(rh_colors[!(rh_colors$color %in% hex.numbers),]$color)
  }
  return(hex.numbers)
}

#' Show available Red Hat colors.
#'
#' @import graphics
#'
#'
#' @examples
#' # Show all Red Hat colors
#' show_redhat_colors()
#'
#' @export
show_redhat_colors <- function() {

  rh_hex <- as.character(rh_colors$color)
  n <- length(rh_hex)
  ncol <- ceiling(sqrt(n))
  nrow <- ceiling(n / ncol)
  rh_hex <- c(rh_hex, rep(NA, nrow * ncol - length(rh_hex)))
  rh_hex <- matrix(rh_hex, ncol = ncol, byrow = TRUE)

  old <- par(pty = "s", mar = c(0, 0, 0, 0))
  on.exit(par(old))

  size <- max(dim(rh_hex))
  plot(c(0, size), c(0, -size), type = "n", xlab="", ylab="", axes = FALSE)
  rect(col(rh_hex) - 1, -row(rh_hex) + 1, col(rh_hex), -row(rh_hex),
       col = rh_hex, border = '#FFFFFF')

  rh_names <- gsub(" ", "\n", rh_colors$name)
  rh_names <- c(rh_names, rep(NA, nrow * ncol - length(rh_names)))
  rh_names <- matrix(rh_names, ncol = ncol, byrow = TRUE)
  text(col(rh_hex) - 0.5, -row(rh_hex) + 0.5, rh_names, cex = 0.75)
}
