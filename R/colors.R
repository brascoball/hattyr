#' Collect hexidecimal colors
#'
#' @param color The Red Hat color(s) to be converted. Either a single color
#'   name, or a vector of color names.
#'
#'
#' @examples
#' # Get one color
#' x <- redhat_colors("Purple")
#'
#' # Get a vector of colors
#' y <- redhat_colors(c("Storage 2", "Gray 3", "Red Hat Red", "Gray 10"))
#' @export
redhat_colors = function(color) {
  hex_number <- as.character(rh_colors[match(color, rh_colors$name),]$color)
  return(hex_number)
}

#' Show available Red Hat colors.
#'
#' (code primarily take from library(scales))
#'
#'
#' @examples
#' # Show all Red Hat colors
#' show_redhat_colors()
#'
#' @export
show_redhat_colors <- function() {

  rh_hex <- rh_colors$color
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
