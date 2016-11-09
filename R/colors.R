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
