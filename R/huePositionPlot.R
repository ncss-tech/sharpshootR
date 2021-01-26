

#' @title Hue Position Chart
#' 
#' @description A simple visualization of the hue positions for a given Munsell value/chroma according to \href{https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569}{Soil Survey Technical Note 2}.
#'
#' @param value a single Munsell value
#' @param chroma a single Munsell chroma
#' @param chip.cex scaling for color chip rectangle
#' @param label.cex scaling for color chip 
#' @param contour.dE00 logical, add dE00 contours from CIELAB coordinates (L,0,0), L is a constant value determined by `value` and `chroma`
#' @param grid.res grid resolution for contours, units are CIELAB A/B coordinates. Caution, small values result in many pair-wise distances which could take a very long time.
#'
#' @export
#'
#' @examples
#' 
#' huePositionPlot(value = 6, chroma = 6, contour.dE00 = TRUE, grid.res = 2)
#' 
huePositionPlot <- function(value = 6, chroma = 6, chip.cex = 4.5, label.cex = 0.75, contour.dE00 = FALSE, grid.res = 2) {
  
  # dE00 contours requires farver >= 2.0.3
  if(contour.dE00) {
    if( !requireNamespace('farver', quietly = TRUE) | packageVersion("farver") < '2.0.3' ) {
      message('farver v2.0.3 or higher required for dE00 contours')
      colorSpace <- 'LAB';
    }
  }
  
  # hues used in the description of soil color (except for neutral hues)
  hues <- huePosition(x = NULL, returnHues = TRUE)
  # convert hue names into position
  hue.pos <- huePosition(hues)
  
  
  # make these into real colors by fixing value and chroma at '6'
  colors <- sprintf("%s %s/%s", hues, value, chroma)
  # convenient labels for figure
  hue.labels <- sprintf("%s\n%s", hues, hue.pos)
  
  # combine colors names, hex representation of colors, and CIELAB coordinates
  x <- data.frame(
    colors, 
    hex = parseMunsell(colors), 
    parseMunsell(colors, returnLAB=TRUE), 
    stringsAsFactors = FALSE
  )
  
  # start the figure, plot nothing
  plot(B ~ A, 
       data = x, 
       type = 'n', 
       asp = 0.75, 
       xlab = 'A-coordinate', 
       ylab = 'B-coordinate', 
       main = 'Hue Order per TN #2\nCIELAB Colorspace',
       axes = FALSE
  )
  
  
  # add dE00 contours from [L,0,0]
  if(contour.dE00) {
    # this should be a single number because hue/value are fixed
    L.val <- round(range(x$L))[1]
    
    # derive range from set of chips
    A.range <- round(range(x$A))
    B.range <- round(range(x$B))
    
    A.seq <- seq(from = A.range[1], to = A.range[2], by = grid.res)
    B.seq <- seq(from = B.range[1], to = B.range[2], by = grid.res)
    
    # establish grid, note that L coordinate is constant
    g <- expand.grid(
      L = L.val,
      A = A.seq,
      B = B.seq
    )
    
    ## TODO: what would dE00 contours from some other point look like / mean?
    # pair-wise distances from [L,0,0]
    d <- farver::compare_colour(from = g, to = cbind(L.val, 0, 0), from_space = 'lab', to_space = 'lab', method = 'CIE2000')
    
    # init matrix of dE00 values along our grid of A and B coordinates
    m <- matrix(
      as.vector(d[, 1]), 
      nrow = length(unique(g$A)), 
      ncol = length(unique(g$B)), 
      byrow = FALSE
    )
    
    # add contours
    contour(
      x = A.seq, 
      y = B.seq,
      z = m,
      add = TRUE
    )
  }
  
  # add '0' lines
  abline(h=0, v=0, lty=2)
  
  # color chips
  points(B ~ A, data=x, pch=15, col=x$hex, cex = chip.cex)
  
  # label chips, color inversion based on legibility rules set by aqp::invertLabelColor
  text(x$A, x$B, labels = hue.labels, cex = label.cex, col = invertLabelColor(x$hex))
  
  # axis
  box()
  axis(side = 1, cex.axis = 0.8)
  axis(side = 2, cex.axis = 0.8, las = 1)
  
}


