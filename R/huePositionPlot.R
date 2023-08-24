

#' @title Hue Position Chart
#' 
#' @description A simple visualization of the hue positions for a given Munsell value/chroma according to Soil Survey Technical Note 2.
#'
#' @param value a single Munsell value
#' 
#' @param chroma a single Munsell chroma
#' 
#' @param chip.cex scaling for color chip rectangle
#' 
#' @param label.cex scaling for color chip 
#' 
#' @param contour.dE00 logical, add dE00 contours from `origin`, imlpicitly `TRUE` when `origin` is not `NULL` 
#' 
#' @param origin point used for distance comparisons can be either single row matrix of CIELAB coordinates, a character vector specifying a Munsell color. By default (`NULL`) represents CIELAB coordinates (L,0,0), where L is a constant value determined by `value` and `chroma`. See examples.
#' 
#' @param origin.cex scaling for origin point
#' 
#' @param grid.res grid resolution for contours, units are CIELAB A/B coordinates. Caution, small values result in many pair-wise distances which could take a very long time.
#' 
#' @param ... additional arguments to [contour()]
#'
#' @export
#' 
#' @return nothing, function is called to generate graphical output
#'
#' @examples
#' 
#' \dontrun{
#' huePositionPlot(value = 4, chroma = 4)
#' 
#' huePositionPlot(value = 6, chroma = 6)
#' 
#' huePositionPlot(value = 8, chroma = 8)
#' 
#' huePositionPlot(value = 6, chroma = 6, contour.dE00 = TRUE, grid.res = 2)
#' 
#' # shift origin to arbitrary CIELAB coordinates or Munsell color
#' huePositionPlot(origin = cbind(40, 5, 15), origin.cex = 0.5)
#' 
#' huePositionPlot(origin = '5G 6/4', origin.cex = 0.5)
#' 
#' huePositionPlot(origin = '10YR 3/4', origin.cex = 0.5)
#' 
#' huePositionPlot(value = 3, chroma = 4, origin = '10YR 3/4', origin.cex = 0.5)
#' 
#' }
#' 
huePositionPlot <- function(value = 6, chroma = 6, chip.cex = 4.5, label.cex = 0.75, contour.dE00 = FALSE, origin = NULL, origin.cex = 0.75, grid.res = 2, ...) {
  
  # interpret !NULL origin argument
  if(!is.null(origin)) {
    contour.dE00 <- TRUE
  }
    
  
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
       xlab = '', 
       ylab = '', 
       main = 'Hue Order per TN #2\nCIELAB Colorspace',
       col.main = par('fg'),
       axes = FALSE
  )
  
  # add '0' lines
  abline(h=0, v=0, lty=2)
  
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
    
    # custom origin
    if(!is.null(origin)) {
      
      # interpret origin
      # matrix of CIELAB coordinates
      if(inherits(origin, 'matrix')) {
        # use as-is
        .to_color <- origin
        # stack coordinates for label
        .to_label <- sprintf("%s\n%s\n%s", origin[1], origin[2], origin[3])
        # sRGB color for display
        .to_hex <- rgb(farver::convert_colour(.to_color, from = 'lab', to = 'rgb', white_to = 'D65') / 255)
      }
      
      # munsell notation
      if(inherits(origin, 'character')) {
        # convert to CIELAB
        .to_color <- as.matrix(parseMunsell(origin, returnLAB = TRUE), nrow = 1)
        # split label into 2 lines
        .pieces <- strsplit(origin, ' ', fixed = TRUE)[[1]]
        .to_label <- sprintf("%s\n%s", .pieces[1], .pieces[2])
        # sRGB color for display
        .to_hex <- rgb(farver::convert_colour(.to_color, from = 'lab', to = 'rgb', white_to = 'D65') / 255)
      }
      
      
      # pair-wise distances from origin
      d <- farver::compare_colour(from = g, to = .to_color, from_space = 'lab', to_space = 'lab', method = 'CIE2000')
    } else {
      # pair-wise distances from [L,0,0]
      d <- farver::compare_colour(from = g, to = cbind(L.val, 0, 0), from_space = 'lab', to_space = 'lab', method = 'CIE2000')
    }
    
    # init matrix of dE00 values along our grid of A and B coordinates
    m <- matrix(
      as.vector(d[, 1]), 
      nrow = length(unique(g$A)), 
      ncol = length(unique(g$B)), 
      byrow = FALSE
    )
    
  }

  # color chips
  points(B ~ A, data = x, pch = 15, col = x$hex, cex = chip.cex)
  
  # label chips, color inversion based on legibility rules set by aqp::invertLabelColor
  text(x$A, x$B, labels = hue.labels, cex = label.cex, col = invertLabelColor(x$hex))
  
  # add optional origin point
  if(!is.null(origin)) {
    points(
      x = .to_color[2], 
      y = .to_color[3], 
      pch = 15, 
      col = .to_hex, 
      cex = chip.cex
    )
    # annotate
    text(
      x = .to_color[2], 
      y = .to_color[3], 
      labels = .to_label, 
      cex = origin.cex, 
      col = invertLabelColor(.to_hex)
    )
  }
  
  # overlay contours last, if requested
  if(contour.dE00) {
    contour(
      x = A.seq, 
      y = B.seq,
      z = m,
      add = TRUE,
      ...
    )
  }
  
  # axis
  box()
  axis(side = 1, cex.axis = 0.8, col.axis = par('fg'), col = par('fg'))
  axis(side = 2, cex.axis = 0.8, col.axis = par('fg'), col = par('fg'), las = 1)
  mtext('A-coordinate', side = 1, col = par('fg'), line = 2.5)
  mtext('B-coordinate', side = 2, col = par('fg'), line = 2.5)
}


