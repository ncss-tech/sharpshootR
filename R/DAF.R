
##
##
##


#' @title Graphical Demonstration of Declining Availability Functions
#' 
#' @description This function is provides a standardized approach for the visualization of "declining availability functions" (DAF) used by various water-balance simulations. The DAF is an approximation of physical constraints on plant water extraction efficiency, as a function of declining soil moisture.
#' 
#'
#' @param s character, selection of declining availability function by name ('B', 'C', 'D', 'G')
#' @param res numeric, curve resolution typically within the range of 0.01 to 0.1
#' @param ... additional arguments to a DAF (see examples)
#'
#' @export
#' @returns nothing, function called for graphical output
#' @rdname DAF
#' 
#' @examples
#'
#'
#' # multi-figure output
#' op <- par(no.readonly = TRUE)
#' par(mfrow = c(2, 2))
#' 
#' # defaults
#' plotDAF('C')
#' plotDAF('G')
#' plotDAF('B')
#' plotDAF('D')
#' 
#' 
#' # adjust intercept term for DAF 'C'
#' plotDAF('C', intercept = 0.1)
#' title(sub = 'intercept = 0.1', font.sub = 3)
#' plotDAF('C', intercept = 0.3)
#' title(sub = 'intercept = 0.3', font.sub = 3)
#' 
#' # adjust threshold term for DAF 'G'
#' plotDAF('G', threshold = 0.5)
#' title(sub = 'threshold = 0.5', font.sub = 3)
#' plotDAF('G', threshold = 0.9)
#' title(sub = 'threshold = 0.9', font.sub = 3)
#' 
#' 
#' # reset output device options
#' par(op) 
#'
plotDAF <- function(s = c('B', 'C', 'D', 'G'), res = 0.01, ...) {
  
  s <- match.arg(s)
  
  .x <- seq(0, 1, by = res)
  
  .f <- switch(s,
               'B' = DAF.B,
               'C' = DAF.C,
               'D' = DAF.D,
               'G' = DAF.G
  )
  
  .txt <- sprintf("Declining Availability Function %s", s)
  
  plot(
    1, 1, 
    type = 'n', 
    # asp = 1, 
    xlim = c(0, 1), 
    ylim = c(0, 1), 
    las = 1, 
    axes = FALSE,
    xlab = 'Soil Moisture % of Total', 
    ylab = 'AET/PET', 
    main = .txt
  )
  
  grid()
  
  axis(side = 1, at = seq(0, 1, by = 0.2))
  axis(side = 2, at = seq(0, 1, by = 0.2), las = 1)
  
  abline(h = c(0, 0.5, 1), v = c(0, 0.5, 1), col = 2, lty = 2)
  abline(a = 0, b = 1, col = grey(0.6))
  lines(.x, .f(.x, ...), type = 'l', lwd = 2)     
  
}


#' @title DAF "C"
#' @rdname DAF
#' @param x ordered sequence of VWC in (0, 1)
#' @param intercept y-intercept, use to limit the minimum AET/PET ratio, must be within (0, 1)
#' @return AET/PET ratio in (0, 1)
#' @export
#' @keywords internal
DAF.C <- function(x, intercept = 0) {
  
  # adjust slope according to intercept
  #
  # m = (y2 - y1) / (x2 - x1)
  # y2 always 1
  # y1 = intercept
  # x2 always 1
  # x1 always 0
  .m <- (1 - intercept) / (1 - 0)
  
  # linear function
  .y <- .m * x + intercept
  
  # constrain to physical limits
  .res <- pmax(pmin(.y, 1), 0)
  
  return(.res)
}

#' @title DAF "G"
#' @rdname DAF
#' @param x ordered sequence of VWC in (0, 1)
#' @param threshold soil moisture fraction threshold above which AET/PET is constant. within (0, 1)
#' @return AET/PET ratio in (0, 1)
#' @export
#' @keywords internal
DAF.G <- function(x, threshold = 0.75) {
  
  # adjust slope according to threshold
  #
  # m = (y2 - y1) / (x2 - x1)
  # y2 always 1
  # y1 always 0
  # x2 = threshold
  # x1 always 0
  .m <- (1 - 0) / (threshold - 0)
  
  # linear function
  .y <- .m * x + 0
  
  # constrain to physical limits
  .res <- pmax(pmin(.y, 1), 0)
  
  return(.res)
}

#' @title DAF "B"
#' @rdname DAF
#' @param x ordered sequence of VWC in (0, 1)
#' @param steepness number between 0 and 2.5
#' @return AET/PET ratio in (0, 1)
#' @export
#' @keywords internal
DAF.B <- function(x, steepness = 1) {
  
  pmax(pmin(4 * x / (3 * x^steepness + 1), 1), 0)
  
}

# tunable curve "D"
#' @rdname DAF
#' @param x ordered sequence of VWC in (0, 1)
#' @param threshold soil moisture fraction threshold above which AET/PET is constant
#' @param intercept AET/PET value when soil moisture fraction is 0
#' @return AET/PET ratio in (0, 1)
#' @export
#' @keywords internal
DAF.D <- function(x, threshold = 0.8, intercept = 0) {
  
  # constrain to reasonable range
  if(threshold < 0.001 | threshold > 1) {
    stop('threshold should be within 0.001 <= x <= 1')
  }
  
  if(intercept < 0 | intercept >= 1) {
    stop('intercept should be within 0 <= x < 1')
  }
  
  # a parabola can be defined with three points
  # limit one of those points to pass through (0, intercept)
  # limit the other two to pass through (-threshold, 1) and (threshold, 1)
  
  # 3 systems of equations
  # 3 unknowns
  # y = ax^2 + bx + c
  
  # coefficients
  A <- matrix(
    rbind(
      c((-threshold)^2, -threshold, 1),
      c(0, 0, 1),
      c(threshold^2, threshold, 1)
    ), nrow = 3,
  )
  
  # known y values
  b <- matrix(
    c(1, intercept, 1), 
    nrow = 3
  )
  
  # safely invert and multiply
  s <- solve(A, b)
  s <- as.vector(s)
  
  # curve = 1 for all x beyond threshold
  # constrain to 0-1
  pmax(pmin((s[1] * x^2) + (s[2] * x) + s[3], 1), 0)
  
}





