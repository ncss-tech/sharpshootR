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

## TODO: make tunable
#' @title DAF "G"
#' @rdname DAF
#' @param x ordered sequence of VWC in (0, 1)
#' @return AET/PET ratio in (0, 1)
#' @export
#' @keywords internal
DAF.B <- function(x) {
  
  pmax(pmin(4 * x / (3 * x^1 + 1), 1), 0)
  
}

## TODO: make tunable
# tunable curve "D"
#' @rdname DAF
#' @param x ordered sequence of VWC in (0, 1)
#' @return AET/PET ratio in (0, 1)
#' @export
#' @keywords internal
DAF.D <- function(x) {
  pmin(1, 1.5 * x^2)
}





