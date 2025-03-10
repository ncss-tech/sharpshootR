##
##
##


#' @title Demonstrate Declining Availability Functions
#' 
#' @description This function ...
#' 
#'
#' @param s character, selection of declining availability function by name ('B', 'C', 'D', 'G')
#' @param res numeric, curve resolution
#' @param ... additional arguments to `plot()` 
#'
#' @export
#' @rdname DAF
#'
plotDAF <- function(s = c('B', 'C', 'D', 'G'), res = 0.01, ...) {
  
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
  lines(.x, .f(.x), type = 'l', lwd = 2)     
  
}


# tunable curve "C"
#' @rdname DAF
#' @param x ordered sequence of VWC in (0, 1).
#' @return AET/PET ratio in (0, 10)
#' @export
#' @keywords internal
DAF.C <- function(x) {
  pmax(pmin(x, 1), 0)
}

# tunable curve "G"
#' @rdname DAF
#' @param x ordered sequence of VWC in (0, 1).
#' @return AET/PET ratio in (0, 10)
#' @export
#' @keywords internal
DAF.G <- function(x) {
  pmax(pmin(1.5 * x, 1), 0)
}

# tunable curve "D"
#' @rdname DAF
#' @param x ordered sequence of VWC in (0, 1).
#' @return AET/PET ratio in (0, 10)
#' @export
#' @keywords internal
DAF.D <- function(x) {
  pmin(1, 1.5 * x^2)
}

# tunable curve "B"
#' @rdname DAF
#' @param x ordered sequence of VWC in (0, 1).
#' @return AET/PET ratio in (0, 10)
#' @export
#' @keywords internal
DAF.B <- function(x) {
  pmax(pmin(4 * x / (3 * x^1 + 1), 1), 0)
}



