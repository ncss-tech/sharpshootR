
#' @title Mineral Soil Material Criteria from 12th Ed. of KST
#' @description Evaluate mineral soil material criteria based on soil organic carbon, clay content, and length of saturation.
#' 
#' @param soc soil organic carbon percent by mass
#' @param clay clay content percent by mass
#' @param saturation logical, cumulative saturation 30+ days
#' 
#' @return `data.frame` of criteria test results
#' 
#' 
isMineralSoilMaterial <- function(soc, clay, saturation = TRUE) {
  
  saturation <- as.logical(saturation)
  if(is.na(saturation)) {
    stop('`saturation must be logical`', call. = FALSE)
  }
  
  # 1. not saturated for 30+ days (cumulative)
  if(! saturation) {
    
    # there is only one test
    test <- soc < 20
    
    res <- data.frame(
      final = test
    )
    
  } else {
    # 2. saturated for 30+ days (cumulative)  
    
    # a. high-clay soil material
    test.a <- (soc < 18) & (clay >= 60)
    
    # b. no-clay soil material
    test.b <- (soc < 12) & (clay < 1)
    
    # c. everything in-between via sliding-scale
    test.c <- (soc < (12 + clay * 0.1)) & (clay < 60)
    
    # OR(a, b, c)
    final <- test.a | test.b | test.c
    
    # save pieces for later inspection
    res <- data.frame(
      clause.a = test.a,
      clause.b = test.b,
      clause.c = test.c,
      final = final
    )
    
  }
  
  return(res)
}
