library(aqp)
library(soilDB)
# library(sharpshootR)

# special installation instructions:
# http://hydromad.catchment.org/#installation
library(hydromad)


data("ROSETTA.centroids")

idx <- which(ROSETTA.centroids$texture %in% c('clay'))
vars <- c('texture', 'pwp', 'fc', 'sat', 'awc')



# near Sonora, CA
# thermic / xeric climate
# units are mm
PPT <- c(65, 59, 57, 28, 13, 3, 0, 1, 4, 20, 33, 53)
PET <- c(14, 22, 38, 54, 92, 125, 154, 140, 106, 66, 29, 14)

w <- ROSETTA.centroids[idx, vars]


d <- data.frame(
  P = PPT,
  E = PET
)



compareBuckets <- function(d, Sb = 47, fc = 1, S_0 = 1, a.ss = 0.01, M = 0, etmult = 1, a.ei = 0) {
  # original implementation
  m <- hydromad::hydromad(d, sma = "bucket", routing = NULL)
  m <- update(m, Sb = Sb, fc = fc, S_0 = S_0, a.ss = a.ss, M = M, etmult = 1, a.ei = 0)
  res <- predict(m, return_state = TRUE)
  
  # combine original PPT,PET with results
  res <- data.frame(d, res)
  
  # cleanup names
  names(res) <- c('PPT', 'PET', 'U', 'S', 'ET')
  
  
  # with "fix" that ensures AET cannot be > S[t]
  res2 <- sharpshootR:::.monthlyBucket(d, Sb = Sb, fc = fc, S_0 = S_0, a.ss = a.ss, M = M, etmult = 1, a.ei = 0)
  
  # combine original PPT,PET with results
  res2 <- data.frame(d, res2)
  
  # cleanup names
  names(res2) <- c('PPT', 'PET', 'U', 'S_new', 'ET_new')
  
  # compare differences
  x <- data.frame(
    PPT = res$PPT,
    PET = res$PET,
    U = res$U,
    S = res$S, 
    S_new = res2$S_new,
    dS = ifelse(res$S != res2$S_new, '*', ''),
    ET = res$ET,
    ET_new = res2$ET_new,
    dET = ifelse(res$ET != res2$ET_new, '*', '')
  )
  
  return(x)
}


# 50mm total storage
# fc  = awc / (sat - pwp)
c1 <- compareBuckets(d, Sb = 50, fc = w$awc / (w$sat - w$pwp), S_0 = 0)
c2 <- compareBuckets(d, Sb = 50, fc = 1, S_0 = 0)
c3 <- compareBuckets(d, Sb = 50, fc = 0.1, S_0 = 0)

knitr::kable(
  c1, 
  digits = 2
)

knitr::kable(
  c2, 
  digits = 2
)

knitr::kable(
  c3, 
  digits = 2
)


sum(c1$ET) - sum(c2$ET)
sum(c1$ET_new) - sum(c2$ET_new)

sum(c1$ET) - sum(c1$ET_new)
sum(c2$ET) - sum(c2$ET_new)
