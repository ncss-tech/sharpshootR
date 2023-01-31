
# https://github.com/josephguillaume/hydromad/issues/190

# source code: 
# https://github.com/josephguillaume/hydromad/blob/master/R/bucket.R
# 



# test with Morley WB:
# https://casoilresource.lawr.ucdavis.edu/sde/?series=morley#water-balance

library(hydromad)

# initial conditions
AWC <- 198
PPT <- c(0, 0, 0, 0, 60, 100, 70, 0, 0, 0, 0, 0)
PET <- c(0, 0, 0, 0, 120, 120, 60, 0, 0, 0, 0, 0)

d <- data.frame(P = PPT, E = PET)

m <- hydromad::hydromad(d[, 1:2], sma = "bucket", routing = NULL)

# start with "full" bucket
m <- update(m, Sb = AWC, fc = 1, S_0 = 1, a.ss = 0.01, M = 0, etmult = 1, a.ei = 0)
res <- predict(m, return_state = TRUE)

# combine climate inputs + water balance
res <- cbind(d, res)

# compute deficit
res$D <- res$ET - res$E

knitr::kable(res)

