
# https://github.com/ncss-tech/sharpshootR/issues/54

# https://github.com/josephguillaume/hydromad/issues/190


# test with Morley WB:
# https://casoilresource.lawr.ucdavis.edu/sde/?series=morley#water-balance


library(sharpshootR)
library(hydromad)

# initial conditions
S_0 <- 1
AWC <- 198
PPT <- c(0, 0, 0, 0, 60, 100, 70, 0, 0, 0, 0, 0)
PET <- c(0, 0, 0, 0, 120, 120, 60, 0, 0, 0, 0, 0)


S_0 <- 1
AWC <- 200
PPT <- c(0, 0, 0, 0, 60, 100, 70, 0, 0, 0, 0, 0)
PET <- c(0, 0, 0, 0, 120, 120, 120, 0, 0, 0, 0, 0)


# 
S_0 <- 1
AWC <- 100
PPT <- c(20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PET <- c(0, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20)

# #
# S_0 <- 1
# AWC <- 100
# PPT <- c(0, 0, 0, 0, 0, 100, 0, 0, 0, 0, 0, 0)
# PET <- c(0, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20)


hydromad.options('pure.R.code' = TRUE)

d <- data.frame(P = PPT, E = PET)

m <- hydromad::hydromad(d[, 1:2], sma = "bucket", routing = NULL)

# start with "full" bucket
m <- update(m, Sb = AWC, fc = 0.5, S_0 = S_0, a.ss = 0, M = 0.5, etmult = 1, a.ei = 0)
res <- predict(m, return_state = TRUE)

# combine climate inputs + water balance
res <- cbind(d, res)

# compute deficit
res$D <- res$ET - res$E

knitr::kable(res)

## TODO: check mass balance
sum(c(0, diff(res$S))) + sum(res$P) - sum(res$ET)


w <- monthlyWB(AWC = AWC, PPT = PPT, PET = PET, S_init = S_0, rep = 1)
# monthlyWB_summary(w)
plotWB(w)







