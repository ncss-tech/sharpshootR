library(sharpshootR)
library(hydromad)
library(gifski)

S_0 <- 1
AWC <- 100
PET <- c(0, 0, 5,80, 90, 120, 130, 140, 110, 90, 20, 5)
PPT <- c(0, 150, 200, 120, 20, 0, 0, 0, 10, 20, 30, 60)


f <- function(x) {
  
  w <- monthlyWB(AWC = x, PPT = PPT, PET = PET, S_init = S_0, rep = 3, keep_last = TRUE, distribute = TRUE, k = 15, method = 'equal') 
  
  par( mar = c(4, 3.5, 3, 2))
  plotWB(w, legend.cex = 0.7, month.cex = 0.8, ylim = c(-150, 200)) ; title('Monthly Totals', cex.main = 1)
}

f(1)
f(100)


gifski::save_gif(
  sapply(100:1, f), 
  gif_file = 'e:/temp/s.gif', 
  delay = 0.1, 
  width = 800, 
  height = 400
)
