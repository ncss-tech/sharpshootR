## re-make `OSDexamples`


library(aqp)
library(soilDB)

x <- c(
  "AMADOR", "ARGONAUT", "CANEYHEAD", "CECIL", "DRUMMER", "HANFORD", 
  "HAYNER", "KLAMATH", "MOGLIA", "MUSICK", "PALAU", "PARDEE", "PENTZ", 
  "REDDING", "SIERRA", "SYCAMORE", "VLECK", "WILLOWS", "YOLO", 
  "ZOOK"
)

OSDexamples <- fetchOSD(x, extended = TRUE)

save(OSDexamples, file = 'data/OSDexamples.rda', compress = 'xz')


