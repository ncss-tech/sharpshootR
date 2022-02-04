library(ggplot2)
library(reshape2)

s <- 'auburn'
x <- fetchOSD(s, extended = TRUE)

PPT <-  x$climate.monthly[x$climate.monthly$variable == 'Precipitation (mm)', ]

PPT$month <- structure(1:12, .Label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), class = "factor")

PET <-  x$climate.monthly[x$climate.monthly$variable == 'Potential ET (mm)', ]

PET$month <- structure(1:12, .Label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), class = "factor")



z <- rbind(
  PPT[c(12, 1:12), ], 
  PET[c(12, 1:12), ]
  )

z <- rbind(
  PPT,
  PET
)

# z.poly <- data.frame(
#   variable = c(z$variable, z$variable),
#   x = c(z$month, rev(z$month)),
#   y = c(z$q75, rev(z$q25))
# )

i <- c(seq_along(z$variable), which(z$variable == "Precipitation (mm)"))

z.poly <- rbind(
  data.frame(z[i, c('month', 'variable')], CI = z$q25[i], type="q25") ,
  data.frame(z[i, c('month', 'variable')], CI = z$q75[i], type="q75")
)

z.poly <- rbind(
  data.frame(z[, c('month', 'variable')], CI = z$q25, type="q25") ,
  data.frame(z[, c('month', 'variable')], CI = z$q75, type="q75")
)

z.poly


ggplot(z, aes(x = month, y = q50, group = variable)) +  

  geom_polygon(aes(group = variable, colour = variable), fill = NA, show.legend = FALSE) +  
  geom_polygon(aes(x = month, y = CI, fill = variable, group = variable), data = z.poly, alpha = 0.4) +

  coord_polar(theta="x", start = 0 , direction = 1) +
  theme_light()

  
