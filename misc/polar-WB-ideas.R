library(soilDB)
library(ggplot2)

s <- 'auburn'
x <- fetchOSD(s, extended = TRUE)

PPT <-  x$climate.monthly[x$climate.monthly$variable == 'Precipitation (mm)', ]

PPT$month <- structure(1:12, .Label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), class = "factor")

PET <-  x$climate.monthly[x$climate.monthly$variable == 'Potential ET (mm)', ]

PET$month <- structure(1:12, .Label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), class = "factor")


# doesn't work... even though it seems like it should
z <- rbind(
  PPT[c(1:12, 1), ],
  PET[c(1:12, 1), ]
  )



ggplot(z, aes(x = month, y = q50, group = variable, color = variable)) +  
  geom_line(show.legend = FALSE) + 
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = variable), alpha = 0.2) +
  coord_polar(theta="x", start = pi/12 , direction = 1) +
  theme_light()

  
