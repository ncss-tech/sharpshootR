library(circlize)

# https://jokergoo.github.io/circlize_book/book/legends.html
# BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)
library(grid)


s <- c('zook', 'pierre', 'lucy', 'redding')
x <- fetchOSD(s, extended = TRUE)



xx <- x$climate.monthly
xx <- xx[grep('ppt', xx$climate_var), ]

u <- unique(xx$series)
n <- length(u)

s <- split(xx, xx$month)

ll <- levels(xx$month)

circos.clear()
circos.par('clock.wise' = TRUE, 'start.degree' = 90, 'gap.degree' = 6, 'points.overflow.warning' = FALSE)

circos.initialize(sectors = ll, xlim = c(0, n))

circos.track(ylim = c(0, 200), track.height = 0.3)

# circos.info(plot = TRUE)

for(i in ll) {
  print(i)
  circos.barplot(value = s[[i]]$q50, pos = 1:n - 0.5, col = 2:(n+1), sector.index = i)
  
  circos.text(x = n / 2, y = 200 + convert_y(2, "mm"), 
              labels = m[as.integer(i)],
              facing = "bending.inside", cex = 1, font = 2,
              adj = c(0.5, 0), niceFacing = TRUE
  )
  
}

circos.yaxis(side = 'left', labels.cex = 0.66, sector.index = '1')





# discrete
lgd_points <- Legend(at = 1:n, labels = u, type = "points", legend_gp = gpar(col = 2:(n+1)))

lgd_list_vertical <- packLegend(lgd_points)

draw(lgd_list_vertical)



# 
# circos.clear()
# circos.par('clock.wise' = TRUE, 'start.degree' = 90)
# 
# circos.initialize('A', xlim = c(0, 12))
# 
# circos.track(ylim = c(-2, 75), panel.fun = function(x, y) {
#   
#   circos.boxplot(t(ET), pos = 1:12 - 0.5, col = 2)
# })
# 
# circos.info(plot = TRUE)
# 
# 
# 
# sectors = letters[1:8]
# circos.initialize(sectors, xlim = c(0, 1))
# for(i in 1:3) {
#   circos.track(ylim = c(0, 1))
# }
# circos.info(plot = TRUE)
# 

