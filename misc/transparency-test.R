library(aqp)
library(sharpshootR)
library(venn)
library(scales)

# https://stephango.com/flexoki
cols <- c('#AF3029', '#BC5215', '#AD8301', '#66800B', '#24837B', '#205EA6', '#5E409D')
# cols <- c('#D14D41', '#DA702C', '#D0A215', '#879A39', '#3AA99F', '#4385BE', '#8B7EC8')
venn(7, ellipse = TRUE, zcolor = cols, opacity = 0.5)


cols <- rainbow(4)
venn(4, ellipse = TRUE, zcolor = cols, opacity = 0.5)

m <- rgb2munsell(t(col2rgb(cols)) / 255)
m$m <- sprintf("%s %s/%s", m$hue, m$value, m$chroma)

colorMixtureVenn(m$m, ellipse = TRUE, mixingMethod = 'estimate')

par(mfcol = c(1, 2))
venn(4, ellipse = TRUE, zcolor = cols, opacity = 0.5)
colorMixtureVenn(m$m, ellipse = TRUE, mixingMethod = 'estimate')



m <- c('10R 6/10', '10P 6/10', '10G 6/10', '10Y 8/6')

par(mfcol = c(1, 3))
colorMixtureVenn(m, ellipse = TRUE, mixingMethod = 'estimate')
title('Estimate', line = -1)
colorMixtureVenn(m, ellipse = TRUE, mixingMethod = 'reference')
title('Reference', line = -1)
colorMixtureVenn(m, ellipse = TRUE, mixingMethod = 'exact')
title('Exact', line = -1)


m <- c('5Y 8/10', '5B 4/10')

par(mfcol = c(1,3), cex = 1.5)
colorMixtureVenn(m, mixingMethod = 'estimate')
title('Weighted Mean CIELAB', line = -3, cex.main = 1)

colorMixtureVenn(m, mixingMethod = 'reference')
title('Reference Spectra', line = -3, cex.main = 1)

colorMixtureVenn(m, mixingMethod = 'exact')
title('Mixed Spectra', line = -3, cex.main = 1)


m <- c('5Y 8/10', '5B 4/10', '5R 4/8')

par(mfcol = c(1,3), cex = 1.5)
colorMixtureVenn(m, mixingMethod = 'estimate')
title('Weighted Mean CIELAB', line = -2, cex.main = 1)

colorMixtureVenn(m, mixingMethod = 'reference')
title('Reference Spectra', line = -2, cex.main = 1)

colorMixtureVenn(m, mixingMethod = 'exact')
title('Mixed Spectra', line = -2, cex.main = 1)


