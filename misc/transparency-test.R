library(aqp)
library(sharpshootR)
library(venn)
library(scales)

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
