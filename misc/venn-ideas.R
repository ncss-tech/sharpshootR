library(aqp)
library(venn)
library(sharpshootR)

venn(7)

x <- as.data.frame(matrix(sample(0:1, 150, replace = TRUE), ncol = 5))
names(x) <- c('DB', 'DSM', 'DSS', 'Initial', 'KSSL')
venn(x)


x <- as.data.frame(matrix(sample(0:1, 1500, replace = TRUE), ncol = 5))
names(x) <- c('Life', 'Work', 'Hobbies', 'Family', 'Sleep')

venn(x, ellipse = TRUE, zcolor = 'style')
venn(x, ellipse = FALSE, zcolor = 'style')

venn(5, ilabels = TRUE, zcolor = "style", ellipse = TRUE)


# chips <- c('10YR 8/2', '10R 4/8', '5Y 8/8', '5B 4/8', '5GY 4/8', '5Y 3/1', '7.5P 4/8')
# chips <- c('10P 4/8', '10R 4/8', '5Y 8/8', '5B 4/8', '5GY 4/8', '5Y 3/1')
# chips <- c('10YR 8/2', '10R 4/8', '5Y 8/8', '5B 4/8', '5GY 4/8')
chips <- c('10YR 8/2', '10R 4/8', '5Y 8/8', '5B 4/8')
# chips <- c('2.5R 4/6', '2.5B 4/6', '2.5Y 8/2')
# chips <- c('10R 6/8', '5GY 4/4')

# # reference diagram
# venn(4, zcolor = 'bw', box = FALSE, ellipse = TRUE, ilabels = TRUE)


colorMixtureVenn(chips)
colorMixtureVenn(chips, labels = FALSE)

colorMixtureVenn(chips, ellipse = TRUE)

par(bg = 'black', fg = 'white')
colorMixtureVenn(chips, ellipse = TRUE)


chips <- c('10YR 8/1', '2.5YR 3/6', '10YR 2/2')
colorMixtureVenn(chips)

chips <- c('10YR 8/1', '2.5YR 3/6', '10YR 2/2', '5P 3/3')
colorMixtureVenn(chips, ellipse = TRUE)


library(soilDB)
x <- fetchOSD('musick')

chips <- unique(sprintf("%s %s/%s", x$hue, x$value, x$chroma))

colorMixtureVenn(chips[1:5])

