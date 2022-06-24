library(soilDB)
library(aqp)
library(sharpshootR)
library(SoilTaxonomy)

s <- c('fresno', 'pentz', 'peters', 'amador', 'hanford', 'cecil', 'san joaquin', 'sierra', 'musick', 'capay', 'leon', 'BADGERWASH', 'BROWNE', 'pierre', 'AMBOYCRATER')

x <- fetchOSD(s)


# classic results from this function, back 7yrs or so
SoilTaxonomyDendrogram(x, width = 0.3, name.style = 'center-center', KST.order = FALSE)

# just a demonstration
SoilTaxonomyDendrogram(x, width = 0.3, name.style = 'center-center', KST.order = FALSE, rotationOrder = toupper(rev(s)))


## TODO: convert SoilWeb/SDE to use this format

# this is typically what most people will want
SoilTaxonomyDendrogram(x, width = 0.3, name.style = 'center-center', KST.order = TRUE)


# not useful, but interesting demonstration of what is possible
# rotation according to drainage class / hillslope position would be interesting
SoilTaxonomyDendrogram(x, width = 0.3, name.style = 'center-center', KST.order = TRUE, rotationOrder = toupper(rev(s)))



## test fall-back in case of obsolete taxa

x <- fetchOSD(c('inks', 'lucy', 'pardee'))

x$subgroup[1] <- 'ruptic lithic argixerolls'

SoilTaxonomyDendrogram(x, width = 0.3, name.style = 'center-center', KST.order = TRUE, id.style = 'top')

SoilTaxonomyDendrogram(x, width = 0.3, name.style = 'center-center', KST.order = FALSE)



