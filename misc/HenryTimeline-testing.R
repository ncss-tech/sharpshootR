devtools::load_all()
library(soilDB)
library(latticeExtra)


# get soil temperature, soil moisture, and air temperature data
x <- fetchHenry(project = 'CA790', gran = 'day', soiltemp.summaries = FALSE, pad.missing.days = TRUE)



HenryTimeLine(x$soiltemp, main='Soil Temperature Records', col='RoyalBlue', border = NA)

HenryTimeLine(x$soilVWC, main='Soil Temperature Records', col='RoyalBlue', border = NA)





## TODO: fix soiltemp.summaries logic when gran != 'day'

w <- fetchHenry(project = 'CA630', gran = 'week', soiltemp.summaries = FALSE)
x <- fetchHenry(project = 'CA630', gran = 'day', soiltemp.summaries = FALSE, pad.missing.days = TRUE)

HenryTimeLine(w$soiltemp, main='Soil Temperature Records', col='RoyalBlue', border = NA)

HenryTimeLine(x$soiltemp, main='Soil Temperature Records', col='RoyalBlue', border = NA)
