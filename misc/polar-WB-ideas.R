library(soilDB)
library(ggplot2)
library(sharpshootR)

# add an extra record at position 0 after each December
.formatForPolarCoords <- function(climate.monthly, column = 'month') {
  
  # convert factor -> numeric
  climate.monthly[[column]] <- as.numeric(climate.monthly[[column]])
  
  # last values
  .last <- climate.monthly[climate.monthly[[column]] == 12, ]
  
  # set month code to 0
  .last[[column]] <- 0
  
  # combine with original
  climate.monthly <- rbind(
    climate.monthly,
    .last
  )
  
  return(climate.monthly)
}


s <- c('zook', 'pierre', 'lucy', 'redding')
x <- fetchOSD(s, extended = TRUE)


z <- .formatForPolarCoords(x$climate.monthly)


cols <- RColorBrewer::brewer.pal(9, 'Set1') 
cols <- cols[c(1:5,7,9)]

m <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


ggplot(z, aes(x = month, y = q50, group = variable, color = variable)) +  
  geom_line(show.legend = FALSE, lty = 2, alpha = 1) + 
  expand_limits(y = 0) + 
  geom_ribbon(aes(ymin = q05, ymax = q95, fill = variable), alpha = 0.25) +
  # coord_polar(theta="x", start = - pi/12 , direction = 1) +
  scale_x_continuous(breaks = 1:12, labels = m) + 
  scale_y_continuous(n.breaks = 10) + 
  scale_fill_manual(values=alpha(cols, 0.25)) +
  scale_color_manual(values=alpha(cols, 0.25)) +
  xlab('') + ylab('') + 
  ggtitle("Monthly Climate Summaries (PRISM 1981-2010)\n5th-50th-95th Percentiles") +
  theme_bw() + 
  theme(plot.title = element_text(size = 10)) +
  theme(legend.position="bottom", legend.title = element_blank()) + 
  facet_wrap("series")


ggplot(z, aes(x = month, y = q50, group = variable, color = variable)) +  
  geom_line(show.legend = FALSE, lty = 2, alpha = 1) + 
  expand_limits(y = 0) + 
  geom_ribbon(aes(ymin = q05, ymax = q95, fill = variable), alpha = 0.25) +
  coord_polar(theta="x", start = - pi/12 , direction = 1) +
  scale_x_continuous(breaks = 1:12, labels = m) + 
  scale_y_continuous(n.breaks = 10) + 
  scale_fill_manual(values=alpha(cols, 0.25)) +
  scale_color_manual(values=alpha(cols, 0.25)) +
  xlab('') + ylab('') + 
  ggtitle("Monthly Climate Summaries (PRISM 1981-2010)\n5th-50th-95th Percentiles") +
  theme_bw() + 
  theme(plot.title = element_text(size = 10)) +
  theme(legend.position="bottom", legend.title = element_blank()) + 
  facet_wrap("series")

##

ggplot(z, aes(x = month, y = q50, group = series, color = series)) +  
  geom_line(show.legend = FALSE, lty = 2, alpha = 1) + 
  expand_limits(y = 0) + 
  geom_ribbon(aes(ymin = q05, ymax = q95, fill = series), alpha = 0.25) +
  # coord_polar(theta="x", start = - pi/12 , direction = 1) +
  scale_x_continuous(breaks = 1:12, labels = m) + 
  scale_y_continuous(n.breaks = 10) + 
  scale_fill_manual(values=alpha(cols, 0.25)) +
  scale_color_manual(values=alpha(cols, 0.25)) +
  xlab('') + ylab('') + 
  ggtitle("Monthly Climate Summaries (PRISM 1981-2010)\n5th-50th-95th Percentiles") +
  theme_bw() + 
  theme(plot.title = element_text(size = 10)) +
  theme(legend.position="bottom", legend.title = element_blank()) +
  facet_wrap("variable")



ggplot(z, aes(x = month, y = q50, group = series, color = series)) +  
  geom_line(show.legend = FALSE, lty = 2, alpha = 1) + 
  expand_limits(y = 0) + 
  geom_ribbon(aes(ymin = q05, ymax = q95, fill = series), alpha = 0.25) +
  coord_polar(theta="x", start = - pi/12 , direction = 1) +
  scale_x_continuous(breaks = 1:12, labels = m) + 
  scale_y_continuous(n.breaks = 10) + 
  scale_fill_manual(values=alpha(cols, 0.25)) +
  scale_color_manual(values=alpha(cols, 0.25)) +
  xlab('') + ylab('') + 
  ggtitle("Monthly Climate Summaries (PRISM 1981-2010)\n5th-50th-95th Percentiles") +
  theme_bw() + 
  theme(plot.title = element_text(size = 10)) +
  theme(legend.position="bottom", legend.title = element_blank()) +
  facet_wrap("variable")

  

#####

s <- 'loafercreek'
x <- fetchOSD(s, extended = TRUE)

# get representative, profile-total AWC from SSURGO
sql <- sprintf(
  "SELECT chorizon.cokey AS cokey, 
SUM(awc_r * (hzdepb_r - hzdept_r)) AS ws 
FROM 
legend JOIN mapunit ON legend.lkey = mapunit.lkey
JOIN component ON mapunit.mukey = component.mukey
JOIN chorizon ON component.cokey = chorizon.cokey 
WHERE compname = '%s'
AND areasymbol != 'US'
GROUP BY chorizon.cokey;", s
)

# get via SDA
res <- SDA_query(sql)

# median AWC in mm
# over all components correlated to named series 
AWC <- round(median(res$ws, na.rm = TRUE) * 10)


# monthly climate data from series summary
PPT <- x$climate.monthly$q50[x$climate.monthly$variable == 'Precipitation (mm)']
PET <- x$climate.monthly$q50[x$climate.monthly$variable == 'Potential ET (mm)']

# 3 warm-up cycles
# keep last iteration
# calendar year
x.wb <- monthlyWB(AWC, PPT, PET, S_init = 0, starting_month = 1, rep = 3, keep_last = TRUE, distribute = TRUE)

# tighter margins
par(mar=c(4,4,3,1), bg = 'white')

plotWB(x.wb)
title(sprintf('Monthly Water Balance: %s Series', toupper(s)), line = 2)

plotWB_lines(x.wb)
title(sprintf('Monthly Water Balance: %s Series', toupper(s)), line = 2)




z <- .formatForPolarCoords(x.wb, column = 'month')

z <- reshape2::melt(z, id.vars = c('month'), measure.vars = c('PPT', 'PET', 'ET', 'D'))


ggplot(z, aes(x = month, y = value, group = variable, color = variable)) +  
  geom_line(show.legend = TRUE, lwd = 1.5) + 
  geom_hline(yintercept = 0, colour = "black", lwd = 1, lty = 2) + 
  coord_polar(theta="x", start = - pi/12 , direction = 1) +
  scale_x_continuous(breaks = 1:12, labels = m) +
  scale_y_continuous(n.breaks = 10) + 
  scale_color_manual(values = cols[c(4, 2, 3, 1)]) +
  xlab('') + ylab('') + 
  ggtitle("Monthly Climate Summaries (PRISM 1981-2010)") +
  theme_bw() + 
  theme(plot.title = element_text(size = 12))





###


s <- 'Pierre'
x <- fetchOSD(s, extended = TRUE)

# get representative, profile-total AWC from SSURGO
sql <- sprintf(
  "
SELECT chorizon.cokey AS cokey, 
SUM(awc_r * (hzdepb_r - hzdept_r)) AS ws 
FROM 
legend JOIN mapunit ON legend.lkey = mapunit.lkey
JOIN component ON mapunit.mukey = component.mukey
JOIN chorizon ON component.cokey = chorizon.cokey 
WHERE compname = '%s'
AND areasymbol != 'US'
GROUP BY chorizon.cokey;", s
)

# get via SDA
res <- SDA_query(sql)

# median AWC in mm
# over all components correlated to named series 
AWC <- round(quantile(res$ws, probs = c(0.25, 0.5, 0.75), na.rm = TRUE) * 10)


# monthly climate data from series summary
PPT <- x$climate.monthly[x$climate.monthly$variable == 'Precipitation (mm)', c('q25', 'q50', 'q75')]
PET <- x$climate.monthly[x$climate.monthly$variable == 'Potential ET (mm)', c('q25', 'q50', 'q75')]

# number of simulations
n <- 100

## assumption: draw from normal distribution (ha!)
##             mean: Q50
##             sd: IQR / 2
##
## way simplistic, but a start


# simulations 1:n
awc.sim <- rnorm(n = n, mean = AWC[2], sd = (AWC[3] - AWC[1]) / 2)

# list of 12 elements, month-order, simulations 1:n
ppt.sim <- apply(PPT, 1, simplify = FALSE, function(i) {
  rnorm(n = n, mean = i[2], sd = (i[3] - i[1]) / 2)
})

# list of 12 elements, month-order, simulations 1:n
pet.sim <- apply(PET, 1, simplify = FALSE, function(i) {
  rnorm(n = n, mean = i[2], sd = (i[3] - i[1]) / 2)
})


# quickly eval simulations
boxplot(list(source = res$ws * 10, sim = awc.sim))
boxplot(list(source = unlist(PPT[1, ]), sim = ppt.sim[[1]]))
boxplot(list(source = unlist(PET[1, ]), sim = pet.sim[[1]]))

# extract simulations and perform water balance
z <- lapply(1:n, function(i) {
  
  # select simulated value i
  .awc <- awc.sim[i]
  # select simulated value i, from each month
  .ppt <- sapply(ppt.sim, '[', i)
  # select simulated value i, from each month
  .pet <- sapply(pet.sim, '[', i)
  
  # 3 warm-up cycles
  .wb <- monthlyWB(.awc, .ppt, .pet, S_init = 0, starting_month = 1, rep = 3, keep_last = TRUE, distribute = TRUE)
  
  return(.wb)
})
  
# flatten simulation parameters to pctiles
.reduceParam <- function(param, id, prefix = 'q', p = c(0.05, 0.5, 0.95)) {
  df <- t(apply(param, 1, quantile, probs = p))
  df <- data.frame(df)
  names(df) <- sprintf("%s%s", prefix, as.character(p * 100))
  df[['variable']] <- id
  df[['month']] <- 1:12
  return(df)
}

# extract simulation data
PET <- sapply(z, '[[', 'PET')
D <- sapply(z, '[[', 'D')
S <- sapply(z, '[[', 'S')
U <- sapply(z, '[[', 'U')
ET <- sapply(z, '[[', 'ET')

# flatten to qtiles -> long format for plotting
z.qtiles <- rbind(
  .reduceParam(D, id = 'deficit'),
  .reduceParam(S, id = 'storage'),
  .reduceParam(U, id = 'surplus'),
  .reduceParam(ET, id = 'AET')
)

# add extra records for coord_polar()
z.qtiles <- .formatForPolarCoords(z.qtiles, column = 'month')




title.txt <- sprintf("Simple Water Balance %s Series\nSeries Extent (PRISM 1981-2010)\n5th-50th-95th Percentiles (%s Simulations)", toupper(s), n)


ggplot(z.qtiles, aes(x = month, y = q50, group = variable, color = variable)) +  
  geom_line(show.legend = FALSE, lty = 2, alpha = 1) + 
  expand_limits(y = min(z.qtiles$q5)) +
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = variable), alpha = 0.25) +
  # coord_polar(theta="x", start = - pi/12 , direction = 1) +
  scale_x_continuous(breaks = 1:12, labels = m) +
  scale_y_continuous(n.breaks = 10) + 
  scale_fill_manual(values=alpha(cols[c(4, 1, 3, 2)], 0.25)) +
  scale_color_manual(values=alpha(cols[c(4, 1, 3, 2)], 0.25)) +
  xlab('mm of water') + ylab('mm of water') +
  ggtitle(title.txt) +
  theme_bw() + 
  theme(plot.title = element_text(size = 10)) +
  theme(legend.position="bottom", legend.title = element_blank())

ggplot(z.qtiles, aes(x = month, y = q50, group = variable, color = variable)) +  
  geom_line(show.legend = FALSE, lty = 2, alpha = 1) + 
  expand_limits(y = min(z.qtiles$q5)) +
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = variable), alpha = 0.25) +
  coord_polar(theta="x", start = - pi/12 , direction = 1) +
  scale_x_continuous(breaks = 1:12, labels = m) +
  scale_y_continuous(n.breaks = 10) + 
  scale_fill_manual(values=alpha(cols[c(4, 1, 3, 2)], 0.25)) +
  scale_color_manual(values=alpha(cols[c(4, 1, 3, 2)], 0.25)) +
  xlab('') + ylab('') + 
  ggtitle(title.txt) +
  theme_bw() + 
  theme(plot.title = element_text(size = 10)) +
  theme(legend.position="bottom", legend.title = element_blank())



## what about annual sums?
# deficit
round(quantile(apply(D, 2, sum), probs = c(0.05, 0.25, 0.5, 0.75, 0.95)))

# surplus
round(quantile(apply(U, 2, sum), probs = c(0.05, 0.25, 0.5, 0.75, 0.95)))

# AET
round(quantile(apply(ET, 2, sum), probs = c(0.05, 0.25, 0.5, 0.75, 0.95)))

round(quantile(apply(ET, 2, sum) / apply(PET, 2, sum), probs = c(0.05, 0.25, 0.5, 0.75, 0.95)), 3)



## WB summary metrics
z.wb.summary <- do.call('rbind', lapply(z, monthlyWB_summary))

hist(z.wb.summary$annual_AET_PET_ratio)
hist(z.wb.summary$total_deficit)





